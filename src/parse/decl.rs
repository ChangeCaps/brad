use std::borrow::Cow;

use super::{binding, consume_newlines, generics, ident, ty, Delim, Token, Tokens};
use crate::{
    ast,
    attribute::{Attribute, Attributes},
    diagnostic::Diagnostic,
    parse::{block, path},
};

pub fn decl(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
    let attrs = attributes(input)?;

    let (token, span) = input.peek();

    match token {
        Token::Fn | Token::Extern => funcy(input, attrs),
        Token::Type => r#type(input, attrs),
        Token::Alias => alias(input, attrs),
        Token::Import => import(input, attrs),
        _ => {
            let diagnostic = Diagnostic::error("expected::declaration")
                .message(format!("expected declaration, found `{}`", token))
                .span(span);

            Err(diagnostic)
        }
    }
}

fn funcy(input: &mut Tokens, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    let is_extern = input.take(Token::Extern);

    input.expect(Token::Fn)?;

    let (name, span) = ident(input)?;

    let generics = match input.is(Token::Lt) {
        true => Some(generics(input)?),
        false => None,
    };

    let mut args = Vec::new();

    while !input.is(Token::Open(Delim::Brace)) && !input.is(Token::ThinArrow) {
        args.push(argument(input)?);
    }

    let output = if input.take(Token::ThinArrow) {
        Some(ty(input)?)
    } else {
        None
    };

    let body = match input.is(Token::Open(Delim::Brace)) {
        true => Some(block(input)?),
        false => None,
    };

    Ok(ast::Decl::Func(ast::Func {
        attrs,
        is_extern,
        name,
        generics,
        args,
        output,
        body,
        span,
    }))
}

fn argument(input: &mut Tokens) -> Result<ast::Argument, Diagnostic> {
    if !input.is(Token::Open(Delim::Paren)) {
        let binding = binding(input)?;
        let span = binding.span();
        return Ok(ast::Argument {
            binding,
            ty: None,
            span,
        });
    }

    let (_, start) = input.consume();

    let binding = binding(input)?;

    let ty = if input.take(Token::Colon) {
        Some(ty(input)?)
    } else {
        None
    };

    let end = input.expect(Token::Close(Delim::Paren))?;
    let span = start.join(end);

    Ok(ast::Argument { binding, ty, span })
}

fn r#type(input: &mut Tokens, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Type)?;

    let (name, span) = ident(input)?;

    let generics = if input.is(Token::Lt) {
        Some(generics(input)?)
    } else {
        None
    };

    let ty = if input.take(Token::Eq) {
        Some(ty(input)?)
    } else {
        None
    };

    Ok(ast::Decl::Type(ast::Type {
        attrs,
        name,
        generics,
        ty,
        span,
    }))
}

fn alias(input: &mut Tokens, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Alias)?;

    let (name, span) = ident(input)?;

    let generics = if input.is(Token::Lt) {
        Some(generics(input)?)
    } else {
        None
    };

    input.expect(Token::Eq)?;

    let ty = ty(input)?;

    Ok(ast::Decl::Alias(ast::Alias {
        attrs,
        name,
        generics,
        ty,
        span,
    }))
}

fn import(input: &mut Tokens, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Import)?;

    let path = path(input)?;

    Ok(ast::Decl::Import(ast::Import { attrs, path }))
}

fn attributes(input: &mut Tokens) -> Result<Attributes, Diagnostic> {
    let mut attributes = Vec::new();

    while input.is(Token::Pound) {
        attributes.push(attribute(input)?);
        consume_newlines(input);
    }

    Ok(Attributes { attributes })
}

fn attribute(input: &mut Tokens) -> Result<Attribute, Diagnostic> {
    input.expect(Token::Pound)?;
    input.expect(Token::Open(Delim::Bracket))?;

    let (name, span) = ident(input)?;

    let value = match input.take(Token::Eq) {
        true => Some(string(input)?),
        false => None,
    };

    input.expect(Token::Close(Delim::Bracket))?;

    Ok(Attribute {
        name: Cow::Borrowed(name),
        value,
        span,
    })
}

fn string(input: &mut Tokens) -> Result<Cow<'static, str>, Diagnostic> {
    let (token, span) = input.consume();

    match token {
        Token::String(value) => Ok(Cow::Borrowed(value)),
        _ => {
            let diagnostic = Diagnostic::error("expected::string")
                .message(format!("expected string, found `{}`", token))
                .span(span);

            Err(diagnostic)
        }
    }
}
