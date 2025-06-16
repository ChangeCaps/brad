use std::borrow::Cow;

use diagnostic::Diagnostic;

use crate::{
    ast::{self, Name},
    attribute::{Attribute, Attributes},
    parse::{block, path},
};

use super::{binding, consume_newlines, expr, generics, ident, ty, Delim, Token, Tokens};

pub fn decl(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
    let attrs = attributes(input, false)?;

    let (token, span) = input.peek();

    match token {
        Token::Fn => funcy(input, false, attrs),
        Token::Type => r#type(input, false, attrs),

        Token::Extern => {
            input.consume();
            let (token, _) = input.peek();

            match token {
                Token::Fn => funcy(input, true, attrs),
                Token::Type => r#type(input, true, attrs),

                _ => {
                    let diagnostic = Diagnostic::error("expected::declaration")
                        .message(format!("expected `fn` or `type`, found `{}`", token))
                        .span(span);

                    Err(diagnostic)
                }
            }
        }

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

fn funcy(input: &mut Tokens, is_extern: bool, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Fn)?;

    let name = name(input)?;

    let generics = match input.is(Token::Lt) {
        true => Some(generics(input)?),
        false => None,
    };

    let mut args = Vec::new();

    while !input.is(Token::Open(Delim::Brace))
        && !input.is(Token::ThinArrow)
        && !input.is(Token::ThickArrow)
    {
        args.push(argument(input)?);
    }

    let output = if input.take(Token::ThinArrow) {
        Some(ty(input)?)
    } else {
        None
    };

    let (token, _) = input.peek();

    let body = match token {
        Token::ThickArrow => {
            input.consume();
            Some(expr(input)?)
        }

        Token::Open(Delim::Brace) => Some(block(input)?),

        _ => None,
    };

    let span = name.span;

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

fn r#type(input: &mut Tokens, is_extern: bool, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Type)?;

    let name = name(input)?;

    let generics = if input.is(Token::Lt) {
        Some(generics(input)?)
    } else {
        None
    };

    let ty = if !is_extern && input.take(Token::Eq) {
        Some(ty(input)?)
    } else {
        None
    };

    let span = name.span;

    Ok(ast::Decl::Type(ast::Type {
        attrs,
        is_extern,
        name,
        generics,
        ty,
        span,
    }))
}

fn alias(input: &mut Tokens, attrs: Attributes) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Alias)?;

    let name = name(input)?;

    let generics = if input.is(Token::Lt) {
        Some(generics(input)?)
    } else {
        None
    };

    input.expect(Token::Eq)?;

    let ty = ty(input)?;

    let span = name.span;

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

pub(super) fn attributes(input: &mut Tokens, top_level: bool) -> Result<Attributes, Diagnostic> {
    let mut attributes = Vec::new();

    loop {
        if !input.is(Token::Pound) {
            break;
        }

        if top_level && !input.nth_is(1, Token::Bang) {
            break;
        }

        attributes.push(attribute(input, top_level)?);
        consume_newlines(input);
    }

    Ok(Attributes { attributes })
}

fn attribute(input: &mut Tokens, top_level: bool) -> Result<Attribute, Diagnostic> {
    input.expect(Token::Pound)?;

    if top_level {
        input.expect(Token::Bang)?;
    }

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
        span: Some(span),
    })
}

fn name(input: &mut Tokens) -> Result<Name, Diagnostic> {
    let (first, span) = ident(input)?;

    let mut segments = vec![first];

    while input.take(Token::ColonColon) {
        let (segment, _) = ident(input)?;

        segments.push(segment);
    }

    Ok(Name { segments, span })
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
