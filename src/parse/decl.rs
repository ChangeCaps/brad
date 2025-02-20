use crate::{
    ast,
    diagnostic::Diagnostic,
    parse::{block, path},
};

use super::{binding, generics, ident, ty, Delim, Token, Tokens};

pub fn decl(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
    let (token, span) = input.peek();

    match token {
        Token::Fn => funcy(input),
        Token::Type => type_(input),
        Token::Alias => alias(input),
        Token::Import => import(input),
        _ => {
            let diagnostic = Diagnostic::error("expected::declaration")
                .message(format!("expected declaration, found {}", token))
                .span(span);

            Err(diagnostic)
        }
    }
}

fn funcy(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Fn)?;

    let (name, span) = ident(input)?;

    let mut args = Vec::new();

    while !input.is(Token::Open(Delim::Brace)) && !input.is(Token::ThinArrow) {
        args.push(argument(input)?);
    }

    let output = if input.take(Token::ThinArrow) {
        Some(ty(input)?)
    } else {
        None
    };

    let body = block(input)?;

    Ok(ast::Decl::Func(ast::Func {
        name,
        args,
        output,
        body,
        span,
    }))
}

fn argument(input: &mut Tokens) -> Result<ast::Argument, Diagnostic> {
    // x
    // (x: t)

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
    input.expect(Token::Colon)?;
    let ty = ty(input)?;

    let end = input.expect(Token::Close(Delim::Paren))?;

    Ok(ast::Argument {
        binding,
        ty: Some(ty),
        span: start.join(end),
    })
}

fn type_(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
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
        name,
        generics,
        ty,
        span,
    }))
}

fn alias(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
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
        name,
        generics,
        ty,
        span,
    }))
}

fn import(input: &mut Tokens) -> Result<ast::Decl, Diagnostic> {
    input.expect(Token::Import)?;

    let path = path(input)?;

    Ok(ast::Decl::Import(ast::Import { path }))
}
