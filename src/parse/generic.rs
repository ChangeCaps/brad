use diagnostic::Diagnostic;

use crate::{ast, parse::Token};

use super::{ident, ty, Tokens};

pub fn generic(input: &mut Tokens) -> Result<ast::Generic, Diagnostic> {
    let start = input.expect(Token::Quote)?;
    let (name, end) = ident(input)?;

    Ok(ast::Generic {
        name,
        span: start.join(end),
    })
}

pub fn generics(input: &mut Tokens) -> Result<ast::Generics, Diagnostic> {
    let mut generics = Vec::new();

    let start = input.expect(Token::Lt)?;

    while !input.is(Token::Gt) {
        generics.push(generic(input)?);

        if input.is(Token::Gt) {
            break;
        }

        input.expect(Token::Comma)?;
    }

    let end = input.expect(Token::Gt)?;

    Ok(ast::Generics {
        params: generics,
        span: start.join(end),
    })
}

pub fn spec(input: &mut Tokens) -> Result<ast::Spec, Diagnostic> {
    let mut tys = Vec::new();

    let start = input.expect(Token::Lt)?;

    while !input.is(Token::Gt) {
        tys.push(ty(input)?);

        if input.is(Token::Gt) {
            break;
        }

        input.expect(Token::Comma)?;
    }

    let end = input.expect(Token::Gt)?;

    Ok(ast::Spec {
        tys,
        span: start.join(end),
    })
}
