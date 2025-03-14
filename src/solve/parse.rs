use std::collections::HashMap;

use crate::{
    diagnostic::Diagnostic,
    parse::{self, Delim, Token, Tokens},
};

use super::Ty;

pub fn ty(input: &mut Tokens) -> Result<Ty, Diagnostic> {
    func(input)
}

fn func(input: &mut Tokens) -> Result<Ty, Diagnostic> {
    let lhs = inter(input)?;

    if input.is(Token::ThinArrow) {
        input.consume();

        let rhs = func(input)?;

        Ok(Ty::Func(Box::new(lhs), Box::new(rhs)))
    } else {
        Ok(lhs)
    }
}

fn inter(input: &mut Tokens) -> Result<Ty, Diagnostic> {
    let lhs = union(input)?;

    if input.is(Token::Amp) {
        input.consume();

        let rhs = inter(input)?;

        Ok(Ty::Inter(Box::new(lhs), Box::new(rhs)))
    } else {
        Ok(lhs)
    }
}

fn union(input: &mut Tokens) -> Result<Ty, Diagnostic> {
    let lhs = term(input)?;

    if input.is(Token::Pipe) {
        input.consume();

        let rhs = union(input)?;

        Ok(Ty::Union(Box::new(lhs), Box::new(rhs)))
    } else {
        Ok(lhs)
    }
}

fn term(input: &mut Tokens) -> Result<Ty, Diagnostic> {
    let (token, span) = input.peek();

    match token {
        Token::Ident(ident) => {
            input.consume();

            Ok(Ty::Name(ident))
        }

        Token::Quote => {
            input.consume();

            let (ident, _) = parse::ident(input)?;

            Ok(Ty::Var(ident))
        }

        Token::Tilde => {
            input.consume();

            let ty = term(input)?;

            Ok(Ty::Neg(Box::new(ty)))
        }

        Token::Open(Delim::Brace) => {
            input.consume();

            let mut fields = HashMap::new();

            while !input.is(Token::Close(Delim::Brace)) {
                let (ident, _) = parse::ident(input)?;

                input.expect(Token::Colon)?;

                fields.insert(ident, ty(input)?);

                if !input.take(Token::Semi) {
                    break;
                }
            }

            input.expect(Token::Close(Delim::Brace))?;

            Ok(Ty::Record(fields))
        }

        Token::Open(Delim::Paren) => {
            input.consume();

            let ty = func(input)?;

            input.expect(Token::Close(Delim::Paren))?;

            Ok(ty)
        }

        _ => {
            let dianostic = Diagnostic::error("expected::type")
                .message(format!("expected type, found {:?}", token))
                .span(span);

            Err(dianostic)
        }
    }
}
