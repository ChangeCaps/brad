mod binding;
mod decl;
mod expr;
mod generic;
mod interner;
mod path;
mod stream;
mod token;
mod tokenize;
mod ty;

pub use binding::*;
pub use decl::*;
pub use expr::*;
pub use generic::*;
pub use interner::*;
pub use path::*;
pub use stream::*;
pub use token::*;
pub use ty::*;

use crate::{
    ast,
    diagnostic::{Diagnostic, Span},
};

pub fn module(input: &mut Tokens) -> Result<ast::Module, Diagnostic> {
    let attrs = attributes(input, true)?;
    let mut decls = Vec::new();

    while !input.is(Token::Eof) {
        if !decls.is_empty() {
            input.expect(Token::Newline)?;
        }

        consume_newlines(input);

        if input.is(Token::Eof) {
            break;
        }

        decls.push(decl(input)?);
    }

    Ok(ast::Module { attrs, decls })
}

pub fn ident(input: &mut Tokens) -> Result<(&'static str, Span), Diagnostic> {
    let (token, span) = input.consume();

    match token {
        Token::Ident(ident) => Ok((ident, span)),
        _ => {
            let diagnostic = Diagnostic::error("expected::identifier")
                .message(format!("expected identifier, found `{}`", token))
                .span(span);

            Err(diagnostic)
        }
    }
}

pub fn consume_newlines(input: &mut Tokens) {
    while input.is(Token::Newline) {
        input.consume();
    }
}
