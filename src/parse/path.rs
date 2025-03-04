use crate::{
    ast,
    diagnostic::Diagnostic,
    parse::{ident, Token},
};

use super::{spec, Tokens};

pub fn path(input: &mut Tokens) -> Result<ast::Path, Diagnostic> {
    let mut segments = Vec::new();

    loop {
        let (name, span) = ident(input)?;

        segments.push(ast::PathSegment { name, span });

        if input.is(Token::ColonColon) {
            input.consume();
        } else {
            break;
        }
    }

    let (Some(first), Some(last)) = (segments.first(), segments.last()) else {
        panic!("segments.first() and segments.last() should always return Some");
    };

    let mut span = first.span.join(last.span);

    let spec = if input.is(Token::Lt) && !input.has_whitespace() {
        let spec = spec(input)?;
        span = span.join(spec.span);
        Some(spec)
    } else {
        None
    };

    Ok(ast::Path {
        segments,
        spec,
        span,
    })
}
