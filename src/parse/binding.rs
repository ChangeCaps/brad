use crate::{ast, diagnostic::Diagnostic};

use super::{ident, Token, Tokens};

pub fn binding(input: &mut Tokens) -> Result<ast::Binding, Diagnostic> {
    let first = term(input)?;

    if !input.is(Token::Comma) {
        return Ok(first);
    }

    let mut span = first.span();
    let mut bindings = vec![first];

    while input.is(Token::Comma) {
        input.consume();

        let binding = term(input)?;
        span = span.join(binding.span());
        bindings.push(binding);
    }

    Ok(ast::Binding::Tuple { bindings, span })
}

fn term(input: &mut Tokens) -> Result<ast::Binding, Diagnostic> {
    let (token, span) = input.peek();

    match token {
        Token::Under => {
            input.consume();
            Ok(ast::Binding::Wild { span })
        }

        Token::Ident(name) => {
            input.consume();
            Ok(ast::Binding::Bind {
                mutable: false,
                name,
                span,
            })
        }

        Token::Mut => {
            input.consume();
            let (name, name_span) = ident(input)?;

            Ok(ast::Binding::Bind {
                mutable: true,
                name,
                span: span.join(name_span),
            })
        }

        _ => {
            let diagnostic = Diagnostic::error("expected::binding")
                .message(format!("expected a binding, found {}", token))
                .span(span);

            Err(diagnostic)
        }
    }
}
