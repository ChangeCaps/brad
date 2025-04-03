use super::{generic, path, Delim, Token, Tokens};
use crate::ast::Spanned;
use crate::{
    ast,
    diagnostic::Diagnostic,
    parse::{consume_newlines, ident},
};

pub fn ty(input: &mut Tokens) -> Result<ast::Ty, Diagnostic> {
    func(input)
}

fn func(input: &mut Tokens) -> Result<ast::Ty, Diagnostic> {
    let ty = union(input)?;

    if !input.is(Token::ThinArrow) {
        return Ok(ty);
    }

    let (_, span) = input.consume();

    let output = func(input)?;
    let span = ty.span().join(output.span()).join(span);

    Ok(ast::Ty::Func {
        input: Box::new(ty),
        output: Box::new(output),
        span,
    })
}

fn union(input: &mut Tokens) -> Result<ast::Ty, Diagnostic> {
    let first = tuple(input)?;

    if !is_union(input) {
        return Ok(first);
    }

    let mut span = first.span();
    let mut tys = vec![first];

    while is_union(input) {
        consume_newlines(input);
        input.consume();

        let term = tuple(input)?;
        span = span.join(term.span());
        tys.push(term);
    }

    Ok(ast::Ty::Union { tys, span })
}

fn is_union(input: &Tokens) -> bool {
    let mut offset = 0;

    while input.nth_is(offset, Token::Newline) {
        offset += 1;
    }

    input.nth_is(offset, Token::Pipe)
}

fn tuple(input: &mut Tokens) -> Result<ast::Ty, Diagnostic> {
    let first = term(input)?;

    if !input.is(Token::Star) {
        return Ok(first);
    }

    let mut span = first.span();
    let mut tys = vec![first];

    while input.is(Token::Star) {
        input.consume();

        let term = term(input)?;
        span = span.join(term.span());
        tys.push(term);
    }

    Ok(ast::Ty::Tuple { tys, span })
}

fn term(input: &mut Tokens) -> Result<ast::Ty, Diagnostic> {
    let (token, span) = input.peek();

    match token {
        Token::Under => {
            let (_, span) = input.consume();
            Ok(ast::Ty::Wild(span))
        }

        Token::Int => {
            let (_, span) = input.consume();
            Ok(ast::Ty::Int(span))
        }

        Token::Float => {
            let (_, span) = input.consume();
            Ok(ast::Ty::Float(span))
        }

        Token::Str => {
            let (_, span) = input.consume();
            Ok(ast::Ty::Str(span))
        }

        Token::True => {
            let (_, span) = input.consume();
            Ok(ast::Ty::True(span))
        }

        Token::False => {
            let (_, span) = input.consume();
            Ok(ast::Ty::False(span))
        }

        Token::None => {
            let (_, span) = input.consume();
            Ok(ast::Ty::None(span))
        }

        Token::Bang => {
            let (_, span) = input.consume();
            Ok(ast::Ty::Never(span))
        }

        Token::Quote => {
            let generic = generic(input)?;
            Ok(ast::Ty::Generic(generic))
        }

        Token::Ref => {
            let (_, span) = input.consume();
            let ty = term(input)?;

            let span = span.join(ty.span());

            Ok(ast::Ty::Ref {
                ty: Box::new(ty),
                span,
            })
        }

        Token::Ident(_) => {
            let path = path(input)?;
            Ok(ast::Ty::Path(path))
        }

        Token::Open(Delim::Paren) => {
            input.expect(Token::Open(Delim::Paren))?;

            let ty = ty(input)?;

            input.expect(Token::Close(Delim::Paren))?;

            Ok(ty)
        }

        Token::Open(Delim::Bracket) => {
            input.expect(Token::Open(Delim::Bracket))?;

            let ty = ty(input)?;

            input.expect(Token::Close(Delim::Bracket))?;

            Ok(ast::Ty::List {
                ty: Box::new(ty),
                span,
            })
        }

        Token::Open(Delim::Brace) => record(input),

        _ => {
            let diagnostic = Diagnostic::error("expected::type")
                .message(format!("expected a type, found `{}`", token))
                .span(span);

            Err(diagnostic)
        }
    }
}

fn record(input: &mut Tokens) -> Result<ast::Ty, Diagnostic> {
    let start = input.expect(Token::Open(Delim::Brace))?;

    let multiline = input.is(Token::Newline);
    consume_newlines(input);

    let mut fields = Vec::new();

    while !input.is(Token::Close(Delim::Brace)) {
        fields.push(field(input)?);

        if input.is(Token::Close(Delim::Brace)) {
            break;
        }

        if multiline {
            consume_newlines(input);
        } else {
            input.expect(Token::Semi)?;
        }
    }

    let end = input.expect(Token::Close(Delim::Brace))?;

    Ok(ast::Ty::Record {
        fields,
        span: start.join(end),
    })
}

fn field(input: &mut Tokens) -> Result<ast::Field, Diagnostic> {
    let (name, span) = ident(input)?;

    input.expect(Token::Colon)?;

    let ty = ty(input)?;
    let span = span.join(ty.span());

    Ok(ast::Field { name, ty, span })
}
