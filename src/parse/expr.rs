use crate::{ast, diagnostic::Diagnostic};

use super::{binding, consume_newlines, ident, path, ty, Delim, Token, Tokens};

pub fn expr(input: &mut Tokens, can_call: bool) -> Result<ast::Expr, Diagnostic> {
    let (token, _) = input.peek();

    match token {
        Token::Let => let_(input),
        Token::Ref => ref_(input),
        Token::Loop => loop_(input),
        Token::Match => match_(input),
        Token::Break => break_(input),
        _ => tuple(input, can_call),
    }
}

fn let_(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Let)?;

    let binding = binding(input)?;

    let ty = match input.take(Token::Colon) {
        true => Some(ty(input)?),
        false => None,
    };

    input.expect(Token::Eq)?;

    let value = Box::new(expr(input, true)?);

    let span = start.join(value.span());

    Ok(ast::Expr::Let(ast::LetExpr {
        binding,
        ty,
        value,
        span,
    }))
}

fn ref_(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Ref)?;

    let expr = Box::new(expr(input, true)?);

    let span = start.join(expr.span());

    Ok(ast::Expr::Ref(ast::RefExpr { expr, span }))
}

fn loop_(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Loop)?;

    let body = Box::new(expr(input, true)?);

    let span = start.join(body.span());

    Ok(ast::Expr::Loop(ast::LoopExpr { body, span }))
}

fn match_(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let mut span = input.expect(Token::Match)?;

    let target = Box::new(expr(input, true)?);

    let mut arms = Vec::new();

    while is_match_arm(input) {
        consume_newlines(input);

        let arm = match_arm(input)?;
        span = span.join(arm.span);

        arms.push(arm);
    }

    Ok(ast::Expr::Match(ast::MatchExpr { target, arms, span }))
}

fn break_(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Break)?;
    let mut span = start;

    let value = if is_expr(input) {
        let value = expr(input, true)?;
        span = span.join(value.span());

        Some(Box::new(value))
    } else {
        None
    };

    Ok(ast::Expr::Break(ast::BreakExpr { value, span }))
}

fn is_match_arm(input: &Tokens) -> bool {
    let mut offset = 1;

    while input.nth_is(offset, Token::Newline) {
        offset += 1;
    }

    matches!(input.peek_nth(offset), (Token::Pipe, _))
}

fn match_arm(input: &mut Tokens) -> Result<ast::MatchArm, Diagnostic> {
    let start = input.expect(Token::Pipe)?;

    let pattern = pattern(input)?;
    input.expect(Token::ThickArrow)?;

    let expr = expr(input, true)?;

    let span = start.join(expr.span());

    Ok(ast::MatchArm {
        pattern,
        expr,
        span,
    })
}

fn pattern(input: &mut Tokens) -> Result<ast::Pattern, Diagnostic> {
    let ty = ty(input)?;
    let mut span = ty.span();

    let binding = if input.take(Token::As) {
        let binding = binding(input)?;
        span = span.join(binding.span());

        Some(binding)
    } else {
        None
    };

    Ok(ast::Pattern::Ty { ty, binding, span })
}

fn is_expr(input: &mut Tokens) -> bool {
    let (token, _) = input.peek();

    match token {
        Token::Ref
        | Token::Match
        | Token::Let
        | Token::True
        | Token::False
        | Token::None
        | Token::Open(_)
        | Token::Ident(_)
        | Token::Integer(_)
        | Token::Floating(_)
        | Token::String(_) => true,

        Token::Bang | Token::Minus | Token::Tilde | Token::Star => !input.nth_has_whitespace(1),

        _ => false,
    }
}

fn tuple(input: &mut Tokens, can_call: bool) -> Result<ast::Expr, Diagnostic> {
    let first = binary(input, can_call)?;

    if !input.is(Token::Comma) {
        return Ok(first);
    }

    let mut span = first.span();
    let mut items = vec![first];

    while input.take(Token::Comma) {
        let expr = binary(input, can_call)?;
        span = span.join(expr.span());
        items.push(expr);
    }

    Ok(ast::Expr::Tuple(ast::TupleExpr { items, span }))
}

fn binary(input: &mut Tokens, can_call: bool) -> Result<ast::Expr, Diagnostic> {
    let mut lhs = assign(input, can_call)?;

    while let Some(op) = binary_op(input) {
        let rhs = binary(input, can_call)?;
        let span = lhs.span().join(rhs.span());

        match rhs {
            ast::Expr::Binary(rhs) if op.precedence() > rhs.op.precedence() => {
                lhs = ast::Expr::Binary(ast::BinaryExpr {
                    lhs: Box::new(ast::Expr::Binary(ast::BinaryExpr {
                        lhs: Box::new(lhs),
                        rhs: rhs.lhs,
                        op,
                        span,
                    })),
                    rhs: rhs.rhs,
                    op: rhs.op,
                    span: rhs.span,
                });
            }

            _ => {
                lhs = ast::Expr::Binary(ast::BinaryExpr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op,
                    span,
                });
            }
        }
    }

    Ok(lhs)
}

fn binary_op(input: &mut Tokens) -> Option<ast::BinaryOp> {
    let (token, _) = input.peek();

    let op = match token {
        Token::Plus => ast::BinaryOp::Add,
        Token::Minus => ast::BinaryOp::Sub,
        Token::Star => ast::BinaryOp::Mul,
        Token::Slash => ast::BinaryOp::Div,
        Token::Percent => ast::BinaryOp::Mod,
        Token::AmpAmp => ast::BinaryOp::And,
        Token::PipePipe => ast::BinaryOp::Or,
        Token::Amp => ast::BinaryOp::BitAnd,
        Token::Pipe => ast::BinaryOp::BitOr,
        Token::Caret => ast::BinaryOp::BitXor,
        Token::LtLt => ast::BinaryOp::Shl,
        Token::GtGt => ast::BinaryOp::Shr,
        Token::EqEq => ast::BinaryOp::Eq,
        Token::NotEq => ast::BinaryOp::Ne,
        Token::Lt => ast::BinaryOp::Lt,
        Token::LtEq => ast::BinaryOp::Le,
        Token::Gt => ast::BinaryOp::Gt,
        Token::GtEq => ast::BinaryOp::Ge,

        _ => return None,
    };

    input.consume();
    Some(op)
}

fn assign(input: &mut Tokens, can_call: bool) -> Result<ast::Expr, Diagnostic> {
    let target = unary(input, can_call)?;

    if input.take(Token::Eq) {
        let value = expr(input, can_call)?;
        let span = target.span().join(value.span());

        Ok(ast::Expr::Assign(ast::AssignExpr {
            target: Box::new(target),
            value: Box::new(value),
            span,
        }))
    } else {
        Ok(target)
    }
}

/// # BNF
///
/// ```text
/// <unary>  ::= bang  <unary> // !   logical not
///            | minus <unary> // -   numerical negation
///            | tilde <unary> // ~   binary negation
///            | star  <unary> // *   dereference
///            | <access>
/// ```
fn unary(input: &mut Tokens, can_call: bool) -> Result<ast::Expr, Diagnostic> {
    if let Some(op) = unary_op(input) {
        let expr = unary(input, can_call)?;
        let span = expr.span();

        Ok(ast::Expr::Unary(ast::UnaryExpr {
            op,
            expr: Box::new(expr),
            span,
        }))
    } else {
        call(input, can_call)
    }
}

fn unary_op(input: &mut Tokens) -> Option<ast::UnaryOp> {
    if input.nth_has_whitespace(1) {
        return None;
    }

    let (token, _) = input.peek();

    let op = match token {
        Token::Bang => ast::UnaryOp::Not,
        Token::Minus => ast::UnaryOp::Neg,
        Token::Tilde => ast::UnaryOp::BitNot,
        Token::Star => ast::UnaryOp::Deref,

        _ => return None,
    };

    input.consume();
    Some(op)
}

fn call(input: &mut Tokens, can_call: bool) -> Result<ast::Expr, Diagnostic> {
    let mut expr = access(input)?;

    while can_call && is_expr(input) {
        let arg = self::expr(input, false)?;
        let span = expr.span().join(arg.span());

        expr = ast::Expr::Call(ast::CallExpr {
            target: Box::new(expr),
            input: Box::new(arg),
            span,
        });
    }

    Ok(expr)
}

/// # BNF
///
/// ```text
/// <access> ::= <access> lbracket <expr> rbracket
///            | <access> dot <ident>
///            | <term>
/// ```
fn access(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let mut expr = term(input)?;

    loop {
        let (token, _) = input.peek();

        match token {
            Token::Dot => {
                input.expect(Token::Dot)?;

                let (name, span) = ident(input)?;

                let span = expr.span().join(span);
                expr = ast::Expr::Field(ast::FieldExpr {
                    target: Box::new(expr),
                    name,
                    span,
                });
            }

            Token::Open(Delim::Bracket) if !input.has_whitespace() => {
                input.expect(Token::Open(Delim::Bracket))?;

                let index = self::expr(input, true)?;

                input.expect(Token::Close(Delim::Bracket))?;

                let span = expr.span().join(index.span());
                expr = ast::Expr::Index(ast::IndexExpr {
                    target: Box::new(expr),
                    index: Box::new(index),
                    span,
                });
            }

            _ => break,
        }
    }

    Ok(expr)
}

/// # BNF
///
/// ```text
/// <term>   ::= <literal> | <path> | lparen <expr> rparen
/// ```
fn term(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let (token, span) = input.peek();

    match token {
        Token::Integer(value) => {
            input.consume();

            let lit = ast::Literal::Int { value, span };
            Ok(ast::Expr::Literal(lit))
        }

        Token::Floating(value) => {
            input.consume();

            let lit = ast::Literal::Float { value, span };
            Ok(ast::Expr::Literal(lit))
        }

        Token::String(value) => {
            input.consume();

            let lit = ast::Literal::String { value, span };
            Ok(ast::Expr::Literal(lit))
        }

        Token::Ident(_) => {
            let path = path(input)?;
            Ok(ast::Expr::Path(path))
        }

        Token::Open(Delim::Paren) => {
            input.expect(Token::Open(Delim::Paren))?;

            let expr = expr(input, true)?;

            input.expect(Token::Close(Delim::Paren))?;

            Ok(expr)
        }

        Token::Open(Delim::Bracket) => list(input),

        Token::Open(Delim::Brace) => {
            if is_record(input) {
                record(input)
            } else {
                block(input)
            }
        }

        _ => {
            let diagnostic = Diagnostic::error("expected::expression")
                .message(format!("expected an expression, found {}", token))
                .label(span, "here");

            Err(diagnostic)
        }
    }
}

fn list(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Open(Delim::Bracket))?;

    let multiline = input.is(Token::Newline);
    consume_newlines(input);

    let mut items = Vec::new();

    while !input.is(Token::Close(Delim::Bracket)) {
        let item = expr(input, true)?;
        items.push(item);

        if input.is(Token::Close(Delim::Bracket)) {
            break;
        }

        if multiline {
            consume_newlines(input);
        } else {
            input.expect(Token::Semi)?;
        }
    }

    let end = input.expect(Token::Close(Delim::Bracket))?;
    let span = start.join(end);

    Ok(ast::Expr::List(ast::ListExpr { items, span }))
}

fn record(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Open(Delim::Brace))?;

    let multiline = input.is(Token::Newline);
    consume_newlines(input);

    let mut fields = Vec::new();

    while !input.is(Token::Close(Delim::Brace)) {
        let (name, span) = ident(input)?;

        input.expect(Token::Colon)?;

        let value = expr(input, true)?;

        fields.push(ast::FieldInit { name, value, span });

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
    let span = start.join(end);

    Ok(ast::Expr::Record(ast::RecordExpr { fields, span }))
}

pub fn block(input: &mut Tokens) -> Result<ast::Expr, Diagnostic> {
    let start = input.expect(Token::Open(Delim::Brace))?;

    consume_newlines(input);

    let mut exprs = Vec::new();

    while !input.is(Token::Close(Delim::Brace)) {
        let expr = self::expr(input, true)?;
        exprs.push(expr);

        if input.is(Token::Close(Delim::Brace)) {
            break;
        }

        input.expect(Token::Newline)?;
        consume_newlines(input);
    }

    let end = input.expect(Token::Close(Delim::Brace))?;

    let span = start.join(end);

    Ok(ast::Expr::Block(ast::BlockExpr { exprs, span }))
}

fn is_record(input: &Tokens) -> bool {
    let mut offset = 1;

    while input.nth_is(offset, Token::Newline) {
        offset += 1;
    }

    matches!(input.peek_nth(offset), (Token::Ident(_), _)) && input.nth_is(offset + 1, Token::Colon)
}
