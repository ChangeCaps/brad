use std::{fmt, sync::Arc};

use diagnostic::{Diagnostic, Span};

use super::token::Token;

#[derive(Clone, Copy, Debug)]
pub struct TokenEntry {
    pub whitespace: bool,
    pub token: Token,
    pub span: Span,
}

#[derive(Clone)]
pub struct Tokens {
    tokens: Arc<[TokenEntry]>,
    index: usize,
    span: Span,
}

impl Tokens {
    pub fn new(tokens: Vec<TokenEntry>, span: Span) -> Tokens {
        Tokens {
            tokens: tokens.into(),
            index: 0,
            span,
        }
    }

    pub fn nth_has_whitespace(&self, n: usize) -> bool {
        (self.tokens.get(self.index + n)).is_some_and(|e| e.whitespace)
    }

    pub fn has_whitespace(&self) -> bool {
        self.nth_has_whitespace(0)
    }

    pub fn peek(&self) -> (Token, Span) {
        self.peek_nth(0)
    }

    pub fn consume(&mut self) -> (Token, Span) {
        self.consume_nth(0)
    }

    pub fn peek_nth(&self, n: usize) -> (Token, Span) {
        match self.tokens.get(self.index + n) {
            Some(result) => (result.token, result.span),
            None => {
                let span = Span::new(
                    self.span.source,
                    self.span.end.saturating_sub(1),
                    self.span.end,
                );

                (Token::Eof, span)
            }
        }
    }

    pub fn consume_nth(&mut self, n: usize) -> (Token, Span) {
        let result = self.peek_nth(n);

        self.index += n + 1;
        self.index = self.index.min(self.tokens.len());

        result
    }

    pub fn span(&self) -> Span {
        let (_, start) = self.peek();
        start
    }

    pub fn take<E>(&mut self, expected: E) -> bool
    where
        E: PartialEq<Token>,
    {
        if self.is(expected) {
            self.consume();
            true
        } else {
            false
        }
    }

    pub fn nth_is<E>(&self, n: usize, expected: E) -> bool
    where
        E: PartialEq<Token>,
    {
        let (actual, _) = self.peek_nth(n);
        expected == actual
    }

    pub fn is<E>(&self, expected: E) -> bool
    where
        E: PartialEq<Token>,
    {
        self.nth_is(0, expected)
    }

    pub fn expect<E>(&mut self, expected: E) -> Result<Span, Diagnostic>
    where
        E: PartialEq<Token> + fmt::Display,
    {
        let (actual, span) = self.consume();

        if expected == actual {
            return Ok(span);
        }

        let diagnostic = Diagnostic::error("unexpected::token")
            .message(format!("expected `{}`, found `{}`", expected, actual))
            .label(span, "here");

        Err(diagnostic)
    }
}

impl fmt::Debug for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.tokens.iter().map(|e| e.token))
            .finish()
    }
}
