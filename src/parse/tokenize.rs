use crate::diagnostic::{Diagnostic, SourceId, Span};

use super::{interner::Interner, stream::Tokens, token::Token};

impl Tokens {
    pub fn tokenize(
        interner: &mut Interner,
        source: SourceId,
        input: &str,
    ) -> Result<Self, Diagnostic> {
        let tokens = Tokenizer::new(interner, source, input).collect::<Result<Vec<_>, _>>()?;

        let span = Span {
            source,
            start: 0,
            end: input.len(),
        };

        Ok(Tokens::new(tokens, span))
    }
}

struct Tokenizer<'a> {
    input: &'a str,
    index: usize,
    source: SourceId,
    interner: &'a mut Interner,
}

impl<'a> Tokenizer<'a> {
    fn new(interner: &'a mut Interner, source: SourceId, input: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            input,
            index: 0,
            source,
            interner,
        }
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.index..]
    }

    fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.index += c.len_utf8();
        Some(c)
    }

    fn is_ident_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_ident_continue(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '-'
    }

    fn ident(&mut self) -> Result<(Token, Span), Diagnostic> {
        let start = self.index;

        while self.peek().map_or(false, Self::is_ident_continue) {
            self.consume();
        }

        let end = self.index;

        let span = Span {
            source: self.source,
            start,
            end,
        };

        let ident = &self.input[start..end];

        if ident == "_" {
            return Ok((Token::Under, span));
        }

        if let Some(token) = Token::from_keyword(ident) {
            return Ok((token, span));
        }

        let s = self.interner.intern(ident);
        Ok((Token::Ident(s), span))
    }

    fn number(&mut self) -> Result<(Token, Span), Diagnostic> {
        let start = self.index;

        while self.peek().map_or(false, |c| c.is_ascii_digit()) {
            self.consume();
        }

        let end = self.index;

        let span = Span {
            source: self.source,
            start,
            end,
        };

        let number = &self.input[start..end];
        let n = number.parse().unwrap();

        Ok((Token::Integer(n), span))
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Result<(Token, Span), Diagnostic>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut c = self.peek()?;

        while c.is_whitespace() && c != '\n' {
            self.consume();
            c = self.peek()?;
        }

        if c == '\n' {
            let span = Span {
                source: self.source,
                start: self.index,
                end: self.index + 1,
            };

            self.index += 1;

            return Some(Ok((Token::Newline, span)));
        }

        if Self::is_ident_start(c) {
            return Some(self.ident());
        }

        if c.is_ascii_digit() {
            return Some(self.number());
        }

        if self.remaining().len() >= 2 {
            if let Some(token) = Token::from_symbol(&self.remaining()[..2]) {
                let span = Span {
                    source: self.source,
                    start: self.index,
                    end: self.index + 2,
                };

                self.index += 2;

                return Some(Ok((token, span)));
            }
        }

        if let Some(mut token) = Token::from_symbol(&self.remaining()[..1]) {
            let span = Span {
                source: self.source,
                start: self.index,
                end: self.index + 1,
            };

            self.index += 1;

            let has_whitespace = self.peek().map_or(false, char::is_whitespace);

            match token {
                Token::Minus if !has_whitespace => token = Token::StickyMinus,
                Token::Star if !has_whitespace => token = Token::StickyStar,
                _ => {}
            }

            return Some(Ok((token, span)));
        }

        let span = Span {
            source: self.source,
            start: self.index,
            end: self.index + c.len_utf8(),
        };

        let diagnostic = Diagnostic::error("unexpected::character")
            .message(format!("unexpected character: {:?}", c))
            .label(span, "found here");

        Some(Err(diagnostic))
    }
}
