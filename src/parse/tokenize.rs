use crate::diagnostic::{Diagnostic, SourceId, Span};

use super::{interner::Interner, stream::Tokens, token::Token, TokenEntry};

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

fn is_digit(c: Option<char>) -> bool {
    c.is_some_and(|c| c.is_ascii_digit())
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

    fn peek_nth(&self, n: usize) -> Option<char> {
        self.remaining().chars().nth(n)
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

        while self.peek().is_some_and(Self::is_ident_continue) {
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
        let mut is_floating = false;

        while is_digit(self.peek()) {
            self.consume();

            // Allow one dot in the number for floating points
            if !is_floating && self.peek_nth(0) == Some('.') && is_digit(self.peek_nth(1)) {
                is_floating = true;
                self.consume();
            }
        }

        let end = self.index;

        let span = Span {
            source: self.source,
            start,
            end,
        };

        let number = &self.input[start..end];

        match is_floating {
            true => Ok((Token::Floating(number.parse().unwrap()), span)),
            false => Ok((Token::Integer(number.parse().unwrap()), span)),
        }
    }

    fn string(&mut self) -> Result<(Token, Span), Diagnostic> {
        let start = self.index;
        self.consume();

        let mut content = String::new();

        loop {
            let Some(c) = self.consume() else {
                let span = Span {
                    source: self.source,
                    start,
                    end: self.index,
                };

                let diagnostic = Diagnostic::error("unexpected::character")
                    .message("unexpected end of file in string")
                    .label(span, "here");

                return Err(diagnostic);
            };

            if c == '"' {
                break;
            }

            if c != '\\' {
                content.push(c);
                continue;
            }

            let Some(c) = self.consume() else {
                let span = Span {
                    source: self.source,
                    start,
                    end: self.index,
                };

                let diagnostic = Diagnostic::error("unexpected::character")
                    .message("unexpected end of file in string")
                    .label(span, "here");

                return Err(diagnostic);
            };

            match c {
                'n' => {
                    content.push('\n');
                }

                'r' => {
                    content.push('\r');
                }

                't' => {
                    content.push('\t');
                }

                '0' => {
                    content.push('\0');
                }

                _ => {
                    let span = Span {
                        source: self.source,
                        start,
                        end: self.index,
                    };

                    let diagnostic = Diagnostic::error("unexpected::character")
                        .message(format!("unexpected character in string, found `{}`", c))
                        .label(span, "here");

                    return Err(diagnostic);
                }
            }
        }

        let end = self.index;

        let span = Span {
            source: self.source,
            start,
            end,
        };

        let s = self.interner.intern(&content);

        Ok((Token::String(s), span))
    }
}

fn with_whitespace(whitespace: bool) -> impl FnOnce((Token, Span)) -> TokenEntry {
    move |(token, span)| TokenEntry {
        token,
        span,
        whitespace,
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Result<TokenEntry, Diagnostic>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut c = self.peek()?;

        let whitespace = c.is_whitespace() && c != '\n';
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

            return Some(Ok(TokenEntry {
                whitespace,
                token: Token::Newline,
                span,
            }));
        }

        if Self::is_ident_start(c) {
            return Some(self.ident().map(with_whitespace(whitespace)));
        }

        if c.is_ascii_digit() {
            return Some(self.number().map(with_whitespace(whitespace)));
        }

        if c == '"' {
            return Some(self.string().map(with_whitespace(whitespace)));
        }

        if c == '/' && self.peek_nth(1) == Some('/') {
            while self.consume() != Some('\n') {}
            return self.next();
        }

        if self.remaining().len() >= 2 {
            if let Some(token) = Token::from_symbol(&self.remaining()[..2]) {
                let span = Span {
                    source: self.source,
                    start: self.index,
                    end: self.index + 2,
                };

                self.index += 2;

                return Some(Ok(TokenEntry {
                    whitespace,
                    token,
                    span,
                }));
            }
        }

        if let Some(token) = Token::from_symbol(&self.remaining()[..1]) {
            let span = Span {
                source: self.source,
                start: self.index,
                end: self.index + 1,
            };

            self.index += 1;

            return Some(Ok(TokenEntry {
                whitespace,
                token,
                span,
            }));
        }

        let span = Span {
            source: self.source,
            start: self.index,
            end: self.index + c.len_utf8(),
        };

        let diagnostic = Diagnostic::error("unexpected::character")
            .message(format!("unexpected character `{}`", c))
            .label(span, "found here");

        Some(Err(diagnostic))
    }
}
