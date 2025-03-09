use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    /* special tokens */
    Ident(&'static str),
    String(&'static str),
    Integer(i64),
    Floating(f64),

    /* whitespace */
    Newline,
    Eof,

    /* two-character symbols */
    DotDot,
    EqEq,
    NotEq,
    GtEq,
    LtEq,
    GtGt,
    LtLt,
    AmpAmp,
    PipePipe,
    ThinArrow,
    ThickArrow,
    ColonColon,

    /* one-character symbols */
    Open(Delim),
    Close(Delim),
    Pound,
    Bang,
    Tilde,
    At,
    Dollar,
    Percent,
    Caret,
    Amp,
    Pipe,
    Star,
    Slash,
    Plus,
    Minus,
    Quote,
    Question,
    Under,
    Colon,
    Semi,
    Comma,
    Dot,
    Eq,
    Gt,
    Lt,

    /* keywords */
    Alias,
    As,
    Async,
    Await,
    Break,
    False,
    Float,
    Fn,
    Import,
    Extern,
    Int,
    Is,
    Let,
    Loop,
    Match,
    Mut,
    None,
    Pub,
    Ref,
    Return,
    Str,
    True,
    Type,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Delim {
    Paren,
    Bracket,
    Brace,
}

impl Token {
    pub fn from_keyword(s: &str) -> Option<Token> {
        Some(match s {
            "alias" => Token::Alias,
            "as" => Token::As,
            "async" => Token::Async,
            "await" => Token::Await,
            "break" => Token::Break,
            "false" => Token::False,
            "float" => Token::Float,
            "fn" => Token::Fn,
            "import" => Token::Import,
            "extern" => Token::Extern,
            "int" => Token::Int,
            "is" => Token::Is,
            "let" => Token::Let,
            "loop" => Token::Loop,
            "match" => Token::Match,
            "mut" => Token::Mut,
            "none" => Token::None,
            "pub" => Token::Pub,
            "ref" => Token::Ref,
            "return" => Token::Return,
            "str" => Token::Str,
            "true" => Token::True,
            "type" => Token::Type,

            _ => return None,
        })
    }

    pub fn from_symbol(s: &str) -> Option<Token> {
        Some(match s {
            /* two-character symbols */
            ".." => Token::DotDot,
            "==" => Token::EqEq,
            "!=" => Token::NotEq,
            ">=" => Token::GtEq,
            "<=" => Token::LtEq,
            "<<" => Token::LtLt,
            ">>" => Token::GtGt,
            "&&" => Token::AmpAmp,
            "||" => Token::PipePipe,
            "->" => Token::ThinArrow,
            "=>" => Token::ThickArrow,
            "::" => Token::ColonColon,

            /* one-character symbols */
            "(" => Token::Open(Delim::Paren),
            "[" => Token::Open(Delim::Bracket),
            "{" => Token::Open(Delim::Brace),
            ")" => Token::Close(Delim::Paren),
            "]" => Token::Close(Delim::Bracket),
            "}" => Token::Close(Delim::Brace),
            "#" => Token::Pound,
            "!" => Token::Bang,
            "~" => Token::Tilde,
            "@" => Token::At,
            "$" => Token::Dollar,
            "%" => Token::Percent,
            "^" => Token::Caret,
            "&" => Token::Amp,
            "|" => Token::Pipe,
            "*" => Token::Star,
            "/" => Token::Slash,
            "+" => Token::Plus,
            "-" => Token::Minus,
            "'" => Token::Quote,
            "?" => Token::Question,
            "_" => Token::Under,
            ":" => Token::Colon,
            ";" => Token::Semi,
            "," => Token::Comma,
            "." => Token::Dot,
            "=" => Token::Eq,
            ">" => Token::Gt,
            "<" => Token::Lt,

            _ => return None,
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            /* special tokens */
            Token::Ident(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Integer(i) => write!(f, "{}", i),
            Token::Floating(x) => write!(f, "{}", x),

            /* whitespace */
            Token::Newline => write!(f, "newline"),
            Token::Eof => write!(f, "end of file"),

            /* two-character symbols */
            Token::DotDot => write!(f, ".."),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::GtEq => write!(f, ">="),
            Token::LtEq => write!(f, "<="),
            Token::LtLt => write!(f, "<<"),
            Token::GtGt => write!(f, ">>"),
            Token::AmpAmp => write!(f, "&&"),
            Token::PipePipe => write!(f, "||"),
            Token::ThinArrow => write!(f, "->"),
            Token::ThickArrow => write!(f, "=>"),
            Token::ColonColon => write!(f, "::"),

            /* one-character symbols */
            Token::Open(Delim::Paren) => write!(f, "("),
            Token::Open(Delim::Bracket) => write!(f, "["),
            Token::Open(Delim::Brace) => write!(f, "{{"),
            Token::Close(Delim::Paren) => write!(f, ")"),
            Token::Close(Delim::Bracket) => write!(f, "]"),
            Token::Close(Delim::Brace) => write!(f, "}}"),
            Token::Pound => write!(f, "#"),
            Token::Bang => write!(f, "!"),
            Token::Tilde => write!(f, "~"),
            Token::At => write!(f, "@"),
            Token::Dollar => write!(f, "$"),
            Token::Percent => write!(f, "%"),
            Token::Caret => write!(f, "^"),
            Token::Amp => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Quote => write!(f, "'"),
            Token::Question => write!(f, "?"),
            Token::Under => write!(f, "_"),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Eq => write!(f, "="),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),

            /* keywords */
            Token::Alias => write!(f, "alias"),
            Token::As => write!(f, "as"),
            Token::Async => write!(f, "async"),
            Token::Await => write!(f, "await"),
            Token::Break => write!(f, "break"),
            Token::False => write!(f, "false"),
            Token::Float => write!(f, "float"),
            Token::Fn => write!(f, "fn"),
            Token::Import => write!(f, "import"),
            Token::Extern => write!(f, "extern"),
            Token::Int => write!(f, "int"),
            Token::Is => write!(f, "is"),
            Token::Let => write!(f, "let"),
            Token::Loop => write!(f, "loop"),
            Token::Match => write!(f, "match"),
            Token::Mut => write!(f, "mut"),
            Token::None => write!(f, "none"),
            Token::Pub => write!(f, "pub"),
            Token::Ref => write!(f, "ref"),
            Token::Return => write!(f, "return"),
            Token::Str => write!(f, "str"),
            Token::True => write!(f, "true"),
            Token::Type => write!(f, "type"),
        }
    }
}
