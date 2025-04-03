use super::generic::Spec;
use crate::ast::spanned::Spanned;
use crate::diagnostic::Span;
use std::fmt;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub spec: Option<Spec>,
    pub span: Span,
}

impl Spanned for Path {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.segments.reset_spans();
    }
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct PathSegment {
    pub name: &'static str,
    pub span: Span,
}

impl Spanned for PathSegment {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
    }
}

impl AsRef<str> for PathSegment {
    fn as_ref(&self) -> &str {
        self.name
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let segments: Vec<_> = self.segments.iter().map(ToString::to_string).collect();

        write!(f, "{}", segments.join("::"))
    }
}
