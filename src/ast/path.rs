use crate::diagnostic::Span;
use std::fmt;

use super::generic::Spec;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub spec: Option<Spec>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct PathSegment {
    pub name: &'static str,
    pub span: Span,
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
