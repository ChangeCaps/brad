use std::fmt;

use crate::diagnostic::Span;

use super::generic::Spec;

#[derive(Clone, Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub spec: Option<Spec>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum PathSegment {
    Item(PathItem),
}

impl PathSegment {
    pub fn span(&self) -> Span {
        match self {
            PathSegment::Item(item) => item.span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PathItem {
    pub name: &'static str,
    pub span: Span,
}

impl fmt::Display for PathItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PathSegment::Item(item) => write!(f, "{}", item),
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let segments: Vec<_> = self.segments.iter().map(ToString::to_string).collect();

        write!(f, "{}", segments.join("::"))
    }
}
