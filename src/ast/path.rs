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
