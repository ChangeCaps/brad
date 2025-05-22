use std::slice;

use diagnostic::Span;

use super::Ty;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Generic {
    pub name: &'static str,
    pub span: Span,
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Generics {
    pub params: Vec<Generic>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Spec {
    pub tys: Vec<Ty>,
    pub span: Span,
}

impl<'a> IntoIterator for &'a Spec {
    type Item = &'a Ty;

    type IntoIter = slice::Iter<'a, Ty>;

    fn into_iter(self) -> Self::IntoIter {
        self.tys.iter()
    }
}
