use crate::diagnostic::Span;
use std::slice;

use super::Ty;

#[derive(Clone, Debug)]
pub struct Generic {
    pub name: &'static str,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Generics {
    pub generics: Vec<Generic>,
    pub span: Span,
}

#[derive(Clone, Debug)]
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
