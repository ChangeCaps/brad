use super::Ty;
use crate::ast::spanned::Spanned;
use crate::diagnostic::Span;
use std::slice;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Generic {
    pub name: &'static str,
    pub span: Span,
}

impl Spanned for Generic {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
    }
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Generics {
    pub params: Vec<Generic>,
    pub span: Span,
}

impl Spanned for Generics {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.params.iter_mut().for_each(Spanned::reset_spans);
    }
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Spec {
    pub tys: Vec<Ty>,
    pub span: Span,
}

impl Spanned for Spec {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.tys.iter_mut().for_each(Spanned::reset_spans);
    }
}

impl<'a> IntoIterator for &'a Spec {
    type Item = &'a Ty;

    type IntoIter = slice::Iter<'a, Ty>;

    fn into_iter(self) -> Self::IntoIter {
        self.tys.iter()
    }
}
