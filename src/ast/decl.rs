use crate::diagnostic::Span;

use super::{Binding, Expr, Generic, Generics, Path, Ty};

#[derive(Clone, Debug)]
pub enum Decl {
    Func(Func),
    Type(Type),
    Alias(Alias),
    Import(Import),
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: &'static str,
    pub generics: Option<Generics>,
    pub args: Vec<Argument>,
    pub output: Option<Ty>,
    pub body: Expr,
    pub is_extern: bool,
    pub span: Span,
}

impl Func {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.generics.iter())
    }
}

#[derive(Clone, Debug)]
pub struct Argument {
    pub binding: Binding,
    pub ty: Option<Ty>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: &'static str,
    pub generics: Option<Generics>,
    pub ty: Option<Ty>,
    pub span: Span,
}

impl Type {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.generics.iter())
    }
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub name: &'static str,
    pub generics: Option<Generics>,
    pub ty: Ty,
    pub span: Span,
}

impl Alias {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.generics.iter())
    }
}

#[derive(Clone, Debug)]
pub struct Import {
    pub path: Path,
}
