use std::{fmt, ops::Deref};

use crate::{attribute::Attributes, diagnostic::Span};

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
    pub attrs: Attributes,
    pub is_extern: bool,
    pub name: Name,
    pub generics: Option<Generics>,
    pub args: Vec<Argument>,
    pub output: Option<Ty>,
    pub body: Option<Expr>,
    pub span: Span,
}

impl Func {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.params.iter())
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
    pub attrs: Attributes,
    pub is_extern: bool,
    pub name: Name,
    pub generics: Option<Generics>,
    pub ty: Option<Ty>,
    pub span: Span,
}

impl Type {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.params.iter())
    }
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub attrs: Attributes,
    pub name: Name,
    pub generics: Option<Generics>,
    pub ty: Ty,
    pub span: Span,
}

impl Alias {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.params.iter())
    }
}

#[derive(Clone, Debug)]
pub struct Import {
    pub attrs: Attributes,
    pub path: Path,
}

#[derive(Clone, Debug)]
pub struct Name {
    pub segments: Vec<&'static str>,
    pub span: Span,
}

impl Deref for Name {
    type Target = [&'static str];

    fn deref(&self) -> &Self::Target {
        &self.segments
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.segments.join("::"))
    }
}
