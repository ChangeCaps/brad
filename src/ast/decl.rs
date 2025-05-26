use std::{fmt, ops::Deref};

use diagnostic::Span;

use crate::attribute::Attributes;

use super::{Binding, Expr, Generic, Generics, Path, Ty, Spanned};

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Func(Func),
    Type(Type),
    Alias(Alias),
    Import(Import),
}

impl Spanned for Decl {
    fn span(&self) -> Span {
        match self {
            Decl::Func(func) => func.span(),
            Decl::Type(ty) => ty.span(),
            Decl::Alias(alias) => alias.span(),
            Decl::Import(import) => import.span(),
        }
    }

    fn reset_spans(&mut self) {
        match self {
            Decl::Func(func) => func.reset_spans(),
            Decl::Type(ty) => ty.reset_spans(),
            Decl::Alias(alias) => alias.reset_spans(),
            Decl::Import(import) => import.reset_spans(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl Spanned for Func {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.attrs.reset_spans();
        self.name.reset_spans();
        self.generics.reset_spans();
        self.args.reset_spans();
        self.output.reset_spans();
        self.body.reset_spans();
    }
}

impl Func {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.params.iter())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
    pub binding: Binding,
    pub ty: Option<Ty>,
    pub span: Span,
}

impl Spanned for Argument {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.binding.reset_spans();
        self.ty.reset_spans();
        self.span = Span::default();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub attrs: Attributes,
    pub is_extern: bool,
    pub name: Name,
    pub generics: Option<Generics>,
    pub ty: Option<Ty>,
    pub span: Span,
}

impl Spanned for Type {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.attrs.reset_spans();
        self.name.reset_spans();
        self.generics.reset_spans();
        self.ty.reset_spans();
        self.span = Span::default();
    }
}

impl Type {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.params.iter())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    pub attrs: Attributes,
    pub name: Name,
    pub generics: Option<Generics>,
    pub ty: Ty,
    pub span: Span,
}

impl Spanned for Alias {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.attrs.reset_spans();
        self.name.reset_spans();
        self.generics.reset_spans();
        self.ty.reset_spans();
        self.span = Span::default();
    }
}

impl Alias {
    pub fn generics(&self) -> impl Iterator<Item = &Generic> {
        self.generics
            .as_ref()
            .into_iter()
            .flat_map(|g| g.params.iter())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub attrs: Attributes,
    pub path: Path,
}

impl Spanned for Import {
    fn span(&self) -> Span {
        self.path.span()
    }

    fn reset_spans(&mut self) {
        self.path.reset_spans();
        self.attrs.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Name {
    pub segments: Vec<&'static str>,
    pub span: Span,
}

impl Spanned for Name {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
    }
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
