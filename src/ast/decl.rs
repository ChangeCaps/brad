use crate::diagnostic::Span;

use super::{Binding, Expr, Generics, Path, Ty};

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
    pub args: Vec<Argument>,
    pub output: Option<Ty>,
    pub body: Expr,
    pub span: Span,
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

#[derive(Clone, Debug)]
pub struct Alias {
    pub name: &'static str,
    pub generics: Option<Generics>,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub path: Path,
}
