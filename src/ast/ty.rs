use crate::diagnostic::Span;

use super::{Generic, Path};

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Ty {
    Wild(Span),

    Int(Span),

    Float(Span),

    Str(Span),

    True(Span),

    False(Span),

    None(Span),

    Never(Span),

    Generic(Generic),

    Path(Path),

    Ref {
        ty: Box<Ty>,
        span: Span,
    },

    Func {
        input: Box<Ty>,
        output: Box<Ty>,
        span: Span,
    },

    List {
        ty: Box<Ty>,
        span: Span,
    },

    Tuple {
        tys: Vec<Ty>,
        span: Span,
    },

    Union {
        tys: Vec<Ty>,
        span: Span,
    },

    Record {
        fields: Vec<Field>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Field {
    pub name: &'static str,
    pub ty: Ty,
    pub span: Span,
}

impl Ty {
    pub fn span(&self) -> Span {
        match self {
            Ty::Wild(span) => *span,
            Ty::Int(span) => *span,
            Ty::Float(span) => *span,
            Ty::Str(span) => *span,
            Ty::True(span) => *span,
            Ty::False(span) => *span,
            Ty::None(span) => *span,
            Ty::Never(span) => *span,
            Ty::Generic(generic) => generic.span,
            Ty::Path(path) => path.span,
            Ty::Ref { span, .. } => *span,
            Ty::Func { span, .. } => *span,
            Ty::List { span, .. } => *span,
            Ty::Tuple { span, .. } => *span,
            Ty::Union { span, .. } => *span,
            Ty::Record { span, .. } => *span,
        }
    }
}
