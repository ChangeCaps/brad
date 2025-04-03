use super::{Generic, Path};
use crate::ast::spanned::Spanned;
use crate::diagnostic::Span;

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

impl Spanned for Ty {
    fn span(&self) -> Span {
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

    fn reset_spans(&mut self) {
        match self {
            Ty::Wild(span) => *span = Span::default(),
            Ty::Int(span) => *span = Span::default(),
            Ty::Float(span) => *span = Span::default(),
            Ty::Str(span) => *span = Span::default(),
            Ty::True(span) => *span = Span::default(),
            Ty::False(span) => *span = Span::default(),
            Ty::None(span) => *span = Span::default(),
            Ty::Never(span) => *span = Span::default(),
            Ty::Generic(generic) => generic.reset_spans(),
            Ty::Path(path) => path.reset_spans(),
            Ty::Ref { span, ty } => {
                ty.reset_spans();
                *span = Span::default();
            }
            Ty::Func {
                span,
                input,
                output,
            } => {
                input.reset_spans();
                output.reset_spans();
                *span = Span::default()
            }
            Ty::List { span, ty } => {
                ty.reset_spans();
                *span = Span::default()
            }
            Ty::Tuple { span, tys } => {
                tys.reset_spans();
                *span = Span::default()
            }
            Ty::Union { span, tys } => {
                tys.reset_spans();
                *span = Span::default()
            }
            Ty::Record { span, fields } => {
                *span = Span::default();
                fields.reset_spans();
            }
        }
    }
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Field {
    pub name: &'static str,
    pub ty: Ty,
    pub span: Span,
}

impl Spanned for Field {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.ty.reset_spans();
    }
}
