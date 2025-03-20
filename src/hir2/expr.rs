use crate::{
    diagnostic::Span,
    solve::{Tag, Ty},
};

use super::{BodyId, LocalId};

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

impl Expr {
    pub fn none(span: Span) -> Self {
        Self {
            kind: ExprKind::ZeroSize(Tag::NONE),
            ty: Ty::NONE,
            span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    ZeroSize(Tag),
    String(&'static str),
    Local(LocalId),
    Tag(Tag, Box<Expr>),
    Func(BodyId),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Record(Vec<Init>),
    Index(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, &'static str),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Ref(Box<Expr>),
    Match(Box<Expr>, MatchBody),
    Loop(Box<Expr>),
    Break(Option<Box<Expr>>),
    Let(Binding, Box<Expr>),
    Block(Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct Init {
    pub name: &'static str,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MatchBody {
    pub arms: Vec<Arm>,
    pub default: Option<Box<(Binding, Expr)>>,
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Tag {
        tag: Tag,
        binding: Binding,
        span: Span,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Deref,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, Debug)]
pub enum Binding {
    Wild { span: Span },

    Bind { local: LocalId, span: Span },

    Tuple { bindings: Vec<Binding>, span: Span },
}
