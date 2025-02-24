use crate::diagnostic::Span;

use super::{LocalId, Ty};

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

impl Expr {
    pub fn none(span: Span) -> Self {
        Self {
            kind: ExprKind::None,
            ty: Ty::None,
            span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    None,
    Int(i64),
    Float(f64),
    String(&'static str),
    Local(LocalId),
    List(Vec<Expr>),
    Record(Vec<Init>),
    Index(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, &'static str),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Ref(Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Let(LocalId, Box<Expr>),
    Block(Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct Init {
    pub name: &'static str,
    pub value: Expr,
    pub span: Span,
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
