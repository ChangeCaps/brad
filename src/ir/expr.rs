use solve::{Tag, Tags};

use super::{Bid, Local, Tid};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Unit,
    Func(Bid),
    Local(Local),
    Int(i64),
    Float(f64),
    String(&'static str),
    Block(Vec<Expr>),
    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    Record(Vec<(&'static str, Expr)>),
    Tag(Tags, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, &'static str),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Match {
        target: Box<Expr>,
        arms: Vec<(Tag, Arm)>,
        default: Option<Box<Arm>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Tid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
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

#[derive(Clone, Debug, PartialEq)]
pub struct Arm {
    pub local: Local,
    pub body: Expr,
}
