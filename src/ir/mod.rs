mod body;
mod types;

pub use body::*;
pub use types::*;

use solve::{Tag, Tags};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Tid(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bid(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Local(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Unit,
    Func(Bid),
    Local(Local),
    Block(Vec<Expr>),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arm {
    pub local: Local,
    pub body: Expr,
}
