use solve::{Tag, Tags};

use super::{Bid, Local, Tid};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Unary {
        op: UnaryOp,
        dst: Local,
        val: Value,
    },
    Binary {
        op: BinaryOp,
        dst: Local,
        lhs: Value,
        rhs: Value,
    },
    Match {
        target: Value,
        arms: Vec<(Tag, Arm)>,
        default: Option<Box<Arm>>,
    },
    TupleInit {
        dst: Local,
        vals: Vec<Value>,
    },
    ArrayInit {
        dst: Local,
        vals: Vec<Value>,
    },
    RecordInit {
        dst: Local,
        vals: Vec<(&'static str, Value)>,
    },
    UnionInit {
        dst: Local,
        val: Value,
        union: Tags,
    },
    Closure {
        dst: Local,
        func: Bid,
    },
    Call {
        dst: Local,
        src: Local,
        arg: Value,
    },
    Mov {
        dst: Local, // reg
        src: Local, // reg
    },
    Read {
        dst: Local, // reg
        src: Local, // addr
        access: Vec<&'static str>,
    },
    Write {
        dst: Local, // addr
        access: Vec<&'static str>,
        src: Value, // reg
    },
    ReadIndex {
        dst: Local, // reg
        src: Local, // addr
        index: Value,
        access: Vec<&'static str>,
    },
    WriteIndex {
        dst: Local, // addr
        index: Value,
        access: Vec<&'static str>,
        src: Value, // reg
    },
    Return {
        val: Value,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Local(Local),
    Int(i64),
    Float(f64),
    String(&'static str),
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
