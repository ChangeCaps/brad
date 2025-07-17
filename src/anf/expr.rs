use super::{Bid, Local, Tid};
use crate::hir2;
use diagnostic::Span;
use solve::{Tag, Tags};

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
        dst: Local,
        target: Value,
        arms: Vec<(Tag, Arm)>,
        default: Option<Box<Arm>>,
    },
    TagInit {
        dst: Local,
        tag: Tag,
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
        src: Value, // reg
    },
    Read {
        dst: Local, // reg
        src: Local, // addr
        access: Option<&'static str>,
    },
    Write {
        dst: Local, // addr
        access: Option<&'static str>,
        src: Value, // reg
    },
    ReadIndex {
        dst: Local, // reg
        src: Local, // addr
        index: Value,
        access: Option<&'static str>,
    },
    WriteIndex {
        dst: Local, // addr
        index: Value,
        access: Option<&'static str>,
        src: Value, // reg
    },
    Loop {
        dst: Local,
        body: Vec<Expr>,
    },
    Continue {},
    Break {
        value: Option<Value>,
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
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

impl From<hir2::UnaryOp> for UnaryOp {
    fn from(op: hir2::UnaryOp) -> Self {
        match op {
            hir2::UnaryOp::Neg => UnaryOp::Neg,
            hir2::UnaryOp::Not => UnaryOp::Not,
            hir2::UnaryOp::BitNot => UnaryOp::BitNot,
            _ => panic!("Unsupported unary operation: {:?}", op),
        }
    }
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

impl From<hir2::BinaryOp> for BinaryOp {
    fn from(op: hir2::BinaryOp) -> Self {
        match op {
            hir2::BinaryOp::Add => BinaryOp::Add,
            hir2::BinaryOp::Sub => BinaryOp::Sub,
            hir2::BinaryOp::Mul => BinaryOp::Mul,
            hir2::BinaryOp::Div => BinaryOp::Div,
            hir2::BinaryOp::Mod => BinaryOp::Mod,
            hir2::BinaryOp::And => BinaryOp::And,
            hir2::BinaryOp::Or => BinaryOp::Or,
            hir2::BinaryOp::BitAnd => BinaryOp::BitAnd,
            hir2::BinaryOp::BitOr => BinaryOp::BitOr,
            hir2::BinaryOp::BitXor => BinaryOp::BitXor,
            hir2::BinaryOp::Shl => BinaryOp::Shl,
            hir2::BinaryOp::Shr => BinaryOp::Shr,
            hir2::BinaryOp::Eq => BinaryOp::Eq,
            hir2::BinaryOp::Ne => BinaryOp::Ne,
            hir2::BinaryOp::Lt => BinaryOp::Lt,
            hir2::BinaryOp::Le => BinaryOp::Le,
            hir2::BinaryOp::Gt => BinaryOp::Gt,
            hir2::BinaryOp::Ge => BinaryOp::Ge,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arm {
    pub local: Local,
    pub body: Vec<Expr>,
}
