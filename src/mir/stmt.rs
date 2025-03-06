use std::collections::BTreeSet;

use super::{Bid, Local, Ty};

#[derive(Clone, Debug)]
pub struct Block<T = Ty> {
    pub stmts: Vec<Stmt<T>>,
    pub term: Term<T>,
}

impl<T> Block<T> {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            term: Term::Exit,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt<T = Ty> {
    Assign(Place, Value<T>),

    Loop(Block<T>),

    Match {
        target: Place,
        cases: Vec<Case<T>>,
        default: Block<T>,
    },
}

#[derive(Clone, Debug)]
pub struct Case<T = Ty> {
    /// The type to match against.
    pub ty: T,

    /// The local to bind the instance to.
    pub local: Local,

    /// The block to execute if the type matches.
    pub block: Block<T>,
}

#[derive(Clone, Debug)]
pub enum Term<T = Ty> {
    Return(Value<T>),
    Break,
    Exit,
}

#[derive(Clone, Debug)]
pub enum Value<T = Ty> {
    /// Use an operand directly.
    Use(Operand),

    /// Initialize a tuple.
    Tuple(Vec<Operand>),

    /// Initialize a record.
    Record(Vec<(&'static str, Operand)>),

    /// Promote a value to a variant.
    Promote {
        input: T,
        variants: BTreeSet<T>,
        operand: Operand,
    },

    /// Coerce a union to another union.
    Coerce {
        inputs: BTreeSet<T>,
        variants: BTreeSet<T>,
        operand: Operand,
    },

    /// Call a closure with arguments.
    Call(Operand, Operand),

    /// Perform a binary operation.
    Binary(BinaryOp, Operand, Operand),

    /// Perform a unary operation.
    Unary(UnaryOp, Operand),

    /// Create a closure, capturing the given operands.
    Closure {
        body: Bid,
        generics: Vec<T>,
        captures: Vec<Operand>,
    },
}

impl<T> Value<T> {
    pub const NONE: Self = Self::Use(Operand::NONE);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    /* integer operations */
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    BAnd,
    BOr,
    BXor,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    /* shifts */
    LShr,
    LShl,

    /* floating point operations */
    FAdd,
    FSub,
    FMul,
    FDiv,
    FMod,

    FEq,
    FNe,
    FLt,
    FLe,
    FGt,
    FGe,

    /* logical operations */
    And,
    Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    FNeg,
    BNot,
    // only bool
    Not,
    // only ptrs
    Deref,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Place(Place),
    Const(Const),
}

impl Operand {
    pub const NONE: Operand = Operand::Const(Const::None);
}

#[derive(Clone, Debug)]
pub enum Const {
    None,
    Int(i64),
    Float(f64),
    String(&'static str),
}

/// Represents a place in memory.
#[derive(Clone, Debug)]
pub struct Place {
    pub local: Local,
    pub proj: Vec<Proj>,
    pub is_mutable: bool,
}

#[derive(Clone, Debug)]
pub enum Proj {
    Field(&'static str),
    Tuple(usize),
    Index(Local),
    Deref,
}
