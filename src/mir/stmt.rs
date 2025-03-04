use std::collections::BTreeSet;

use super::{Bid, Local, Ty};

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub term: Term,
}

impl Block {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            term: Term::Exit,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Assign(Place, Value),
    Match {
        target: Place,
        cases: Vec<Case>,
        default: Block,
    },
}

#[derive(Clone, Debug)]
pub struct Case {
    /// The type to match against.
    pub ty: Ty,

    /// The local to bind the instance to.
    pub local: Local,

    /// The block to execute if the type matches.
    pub block: Block,
}

#[derive(Clone, Debug)]
pub enum Term {
    Return(Value),
    Exit,
}

#[derive(Clone, Debug)]
pub enum Value {
    /// Use an operand directly.
    Use(Operand),

    /// Initialize a tuple.
    Tuple(Vec<Operand>),

    /// Initialize a record.
    Record(Vec<(&'static str, Operand)>),

    /// Promote a value to a variant.
    Promote {
        input: Ty,
        variants: BTreeSet<Ty>,
        operand: Operand,
    },

    /// Coerce a union to another union.
    Coerce {
        inputs: BTreeSet<Ty>,
        variants: BTreeSet<Ty>,
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
        captures: Vec<Operand>,
        generics: Vec<Ty>,
    },
}

impl Value {
    pub const NONE: Value = Value::Use(Operand::NONE);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    /* integer operations */
    Addi,
    Subi,
    Muli,
    Divi,
    Modi,
    BitAndi,
    BitOri,
    BitXori,
    Shli,
    Shri,
    Eqi,
    Nei,
    Lti,
    Lei,
    Gti,
    Gei,

    /* floating point operations */
    Addf,
    Subf,
    Mulf,
    Divf,
    Modf,
    Eqf,
    Nef,
    Ltf,
    Lef,
    Gtf,
    Gef,

    /* logical operations */
    And,
    Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Negi,
    BitNoti,
    Negf,
    // Only bools
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
