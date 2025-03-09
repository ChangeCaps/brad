use std::collections::BTreeSet;

use super::{Bid, Local, Locals, Ty};

#[derive(Clone, Debug)]
pub struct Block<T = Ty> {
    pub stmts: Vec<Stmt<T>>,
    pub term: Option<Term<T>>,
}

impl<T> Block<T> {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            term: None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt<T = Ty> {
    Drop(Value<T>),

    Assign(Place<T>, Value<T>),

    Loop(Block<T>),

    Match {
        target: Place<T>,
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
}

#[derive(Clone, Debug)]
pub enum Value<T = Ty> {
    /// Use an operand directly.
    Use(Operand<T>),

    /// Initialize a tuple.
    Tuple(Vec<Operand<T>>),

    /// Initialize a record.
    Record(Vec<(&'static str, Operand<T>)>),

    /// Promote a value to a variant.
    Promote {
        variant: T,
        variants: BTreeSet<T>,
        operand: Operand<T>,
    },

    /// Coerce a union to another union.
    Coerce {
        inputs: BTreeSet<T>,
        variants: BTreeSet<T>,
        operand: Operand<T>,
    },

    /// Call a closure with arguments.
    Call(Place<T>, Operand<T>),

    /// Perform a binary operation.
    Binary(BinaryOp, Operand<T>, Operand<T>),

    /// Perform a unary operation.
    Unary(UnaryOp, Operand<T>),

    /// Create a closure, capturing the given operands.
    Closure {
        body: Bid,
        generics: Vec<T>,
        captures: Vec<Operand<T>>,
    },
}

impl Value {
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
pub enum Operand<T = Ty> {
    Copy(Place<T>),
    Const(Const, T),
}

impl Operand {
    pub const NONE: Self = Operand::Const(Const::None, Ty::None);
}

impl<T> Operand<T> {
    pub fn ty<'a>(&'a self, locals: &'a Locals<T>) -> &'a T {
        match self {
            Operand::Copy(place) => place.ty(locals),
            Operand::Const(_, ty) => ty,
        }
    }
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
pub struct Place<T = Ty> {
    pub local: Local,
    pub proj: Vec<(Proj, T)>,
    pub is_mutable: bool,
}

impl<T> Place<T> {
    pub fn ty<'a>(&'a self, locals: &'a Locals<T>) -> &'a T {
        match self.proj.last() {
            Some((_, ty)) => ty,
            None => &locals[self.local],
        }
    }
}

#[derive(Clone, Debug)]
pub enum Proj {
    Field(&'static str),
    Tuple(usize),
    Index(Local),
    Deref,
}
