use super::{Bid, Local, Locals, Ty};
use std::collections::BTreeSet;
use std::fmt::Formatter;

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

    pub fn push(&mut self, stmt: Stmt<T>) {
        if self.term.is_none() {
            self.stmts.push(stmt);
        }
    }

    pub fn term(&mut self, term: Term<T>) {
        self.term = Some(term);
    }
}

#[derive(Clone, Debug)]
pub enum Stmt<T = Ty> {
    Drop(Local),

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

impl<T> Value<T> {
    pub fn local(local: Local) -> Self {
        Self::Use(Operand::local(local))
    }
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

    FLt,
    FLe,
    FGt,
    FGe,

    /* logical operations */
    And,
    Or,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::BAnd => "&",
            Self::BOr => "|",
            Self::BXor => "^",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Gt => ">",
            Self::Ge => ">=",
            Self::LShr => ">>",
            Self::LShl => "<<",
            Self::FAdd => "+",
            Self::FSub => "-",
            Self::FMul => "*",
            Self::FDiv => "/",
            Self::FMod => "%",
            Self::FLt => "<",
            Self::FLe => "<=",
            Self::FGt => ">",
            Self::FGe => ">=",
            Self::And => "&&",
            Self::Or => "||",
        };

        write!(f, "{}", s)
    }
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

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Neg => "-",
            Self::FNeg => "-",
            Self::BNot => "~",
            Self::Not => "!",
            Self::Deref => "*",
        };

        write!(f, "{}", s)
    }
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
    pub fn local(local: Local) -> Self {
        Operand::Copy(Place::local(local))
    }

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
    pub proj: Vec<(Proj<T>, T)>,
    pub is_mutable: bool,
}

impl<T> Place<T> {
    pub fn local(local: Local) -> Self {
        Self {
            local,
            proj: Vec::new(),
            is_mutable: false,
        }
    }

    pub fn ty<'a>(&'a self, locals: &'a Locals<T>) -> &'a T {
        match self.proj.last() {
            Some((_, ty)) => ty,
            None => &locals[self.local],
        }
    }
}

#[derive(Clone, Debug)]
pub enum Proj<T = Ty> {
    Field(&'static str),
    Tuple(usize),
    Index(Operand<T>),
    Deref,
}
