use super::{BodyId, Local, Tid};

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
        default: Block,
        cases: Vec<(Tid, Local, Block)>,
    },
}

#[derive(Clone, Debug)]
pub enum Term {
    Return(Value),
    Exit,
}

#[derive(Clone, Debug)]
pub enum Value {
    Use(Operand),
    Tuple(Vec<Operand>),
    Record(Vec<(&'static str, Operand)>),
    Promote(Tid, Vec<Tid>, Operand),
    Coerce(Vec<Tid>, Vec<Tid>, Operand),
    Call(Operand, Operand),
    Binary(BinaryOp, Operand, Operand),
    Unary(UnaryOp, Operand),
    Closure(BodyId, Vec<Operand>, Vec<Tid>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    /// Integer operations
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
    /// Floating point operations
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
    /// Booleans only
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

impl Value {
    pub const ZST: Value = Value::Use(Operand::ZST);
}

#[derive(Clone, Debug)]
pub enum Operand {
    Place(Place),
    Const(Const),
}

impl Operand {
    pub const ZST: Operand = Operand::Const(Const::None);
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
