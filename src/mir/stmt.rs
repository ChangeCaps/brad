use super::{BodyId, Local, Tid};
use crate::hir::{BinaryOp, UnaryOp};

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
    Match(Place, Block, Vec<(Tid, Block)>),
}

#[derive(Clone, Debug)]
pub enum Term {
    Return(Value),
    Exit,
}

#[derive(Clone, Debug)]
pub enum Value {
    Use(Operand),
    Record(Vec<Operand>),
    Promote(Tid, Vec<Tid>, Operand),
    Coerce(Vec<Tid>, Vec<Tid>, Operand),
    Call(Operand, Operand),
    Binary(BinaryOp, Operand, Operand),
    Unary(UnaryOp, Operand),
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
    pub const ZST: Operand = Operand::Const(Const::Zst);
}

#[derive(Clone, Debug)]
pub enum Const {
    Zst,
    Int(i64),
    Float(f64),
    String(&'static str),
    Func(BodyId, Vec<Tid>),
}

/// Represents a place in memory.
#[derive(Clone, Debug)]
pub struct Place {
    pub local: Local,
    pub proj: Vec<Proj>,
}

#[derive(Clone, Debug)]
pub enum Proj {
    Field(usize),
    Index(Local),
    Deref,
}
