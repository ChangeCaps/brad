mod ty;

pub use ty::*;

use crate::mir;

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
}

pub type Bodies = mir::Bodies<Tid>;

pub type Body = mir::Body<Tid>;
pub type BodyId = mir::BodyId;

pub type Block = mir::Block<Tid>;

pub type Locals = mir::Locals<Tid>;
pub type Local = mir::Local;

pub type Stmt = mir::Stmt<Tid>;
pub type Case = mir::Case<Tid>;

pub type Term = mir::Term<Tid>;

pub type Value = mir::Value<Tid>;
pub type Operand = mir::Operand;
pub type Const = mir::Const;
pub type Place = mir::Place;
pub type Proj = mir::Proj;

pub type BinaryOp = mir::BinaryOp;
pub type UnaryOp = mir::UnaryOp;
