use std::collections::BTreeSet;

use super::{
    annotation::StmtAnnotation,
    body::{Bid, Local},
    ty::Tid,
};

#[derive(Clone, Debug)]
pub enum Stmt {
    // basic
    /// read from src into dst
    Read {
        annotations: BTreeSet<StmtAnnotation>,
        dst: Local,
        src: Source,
    },
    /// write into dst from src
    Write {
        annotations: BTreeSet<StmtAnnotation>,
        dst: Destination,
        src: Local,
    },
    /// eval expr into dst
    Eval {
        annotations: BTreeSet<StmtAnnotation>,
        dst: Local,
        expr: Expression,
    },

    // control flow
    /// loop over statements in block
    Loop {
        annotations: BTreeSet<StmtAnnotation>,
        block: Block,
    },
    /// match target agains cases
    Match {
        annotations: BTreeSet<StmtAnnotation>,
        target: Local,
        cases: Vec<Case>,
        default: Block,
    },
    /// return out of body with value
    Return {
        annotations: BTreeSet<StmtAnnotation>,
        value: Local,
    },
    /// break out of loop
    Break {
        annotations: BTreeSet<StmtAnnotation>,
    },

    // functions
    Close {
        annotations: BTreeSet<StmtAnnotation>,
        dst: Local,
        func: Bid,
    },
}

#[derive(Debug, Clone)]
pub enum Source {}

#[derive(Debug, Clone)]
pub enum Destination {}

#[derive(Debug, Clone)]
pub enum Expression {}

#[derive(Debug, Clone)]
pub struct Block {
    stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Case {
    /// type to match on
    pub tid: Tid,
    /// local to output into
    pub local: Local,
    /// case block (code)
    pub block: Block,
}
