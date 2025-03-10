use std::default;

use super::{body::Bid, body::Local, ty::Tid};

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }

    pub fn push(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    /// dst = src
    Eval { dst: Var, src: Value },

    //// dst = new closure from func
    /// create closure from function
    Closure { dst: Var, func: Bid },

    /// returns from function giving val
    /// defaults to None
    Return { val: Operand },

    /// breaks out of loop giving val
    /// return val is in local
    Break,

    /// destroy val (deconstruct/free)
    Drop { var: Operand, tid: Tid },

    /// dst[index].access = val
    WriteIndex {
        dst: Var,
        index: Operand,
        access: Access,
        val: Operand,
    },

    /// dst = array[index].access
    ReadIndex {
        dst: Var,
        array: Var,
        index: Operand,
        access: Access,
    },

    /// (*dst).access = val
    WriteRef {
        dst: Var,
        access: Access,
        val: Operand,
    },

    /// dst = (*mem).access
    ReadRef {
        dst: Var,
        mem: Operand,
        access: Access,
    },

    /// match statement
    Match {
        target: Var,
        cases: Vec<Case>,
        default: Block,
    },

    /// loop statement
    Loop { body: Block },
}

#[derive(Clone, Debug)]
pub struct Case {
    /// type to match on
    pub tid: Tid,
    /// local to output into
    pub local: Var,
    /// case block (code)
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub local: Local,
    pub access: Vec<(Access, Tid)>,
}

impl From<Local> for Var {
    fn from(local: Local) -> Self {
        Self {
            local,
            access: Vec::new(),
        }
    }
}

impl From<(Local, Access, Tid)> for Var {
    fn from((local, access, tid): (Local, Access, Tid)) -> Self {
        Self {
            local,
            access: {
                let mut v = Vec::new();
                v.push((access, tid));
                v
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    /// refer to local
    Use(Operand),
    /// compute binary operation
    Binary(BinaryOp, Operand, Operand),
    /// compute younary operation
    Unary(UnaryOp, Operand),
    /// promote to union type output from member type input
    Promote {
        input: Tid,
        output: Tid,
        operand: Operand,
    },
    /// convert to union type output from union subtype input
    Coerce {
        input: Tid,
        output: Tid,
        operand: Operand,
    },
    /// call closure with arg
    Call(Var, Operand),
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
    Var(Var),
    Const(Tid, Const),
}

#[derive(Clone, Debug)]
pub enum Const {
    Empty,
    Int(i64),
    Float(f64),
    String(&'static str),
}

#[derive(Clone, Debug)]
pub enum Access {
    Tuple(u32),
    Field(&'static str),
}

impl Var {
    pub fn new() -> Self {
        Self {
            local: Local(0),
            access: Vec::new(),
        }
    }

    pub fn len(&self) -> u32 {
        self.access.len() as u32
    }

    pub fn push(&mut self, access: Access, tid: Tid) {
        self.access.push((access, tid));
    }

    pub fn clear(&mut self) {
        self.access.clear();
    }
}

#[derive(Clone, Debug)]
pub struct Place {
    pub local: Local,
    pub index: Option<Operand>,
    pub deref: bool,
    pub proj: Vec<(Proj, Tid)>,
}

#[derive(Clone, Debug)]
pub enum Proj {
    Field(&'static str),
    Tuple(u32),
}

impl Place {
    pub fn new() -> Self {
        Self {
            local: Local(0),
            index: None,
            deref: false,
            proj: Vec::new(),
        }
    }

    pub fn from(&mut self, local: Local) {
        self.local = local;
        self.index = None;
        self.deref = false;
        self.proj.clear();
    }
}
