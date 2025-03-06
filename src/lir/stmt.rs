use super::{body::Local, ty::Tid};

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    /// dst = src
    Eval { dst: Var, src: Value },
    /// returns from function giving val
    Return { val: Operand },
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
    Match {
        target: Var,
        cases: Vec<Case>,
        default: Block,
    },
}

#[derive(Clone, Debug)]
pub struct Case {
    pub tid: Tid,
    pub local: Var,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Var {
    local: Local,
    access: Access,
}

#[derive(Clone, Debug)]
pub enum Value {
    /// refer to local
    Use(Var),
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
    Call(Operand, Operand),
    /// create closure from function
    Closure { func: Tid },
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
    Const(Const),
}

#[derive(Clone, Debug)]
pub enum Const {
    Empty(Tid),
    Int(Tid, i64),
    Float(Tid, f64),
    String(Tid, &'static str),
}

#[derive(Clone, Debug)]
pub struct Access(Vec<u32>);

impl Access {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> u32 {
        self.0.len() as u32
    }

    pub fn push(&mut self, field_index: u32) {
        self.0.push(field_index);
    }
}
