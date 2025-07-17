use super::{
    common::{ComptimeVal, Imm32, Imm64, MemScale},
    reg::Reg,
};

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub enum Instr {
    MOV_RI {
        dst: Reg,
        src: Imm64,
    },
    MOV_RR {
        dst: Reg,
        src: Reg,
    },
    MOV_RM {
        dst: Reg,
        scale: MemScale,
        index: Option<Reg>,
        base: Option<Reg>,
        offset: Imm32,
    },
    MOV_MR {
        scale: MemScale,
        index: Option<Reg>,
        base: Option<Reg>,
        offset: Imm32,
        src: Reg,
    },

    NEG_R {
        dst: Reg,
    },

    ADD_RI {
        dst: Reg,
        rhs: Imm32,
    },

    ADD_RR {
        dst: Reg,
        rhs: Reg,
    },

    SUB_RI {
        dst: Reg,
        rhs: Imm32,
    },

    SUB_RR {
        dst: Reg,
        rhs: Reg,
    },

    IMUL_RR {
        dst: Reg,
        rhs: Reg,
    },

    IDIV_RAX_R {
        rhs: Reg,
    },
    IMOD_RDX_R {
        rhs: Reg,
    },

    NOT_R {
        dst: Reg,
    },
    AND_RR {
        dst: Reg,
        rhs: Reg,
    },
    OR_RR {
        dst: Reg,
        rhs: Reg,
    },
    XOR_RI {
        dst: Reg,
        rhs: Imm32,
    },
    XOR_RR {
        dst: Reg,
        rhs: Reg,
    },

    SHL_R_RCX {
        dst: Reg,
    },
    SHR_R_RCX {
        dst: Reg,
    },

    TEST {
        lhs: Reg,
        rhs: Reg,
    },
    CMP {
        lhs: Reg,
        rhs: Reg,
    },

    SETE_R {
        dst: Reg,
    },
    SETNE_R {
        dst: Reg,
    },
    SETG {
        dst: Reg,
    },
    SETGE {
        dst: Reg,
    },
    SETL {
        dst: Reg,
    },
    SETLE {
        dst: Reg,
    },

    CALL_M {
        // TODO
    },
    RET,
}

#[derive(Debug, Clone)]
pub enum InstrElement {
    Instr(Instr),
    Label(ComptimeVal),
}

#[derive(Debug, Clone)]
pub struct InstrBlock {
    elems: Vec<InstrElement>,
}

impl InstrBlock {
    pub fn new() -> Self {
        Self { elems: Vec::new() }
    }

    pub fn clear(&mut self) {
        self.elems.clear();
    }

    pub fn put(&mut self, elem: InstrElement) {
        self.elems.push(elem);
    }

    pub fn puti(&mut self, instr: Instr) {
        self.put(InstrElement::Instr(instr));
    }

    pub fn putl(&mut self, label: ComptimeVal) {
        self.put(InstrElement::Label(label));
    }

    pub fn iter(&self) -> impl Iterator<Item = &InstrElement> {
        self.elems.iter()
    }
}
