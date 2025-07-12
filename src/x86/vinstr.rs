use super::{
    common::{Imm32, Imm64, MemScale, RegConstraint, SizeConstraint, SizeKind},
    reg::Reg,
};

#[derive(Debug, Clone, Copy)]
pub struct VirtualReg(u32);

impl VirtualReg {
    pub fn new(v: u32) -> Self {
        Self(v)
    }
}

// TODO :: add label
#[derive(Debug, Clone, Copy)]
pub enum VirtualArg {
    Reg { local: VirtualReg },
    Imm,
    Mem { index: u32, base: u32 },
}

// TODO :: add label
#[derive(Debug, Clone, Copy)]
pub enum Arg {
    Reg {
        rconst: RegConstraint,
        sconst: SizeConstraint,
    },
    Imm32 {
        v: Imm32,
    },
    Imm64 {
        v: Imm64,
    },
    Mem {
        // scale of index
        scale: MemScale,
        // index in terms of scale
        index: RegConstraint,
        // base of memory block
        base: RegConstraint,
        // element offset
        offset: i32,
    },
}

impl Arg {
    pub fn reg_i64() -> Self {
        Self::Reg {
            rconst: RegConstraint::Int,
            sconst: SizeConstraint::Specific(SizeKind::S64),
        }
    }

    pub fn imm_32(value: Imm32) -> Self {
        Self::Imm32 { v: value }
    }

    pub fn imm_64(value: Imm64) -> Self {
        Self::Imm64 { v: value }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VInstrKind {
    MOV,

    NEG,
    ADD,
    SUB,
    IMUL,
    IDIV,
    IMOD,

    NOT,
    AND,
    OR,
    XOR,

    SHL,
    SHR,

    TEST,
    CMP,

    SETE,
    SETNE,
    SETG,
    SETGE,
    SETL,
    SETLE,

    JMP,
    JE,
    JNE,
    JG,
    JGE,
    JL,
    JLE,

    PUSH,
    POP,
}

#[derive(Debug, Clone)]
pub struct VInstr {
    pub kind: VInstrKind,
    pub dest: Option<(VirtualArg, Arg)>,
    pub srcs: Vec<(VirtualArg, Arg)>,
}

#[derive(Debug, Clone)]
pub struct VCall {}

#[derive(Debug, Clone)]
pub struct VRet {}

#[derive(Debug, Clone)]
pub enum VInstrElement {
    VInstr(VInstr),
    Label(u32),
    AllocVReg(VirtualReg),
    DropVReg(VirtualReg),
    VCall(VCall),
    VRet(VRet),
}

#[derive(Debug, Clone)]
pub struct VInstrBlock {
    elems: Vec<VInstrElement>,
}

impl VInstrBlock {
    pub fn new() -> Self {
        Self { elems: Vec::new() }
    }

    pub fn clear(&mut self) {
        self.elems.clear();
    }

    pub fn put(&mut self, elem: VInstrElement) {
        self.elems.push(elem);
    }

    pub fn iter(&self) -> impl Iterator<Item = &VInstrElement> {
        self.elems.iter()
    }
}

impl VInstr {
    pub fn mov_ri(dst: VirtualArg, src: Imm64) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![(VirtualArg::Imm, Arg::imm_64(src))],
        })
    }

    pub fn mov_rr(dst: VirtualArg, src: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![(src, Arg::reg_i64())],
        })
    }

    pub fn mov_rm(dst: VirtualArg, mem: VirtualArg, scale: MemScale, offset: i32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![(
                mem,
                Arg::Mem {
                    scale,
                    index: RegConstraint::Int,
                    base: RegConstraint::Int,
                    offset,
                },
            )],
        })
    }

    pub fn mov_mr(mem: VirtualArg, scale: MemScale, offset: i32, src: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV,
            dest: Some((
                mem,
                Arg::Mem {
                    scale,
                    index: RegConstraint::Int,
                    base: RegConstraint::Int,
                    offset,
                },
            )),
            srcs: vec![(src, Arg::reg_i64())],
        })
    }

    pub fn neg(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::NEG,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64())],
        })
    }

    pub fn add(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::ADD,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn sub(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SUB,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn imul(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::IMUL,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn idiv(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::IDIV,
            dest: None,
            srcs: vec![
                (
                    dst,
                    Arg::Reg {
                        rconst: RegConstraint::Specific(Reg::RAX),
                        sconst: SizeConstraint::Specific(SizeKind::S64),
                    },
                ),
                (rhs, Arg::reg_i64()),
            ],
        })
    }

    pub fn imod(dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::IDIV,
            dest: Some((
                dst,
                Arg::Reg {
                    rconst: RegConstraint::Specific(Reg::RDX),
                    sconst: SizeConstraint::Specific(SizeKind::S64),
                },
            )),
            srcs: vec![
                (
                    lhs,
                    Arg::Reg {
                        rconst: RegConstraint::Specific(Reg::RAX),
                        sconst: SizeConstraint::Specific(SizeKind::S64),
                    },
                ),
                (rhs, Arg::reg_i64()),
            ],
        })
    }

    pub fn not(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::NOT,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64())],
        })
    }

    pub fn and(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::AND,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn or(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::OR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn xor_ri(dst: VirtualArg, rhs: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::XOR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (VirtualArg::Imm, Arg::imm_32(rhs))],
        })
    }

    pub fn xor_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::XOR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn shl(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SHL,
            dest: None,
            srcs: vec![
                (dst, Arg::reg_i64()),
                (
                    rhs,
                    Arg::Reg {
                        rconst: RegConstraint::Specific(Reg::RCX),
                        sconst: SizeConstraint::Any,
                    },
                ),
            ],
        })
    }

    pub fn shr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SHR,
            dest: None,
            srcs: vec![
                (dst, Arg::reg_i64()),
                (
                    rhs,
                    Arg::Reg {
                        rconst: RegConstraint::Specific(Reg::RCX),
                        sconst: SizeConstraint::Any,
                    },
                ),
            ],
        })
    }

    pub fn cmp(lhs: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::CMP,
            dest: None,
            srcs: vec![(lhs, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn test_ri(lhs: VirtualArg, rhs: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::TEST,
            dest: None,
            srcs: vec![(lhs, Arg::reg_i64()), (VirtualArg::Imm, Arg::imm_32(rhs))],
        })
    }

    pub fn test_rr(lhs: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::TEST,
            dest: None,
            srcs: vec![(lhs, Arg::reg_i64()), (rhs, Arg::reg_i64())],
        })
    }

    pub fn sete(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETE,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
        })
    }

    pub fn setne(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETNE,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
        })
    }

    pub fn setg(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETG,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
        })
    }

    pub fn setge(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETGE,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
        })
    }

    pub fn setl(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETL,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
        })
    }

    pub fn setle(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETLE,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
        })
    }

    pub fn jmp(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JMP,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }

    pub fn je(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JE,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }

    pub fn jne(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JNE,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }

    pub fn jg(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JG,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }

    pub fn jge(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JGE,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }

    pub fn jl(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JL,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }

    pub fn jle(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JLE,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
        })
    }
}
