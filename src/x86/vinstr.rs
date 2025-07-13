use super::{
    common::{
        ComptimeVal, Imm32, Imm64, MemScale, PrimitiveType, RegConstraint, SizeConstraint, SizeKind,
    },
    reg::Reg,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VirtualReg(pub u32);

impl VirtualReg {
    pub fn new(v: u32) -> Self {
        Self(v)
    }

    pub fn usize(self) -> usize {
        self.0 as usize
    }
}

// TODO :: add label
#[derive(Debug, Clone, Copy)]
pub enum VirtualArg {
    Reg {
        local: VirtualReg,
    },
    Imm,
    Mem {
        index: Option<VirtualReg>,
        base: Option<VirtualReg>,
    },
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
        offset: Imm32,
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

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub enum VInstrKind {
    MOV_RI,
    MOV_RR,
    MOV_RM,
    MOV_MR,

    NEG_R,
    ADD_RR,
    SUB_RR,
    IMUL_RR,
    IDIV_RR,
    IMOD_RR,

    NOT_R,
    AND_RR,
    OR_RR,
    XOR_RI,
    XOR_RR,

    SHL_RR,
    SHR_RR,

    TEST_RI,
    TEST_RR,
    CMP_RR,

    SETE_R,
    SETNE_R,
    SETG_R,
    SETGE_R,
    SETL_R,
    SETLE_R,

    JMP_I,
    JMP_R,
    JMP_M,
    JE_I,
    JNE_I,
    JG_I,
    JGE_I,
    JL_I,
    JLE_I,

    PUSH,
    POP,
}

#[derive(Debug, Clone)]
pub struct VInstr {
    pub kind: VInstrKind,
    pub dest: Option<(VirtualArg, Arg)>,
    pub srcs: Vec<(VirtualArg, Arg)>,
    pub clobber: Vec<Reg>,
}

#[derive(Debug, Clone)]
pub struct VCall {
    args: Vec<(VirtualReg, PrimitiveType)>,
}

#[derive(Debug, Clone)]
pub struct VRet {}

#[derive(Debug, Clone)]
pub enum VInstrElement {
    // manual insertion
    VInstr(VInstr),
    Label(ComptimeVal),
    VCall(VCall),
    VRet(VRet),

    // automatic insertion
    DropVReg(VirtualReg),
    LoopBegin(ComptimeVal, Vec<VirtualReg>),
    LoopEnd(ComptimeVal),
    LoopExit(ComptimeVal),
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
            kind: VInstrKind::MOV_RI,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![(VirtualArg::Imm, Arg::imm_64(src))],
            clobber: Vec::new(),
        })
    }

    pub fn mov_rr(dst: VirtualArg, src: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV_RR,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![(src, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn mov_rm(
        dst: VirtualArg,
        mem: VirtualArg,
        scale: MemScale,
        offset: Imm32,
    ) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV_RM,
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
            clobber: Vec::new(),
        })
    }

    pub fn mov_mr(
        mem: VirtualArg,
        scale: MemScale,
        offset: Imm32,
        src: VirtualArg,
    ) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::MOV_MR,
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
            clobber: Vec::new(),
        })
    }

    pub fn neg_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::NEG_R,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn add_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::ADD_RR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn sub_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SUB_RR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn imul_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::IMUL_RR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn idiv_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::IDIV_RR,
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
            clobber: vec![Reg::RDX],
        })
    }

    pub fn imod_rr(dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::IDIV_RR,
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
            clobber: vec![Reg::RAX],
        })
    }

    pub fn not_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::NOT_R,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn and_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::AND_RR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn or_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::OR_RR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn xor_ri(dst: VirtualArg, rhs: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::XOR_RI,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (VirtualArg::Imm, Arg::imm_32(rhs))],
            clobber: Vec::new(),
        })
    }

    pub fn xor_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::XOR_RR,
            dest: None,
            srcs: vec![(dst, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn shl_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SHL_RR,
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
            clobber: Vec::new(),
        })
    }

    pub fn shr_rr(dst: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SHR_RR,
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
            clobber: Vec::new(),
        })
    }

    pub fn cmp_rr(lhs: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::CMP_RR,
            dest: None,
            srcs: vec![(lhs, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn test_ri(lhs: VirtualArg, rhs: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::TEST_RI,
            dest: None,
            srcs: vec![(lhs, Arg::reg_i64()), (VirtualArg::Imm, Arg::imm_32(rhs))],
            clobber: Vec::new(),
        })
    }

    pub fn test_rr(lhs: VirtualArg, rhs: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::TEST_RR,
            dest: None,
            srcs: vec![(lhs, Arg::reg_i64()), (rhs, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn sete_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETE_R,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
            clobber: Vec::new(),
        })
    }

    pub fn setne_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETNE_R,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
            clobber: Vec::new(),
        })
    }

    pub fn setg_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETG_R,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
            clobber: Vec::new(),
        })
    }

    pub fn setge_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETGE_R,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
            clobber: Vec::new(),
        })
    }

    pub fn setl_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETL_R,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
            clobber: Vec::new(),
        })
    }

    pub fn setle_r(dst: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::SETLE_R,
            dest: Some((dst, Arg::reg_i64())),
            srcs: vec![],
            clobber: Vec::new(),
        })
    }

    pub fn jmp_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JMP_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }

    pub fn jmp_r(reg: VirtualArg) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JMP_R,
            dest: None,
            srcs: vec![(reg, Arg::reg_i64())],
            clobber: Vec::new(),
        })
    }

    pub fn jmp_m(mem: VirtualArg, scale: MemScale, offset: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JMP_M,
            dest: None,
            srcs: vec![(
                mem,
                Arg::Mem {
                    scale,
                    index: RegConstraint::Int,
                    base: RegConstraint::Int,
                    offset,
                },
            )],
            clobber: Vec::new(),
        })
    }

    pub fn je_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JE_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }

    pub fn jne_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JNE_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }

    pub fn jg_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JG_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }

    pub fn jge_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JGE_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }

    pub fn jl_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JL_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }

    pub fn jle_rel(rel: Imm32) -> VInstrElement {
        VInstrElement::VInstr(VInstr {
            kind: VInstrKind::JLE_I,
            dest: None,
            srcs: vec![(VirtualArg::Imm, Arg::imm_32(rel))],
            clobber: Vec::new(),
        })
    }
}
