use core::panic;
use std::collections::{BTreeSet, HashMap};

use crate::anf;

use super::{
    common::{Imm32, Imm64, MemScale, RegConstraint},
    instr::{Instr, InstrBlock, InstrElement},
    reg::Reg,
    vbuild,
    vinstr::{self, VInstrElement, VInstrKind, VirtualArg, VirtualReg},
};

pub struct Builder {
    anf: anf::Program,
}

impl Builder {
    pub fn new(anf: anf::Program) -> Self {
        Self { anf }
    }

    pub fn build(self) {
        let mut vbbuilder = vbuild::BodyBuilder::new();
        let mut bbuilder = BodyBuilder::new();

        for (bid, body) in self.anf.bodies.iter() {
            if body.is_extern {
                continue;
            }

            let vbody = vbbuilder.build(body, &self.anf.types);
            let body = bbuilder.build(vbody, &self.anf.types);

            todo!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallingConv {
    iargs: Vec<Reg>,
    fargs: Vec<Reg>,

    iret: Vec<Reg>,
    fret: Vec<Reg>,

    isave: BTreeSet<Reg>,
    fsave: BTreeSet<Reg>,
}

impl CallingConv {
    pub fn sysv_abi() -> Self {
        CallingConv::new(
            &[Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9],
            &[
                Reg::XMM0,
                Reg::XMM1,
                Reg::XMM2,
                Reg::XMM3,
                Reg::XMM4,
                Reg::XMM5,
                Reg::XMM6,
                Reg::XMM7,
            ],
            &[Reg::RAX, Reg::RDX],
            &[Reg::XMM0, Reg::XMM1],
            &[
                Reg::RBX,
                Reg::RSP,
                Reg::RBP,
                Reg::R12,
                Reg::R13,
                Reg::R14,
                Reg::R15,
            ],
            &[],
        )
    }

    pub fn new(
        iargs: &[Reg],
        fargs: &[Reg],
        iret: &[Reg],
        fret: &[Reg],
        isave: &[Reg],
        fsave: &[Reg],
    ) -> Self {
        let mut ret = Self {
            iargs: iargs.to_vec(),
            fargs: fargs.to_vec(),
            iret: iret.to_vec(),
            fret: fret.to_vec(),
            isave: BTreeSet::new(),
            fsave: BTreeSet::new(),
        };

        for reg in isave {
            ret.isave.insert(reg.clone());
        }

        for reg in fsave {
            ret.fsave.insert(reg.clone());
        }

        ret
    }
}

#[derive(Debug, Clone)]
struct RegUsage {
    ifree: BTreeSet<Reg>,
    ffree: BTreeSet<Reg>,
    iclobber: BTreeSet<Reg>,
    fclobber: BTreeSet<Reg>,
    map: HashMap<Reg, VirtualReg>,
}

impl RegUsage {
    pub fn new() -> Self {
        Self {
            ifree: BTreeSet::new(),
            ffree: BTreeSet::new(),
            iclobber: BTreeSet::new(),
            fclobber: BTreeSet::new(),
            map: HashMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.ifree.clear();
        self.ffree.clear();

        self.iclobber.clear();
        self.fclobber.clear();

        self.map.clear();

        for reg in Reg::ALLOC_I {
            self.ifree.insert(reg);
        }

        for reg in Reg::ALLOC_F {
            self.ffree.insert(reg);
        }
    }

    pub fn alloc_i(&mut self, v_reg: VirtualReg) -> Option<Reg> {
        let ret = self.ifree.pop_last();

        if let Some(reg) = ret {
            self.map.insert(reg, v_reg);
            self.iclobber.insert(reg);
        }

        ret
    }

    pub fn alloc_si(&mut self, v_reg: VirtualReg, reg: Reg) -> VirtualReg {
        if self.ifree.remove(&reg) {
            self.map.insert(reg, v_reg);
            self.iclobber.insert(reg);
            v_reg
        } else {
            match self.map.get(&reg) {
                Some(virt) => virt.clone(),
                None => panic!("Impossible"),
            }
        }
    }

    pub fn alloc_f(&mut self, v_reg: VirtualReg) -> Option<Reg> {
        let ret = self.ffree.pop_last();

        if let Some(reg) = ret {
            self.map.insert(reg, v_reg);
            self.fclobber.insert(reg);
        }

        ret
    }

    pub fn alloc_sf(&mut self, v_reg: VirtualReg, reg: Reg) -> VirtualReg {
        if self.ffree.remove(&reg) {
            self.map.insert(reg, v_reg);
            self.fclobber.insert(reg);
            v_reg
        } else {
            match self.map.get(&reg) {
                Some(virt) => virt.clone(),
                None => panic!("Impossible"),
            }
        }
    }

    pub fn remap(&mut self, reg: Reg, v_reg: VirtualReg) {
        if self.map.insert(reg, v_reg).is_none() {
            panic!("Remapped unallocated register");
        }
    }

    pub fn free(&mut self, reg: Reg) {
        self.map.remove(&reg);
        if reg.float {
            self.ffree.insert(reg);
        } else {
            self.ifree.insert(reg);
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum VLocation {
    None,
    Stack(i32),
    Reg(Reg),
}

#[derive(Debug, Clone)]
struct BodyBuilder {
    instrs: InstrBlock,
    v_map: Vec<VLocation>,
    conv: CallingConv,
    usage: RegUsage,
    sp: i32,
}

impl BodyBuilder {
    pub fn new() -> Self {
        Self {
            instrs: InstrBlock::new(),
            v_map: Vec::new(),
            conv: CallingConv::sysv_abi(),
            usage: RegUsage::new(),
            sp: 0,
        }
    }

    pub fn push(&mut self, v_reg: &VirtualReg) {
        let idx = v_reg.usize();

        // TODO :: maybe size check if everything isn't 8 bytes
        self.sp -= 8;

        match self.v_map[idx] {
            VLocation::None => panic!(),
            VLocation::Stack(_) => {}
            VLocation::Reg(reg) => {
                self.instrs.puti(Instr::MOV_MR {
                    scale: MemScale::S1,
                    index: None,
                    base: Some(Reg::RSP),
                    offset: Imm32::Const(self.sp),
                    src: reg,
                });
                self.v_map[idx] = VLocation::Stack(self.sp);
                self.free(reg);
            }
        };
    }

    pub fn ensure_reg(
        &mut self,
        v_reg: VirtualReg,
        rconst: RegConstraint,
        save: &Vec<Reg>,
        forbidden: &Vec<Reg>,
    ) -> Reg {
        let idx = v_reg.usize();

        match self.v_map[idx] {
            VLocation::None => self.alloc(v_reg, rconst, save, forbidden),
            VLocation::Stack(offset) => {
                let reg = self.alloc(v_reg, rconst, save, forbidden);
                self.instrs.puti(Instr::MOV_RM {
                    dst: reg,
                    scale: MemScale::S1,
                    index: None,
                    base: Some(Reg::RSP),
                    offset: Imm32::Const(offset),
                });
                reg
            }
            VLocation::Reg(reg) => reg,
        }
    }

    pub fn evict_regs(&mut self, regs: &Vec<Reg>, save: &Vec<Reg>) {
        let forbidden = &mut regs.clone();
        forbidden.extend(save.iter());

        for reg in regs {
            self.evict_reg(reg.clone(), forbidden);
        }
    }

    pub fn evict_reg(&mut self, e_reg: Reg, forbidden: &Vec<Reg>) {
        let v_reg = self.usage.map.get(&e_reg).cloned();
        match v_reg {
            Some(v_reg) => {
                if e_reg.float {
                    for reg in Reg::ALLOC_F {
                        if forbidden.contains(&reg) {
                            continue;
                        }

                        if self.usage.map.contains_key(&reg) {
                            continue;
                        }

                        self.usage.alloc_sf(v_reg, reg);
                        self.usage.map.remove(&e_reg);
                        self.v_map[v_reg.usize()] = VLocation::Reg(reg);

                        return;
                    }
                } else {
                    for reg in Reg::ALLOC_I {
                        if forbidden.contains(&reg) {
                            continue;
                        }

                        if self.usage.map.contains_key(&reg) {
                            continue;
                        }

                        self.usage.alloc_si(v_reg, reg);
                        self.usage.map.remove(&e_reg);
                        self.v_map[v_reg.usize()] = VLocation::Reg(reg);

                        return;
                    }
                }

                self.push(&v_reg);
            }
            None => {}
        };
    }

    pub fn alloc(
        &mut self,
        v_reg: VirtualReg,
        rconst: RegConstraint,
        save: &Vec<Reg>,
        forbidden: &Vec<Reg>,
    ) -> Reg {
        let idx = v_reg.usize();

        match rconst {
            RegConstraint::Int => {
                let alloc = {
                    let mut iter = Reg::ALLOC_I.iter();
                    loop {
                        if let Some(reg) = iter.next() {
                            if forbidden.contains(&reg) {
                                continue;
                            }
                            match self.usage.map.get(&reg) {
                                Some(_) => {
                                    continue;
                                }
                                None => {
                                    break Some(reg);
                                }
                            }
                        } else {
                            break None;
                        }
                    }
                };

                match alloc {
                    Some(reg) => reg.clone(),
                    None => {
                        let mut iter = Reg::ALLOC_I.iter();
                        let res = loop {
                            if let Some(reg) = iter.next() {
                                if forbidden.contains(&reg) {
                                    continue;
                                }
                                if save.contains(&reg) {
                                    continue;
                                }
                                break Some(reg);
                            } else {
                                break None;
                            }
                        };

                        match res {
                            Some(reg) => {
                                let val = self.usage.map.get(reg).cloned();
                                match val {
                                    Some(virt) => {
                                        self.push(&virt);
                                        self.usage.alloc_si(v_reg, reg.clone());
                                        self.v_map[idx] = VLocation::Reg(reg.clone());
                                        reg.clone()
                                    }
                                    None => panic!("Not possible"),
                                }
                            }
                            None => panic!("Out of regs"),
                        }
                    }
                }
            }
            RegConstraint::Float => {
                let alloc = {
                    let mut iter = Reg::ALLOC_I.iter();
                    loop {
                        if let Some(reg) = iter.next() {
                            if forbidden.contains(&reg) {
                                continue;
                            }
                            match self.usage.map.get(&reg) {
                                Some(_) => {
                                    continue;
                                }
                                None => {
                                    break Some(reg);
                                }
                            }
                        } else {
                            break None;
                        }
                    }
                };

                match alloc {
                    Some(reg) => reg.clone(),
                    None => {
                        let mut iter = Reg::ALLOC_I.iter();
                        let res = loop {
                            if let Some(reg) = iter.next() {
                                if forbidden.contains(&reg) {
                                    continue;
                                }
                                if save.contains(&reg) {
                                    continue;
                                }
                                break Some(reg);
                            } else {
                                break None;
                            }
                        };

                        match res {
                            Some(reg) => {
                                let val = self.usage.map.get(reg).cloned();
                                match val {
                                    Some(virt) => {
                                        self.push(&virt);
                                        self.usage.alloc_si(v_reg, reg.clone());
                                        self.v_map[idx] = VLocation::Reg(reg.clone());
                                        reg.clone()
                                    }
                                    None => panic!("Not possible"),
                                }
                            }
                            None => panic!("Out of regs"),
                        }
                    }
                }
            }
            RegConstraint::Specific(reg) => {
                if save.contains(&reg) {
                    panic!("oh fucky");
                }

                if forbidden.contains(&reg) {
                    panic!("oh wucky");
                }

                if reg.float {
                    let alloced = self.usage.alloc_sf(v_reg, reg);
                    if alloced == v_reg {
                        self.v_map[idx] = VLocation::Reg(reg);
                        reg
                    } else {
                        self.evict_reg(reg, save);
                        self.usage.alloc_sf(v_reg, reg);
                        self.v_map[idx] = VLocation::Reg(reg);
                        reg
                    }
                } else {
                    let alloced = self.usage.alloc_si(v_reg, reg);
                    if alloced == v_reg {
                        self.v_map[idx] = VLocation::Reg(reg);
                        reg
                    } else {
                        self.evict_reg(reg, save);
                        self.usage.alloc_si(v_reg, reg);
                        self.v_map[idx] = VLocation::Reg(reg);
                        reg
                    }
                }
            }
        }
    }

    pub fn expect_format(vinstr: &vinstr::VInstr, dest: bool, srcs: usize) {
        assert!(vinstr.srcs.len() == srcs);
        match vinstr.dest {
            Some(_) => assert!(dest == true),
            None => assert!(dest == false),
        }
    }

    pub fn expect_dest(
        dest: Option<(vinstr::VirtualArg, vinstr::Arg)>,
    ) -> (vinstr::VirtualArg, vinstr::Arg) {
        match dest {
            Some((varg, arg)) => (varg, arg),
            None => panic!(),
        }
    }

    pub fn expect_imm32(src: (vinstr::VirtualArg, vinstr::Arg)) -> Imm32 {
        let (varg, arg) = src;

        match arg {
            vinstr::Arg::Imm32 { v } => match varg {
                VirtualArg::Imm => v,
                VirtualArg::Reg { local: _ } | VirtualArg::Mem { index: _, base: _ } => {
                    panic!("invalid varg")
                }
            },
            vinstr::Arg::Reg {
                rconst: _,
                sconst: _,
            }
            | vinstr::Arg::Imm64 { v: _ }
            | vinstr::Arg::Mem {
                scale: _,
                index: _,
                base: _,
                offset: _,
            } => panic!("invalid arg"),
        }
    }

    pub fn expect_imm64(src: (vinstr::VirtualArg, vinstr::Arg)) -> Imm64 {
        let (varg, arg) = src;

        match arg {
            vinstr::Arg::Imm64 { v } => match varg {
                VirtualArg::Imm => v,
                VirtualArg::Reg { local: _ } | VirtualArg::Mem { index: _, base: _ } => {
                    panic!("invalid varg")
                }
            },
            vinstr::Arg::Reg {
                rconst: _,
                sconst: _,
            }
            | vinstr::Arg::Imm32 { v: _ }
            | vinstr::Arg::Mem {
                scale: _,
                index: _,
                base: _,
                offset: _,
            } => panic!("invalid arg"),
        }
    }
    pub fn expect_reg(src: (vinstr::VirtualArg, vinstr::Arg)) -> (VirtualReg, RegConstraint) {
        let (varg, arg) = src;

        match arg {
            vinstr::Arg::Reg { rconst, sconst: _ } => match varg {
                VirtualArg::Reg { local } => (local, rconst),
                VirtualArg::Imm | VirtualArg::Mem { index: _, base: _ } => panic!("invalid varg"),
            },
            vinstr::Arg::Imm32 { v: _ }
            | vinstr::Arg::Imm64 { v: _ }
            | vinstr::Arg::Mem {
                scale: _,
                index: _,
                base: _,
                offset: _,
            } => panic!("invalid arg"),
        }
    }

    pub fn expect_mem(
        src: (vinstr::VirtualArg, vinstr::Arg),
    ) -> (
        (Option<VirtualReg>, Option<VirtualReg>),
        (MemScale, RegConstraint, RegConstraint, Imm32),
    ) {
        let (varg, arg) = src;

        match arg {
            vinstr::Arg::Mem {
                scale,
                index,
                base,
                offset,
            } => match varg {
                VirtualArg::Mem {
                    index: v_index,
                    base: v_base,
                } => ((v_index, v_base), (scale, index, base, offset)),
                VirtualArg::Imm | VirtualArg::Reg { local: _ } => panic!("invalid varg"),
            },
            vinstr::Arg::Imm32 { v: _ }
            | vinstr::Arg::Imm64 { v: _ }
            | vinstr::Arg::Reg {
                rconst: _,
                sconst: _,
            } => panic!("invalid arg"),
        }
    }

    pub fn free(&mut self, reg: Reg) {
        self.usage.free(reg);
    }

    pub fn build(&mut self, v_body: vbuild::BodyOutput, types: &anf::Types) -> InstrBlock {
        // step 1 - register alloc base assembly

        self.sp = -8;
        self.v_map.clear();
        self.v_map.reserve_exact(v_body.valc as usize);
        self.v_map.fill(VLocation::None);

        for elem in v_body.instrs.iter() {
            match elem {
                VInstrElement::VInstr(vinstr) => {
                    let forbidden = &vinstr.clobber;
                    self.evict_regs(forbidden, &Vec::new());
                    match vinstr.kind {
                        VInstrKind::MOV_RI => {
                            Self::expect_format(vinstr, true, 1);

                            let dst = Self::expect_dest(vinstr.dest);
                            let (dst_v_reg, dst_rconst) = Self::expect_reg(dst);
                            let src = Self::expect_imm64(vinstr.srcs[0]);
                            let dst_reg =
                                self.ensure_reg(dst_v_reg, dst_rconst, &Vec::new(), forbidden);

                            self.instrs.puti(Instr::MOV_RI { dst: dst_reg, src });
                        }
                        VInstrKind::MOV_RR => {
                            Self::expect_format(vinstr, true, 1);

                            let dst = Self::expect_dest(vinstr.dest);
                            let (dst_v_reg, dst_rconst) = Self::expect_reg(dst);
                            let (src_v_reg, src_rconst) = Self::expect_reg(vinstr.srcs[0]);

                            let src_reg =
                                self.ensure_reg(src_v_reg, src_rconst, &Vec::new(), forbidden);
                            let dst_reg =
                                self.ensure_reg(dst_v_reg, dst_rconst, &vec![src_reg], forbidden);

                            self.instrs.puti(Instr::MOV_RR {
                                dst: dst_reg,
                                src: src_reg,
                            });
                        }
                        VInstrKind::MOV_RM => {
                            Self::expect_format(vinstr, true, 1);

                            let dst = Self::expect_dest(vinstr.dest);
                            let (dst_v_reg, dst_rconst) = Self::expect_reg(dst);
                            let (
                                (src_v_index, src_v_base),
                                (src_scale, src_index_rconst, src_base_rconst, src_offset),
                            ) = Self::expect_mem(vinstr.srcs[0]);

                            let save = &mut Vec::new();

                            let index_reg = match src_v_index {
                                Some(v_reg) => {
                                    let reg =
                                        self.ensure_reg(v_reg, src_index_rconst, save, forbidden);
                                    save.push(reg);
                                    Some(reg)
                                }
                                None => None,
                            };

                            let base_reg = match src_v_base {
                                Some(v_reg) => {
                                    let reg =
                                        self.ensure_reg(v_reg, src_base_rconst, save, forbidden);
                                    save.push(reg);
                                    Some(reg)
                                }
                                None => None,
                            };

                            let dst_reg = self.ensure_reg(dst_v_reg, dst_rconst, save, forbidden);

                            self.instrs.puti(Instr::MOV_RM {
                                dst: dst_reg,
                                scale: src_scale,
                                index: index_reg,
                                base: base_reg,
                                offset: src_offset,
                            });
                        }
                        VInstrKind::MOV_MR => {
                            Self::expect_format(vinstr, true, 1);

                            let dst = Self::expect_dest(vinstr.dest);
                            let (
                                (dst_v_index, dst_v_base),
                                (dst_scale, dst_index_rconst, dst_base_rconst, dst_offset),
                            ) = Self::expect_mem(vinstr.srcs[0]);
                            let (src_v_reg, src_v_rconst) = Self::expect_reg(vinstr.srcs[0]);

                            let save = &mut Vec::new();

                            let src_reg = self.ensure_reg(src_v_reg, src_v_rconst, save, forbidden);
                            save.push(src_reg);

                            let index_reg = match dst_v_index {
                                Some(v_reg) => {
                                    let reg =
                                        self.ensure_reg(v_reg, dst_index_rconst, save, forbidden);
                                    save.push(reg);
                                    Some(reg)
                                }
                                None => None,
                            };

                            let base_reg = match dst_v_base {
                                Some(v_reg) => {
                                    let reg =
                                        self.ensure_reg(v_reg, dst_base_rconst, save, forbidden);
                                    Some(reg)
                                }
                                None => None,
                            };

                            self.instrs.puti(Instr::MOV_MR {
                                scale: dst_scale,
                                index: index_reg,
                                base: base_reg,
                                offset: dst_offset,
                                src: src_reg,
                            });
                        }
                        VInstrKind::NEG_R => {
                            Self::expect_format(vinstr, false, 1);

                            let (src_v_reg, src_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let src_reg =
                                self.ensure_reg(src_v_reg, src_rconst, &Vec::new(), forbidden);

                            self.instrs.puti(Instr::NEG_R { dst: src_reg });
                        }
                        VInstrKind::ADD_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::ADD_RR {
                                dst: lhs_reg,
                                rhs: rhs_reg,
                            });
                        }
                        VInstrKind::SUB_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::SUB_RR {
                                dst: lhs_reg,
                                rhs: rhs_reg,
                            });
                        }
                        VInstrKind::IMUL_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::IMUL_RR {
                                dst: lhs_reg,
                                rhs: rhs_reg,
                            });
                        }
                        VInstrKind::IDIV_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::IDIV_RAX_R { rhs: rhs_reg });
                        }
                        VInstrKind::IMOD_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::IMOD_RDX_R { rhs: rhs_reg });
                        }
                        VInstrKind::NOT_R => {
                            Self::expect_format(vinstr, false, 1);

                            let (src_v_reg, src_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let src_reg =
                                self.ensure_reg(src_v_reg, src_rconst, &Vec::new(), forbidden);

                            self.instrs.puti(Instr::NOT_R { dst: src_reg });
                        }
                        VInstrKind::AND_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::AND_RR {
                                dst: lhs_reg,
                                rhs: rhs_reg,
                            });
                        }
                        VInstrKind::OR_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::OR_RR {
                                dst: lhs_reg,
                                rhs: rhs_reg,
                            });
                        }
                        VInstrKind::XOR_RI => {
                            Self::expect_format(vinstr, false, 2);

                            let (src_v_reg, src_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let src_reg =
                                self.ensure_reg(src_v_reg, src_rconst, &Vec::new(), forbidden);
                            let rhs = Self::expect_imm32(vinstr.srcs[1]);

                            self.instrs.puti(Instr::XOR_RI { dst: src_reg, rhs });
                        }
                        VInstrKind::XOR_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);
                            save.push(lhs_reg);
                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::XOR_RR {
                                dst: lhs_reg,
                                rhs: rhs_reg,
                            });
                        }
                        VInstrKind::SHL_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);
                            save.push(rhs_reg);
                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::SHL_R_RCX { dst: lhs_reg });
                        }
                        VInstrKind::SHR_RR => {
                            Self::expect_format(vinstr, false, 2);

                            let save = &mut Vec::new();

                            let (rhs_v_reg, rhs_rconst) = Self::expect_reg(vinstr.srcs[1]);
                            let rhs_reg = self.ensure_reg(rhs_v_reg, rhs_rconst, save, forbidden);
                            save.push(rhs_reg);
                            let (lhs_v_reg, lhs_rconst) = Self::expect_reg(vinstr.srcs[0]);
                            let lhs_reg = self.ensure_reg(lhs_v_reg, lhs_rconst, save, forbidden);

                            self.instrs.puti(Instr::SHR_R_RCX { dst: lhs_reg });
                        }
                        VInstrKind::TEST_RI => {
                            Self::expect_format(vinstr, false, 2);
                            todo!()
                        }
                        VInstrKind::TEST_RR => {
                            Self::expect_format(vinstr, false, 2);
                            todo!()
                        }
                        VInstrKind::CMP_RR => {
                            Self::expect_format(vinstr, false, 2);
                            todo!()
                        }
                        VInstrKind::SETE_R => {
                            Self::expect_format(vinstr, true, 0);
                            todo!()
                        }
                        VInstrKind::SETNE_R => {
                            Self::expect_format(vinstr, true, 0);
                            todo!()
                        }
                        VInstrKind::SETG_R => {
                            Self::expect_format(vinstr, true, 0);
                            todo!()
                        }
                        VInstrKind::SETGE_R => {
                            Self::expect_format(vinstr, true, 0);
                            todo!()
                        }
                        VInstrKind::SETL_R => {
                            Self::expect_format(vinstr, true, 0);
                            todo!()
                        }
                        VInstrKind::SETLE_R => {
                            Self::expect_format(vinstr, true, 0);
                            todo!()
                        }
                        VInstrKind::JMP_I => todo!(),
                        VInstrKind::JMP_R => todo!(),
                        VInstrKind::JMP_M => todo!(),
                        VInstrKind::JE_I => todo!(),
                        VInstrKind::JNE_I => todo!(),
                        VInstrKind::JG_I => todo!(),
                        VInstrKind::JGE_I => todo!(),
                        VInstrKind::JL_I => todo!(),
                        VInstrKind::JLE_I => todo!(),
                        VInstrKind::PUSH => todo!(),
                        VInstrKind::POP => todo!(),
                    };
                }
                VInstrElement::Label(v) => self.instrs.put(InstrElement::Label(v.clone())),
                VInstrElement::DropVReg(virtual_reg) => {
                    let loc = virtual_reg.usize();
                    match self.v_map[loc] {
                        VLocation::None => {}
                        VLocation::Stack(_) => self.v_map[loc] = VLocation::None,
                        VLocation::Reg(reg) => self.free(reg),
                    };
                }
                VInstrElement::VCall(vcall) => todo!(),
                VInstrElement::VRet(vret) => todo!(),
                VInstrElement::LoopBegin(comptime_val, virtual_regs) => todo!(),
                VInstrElement::LoopEnd(comptime_val) => todo!(),
                VInstrElement::LoopExit(comptime_val) => todo!(),
            }
        }

        // step 2 - save registers based where required based on function calls

        // step 3 - save clobbered registers

        // block
        todo!()
    }
}
