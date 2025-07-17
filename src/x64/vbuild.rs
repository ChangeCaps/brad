use crate::anf;

use super::{
    common::{Imm32, Imm64, LocalId, MemScale, VLocals, VRegister},
    program::ProgramBuilder,
    vinstr::{VInstr, VInstrBlock, VirtualArg},
};

#[derive(Debug, Clone)]
pub struct BodyBuilder {
    instrs: VInstrBlock,
    locals: VLocals,
    vreg_count: u32,
    label_count: u32,
    loop_count: u32,
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
    pub id: LocalId,
    // both const and mut
    // const may get pushed onto stack to make room
    // needs to be placed back in register at end of loop
    pub vars: Vec<VRegister>,
}

#[derive(Debug, Clone)]
pub struct BodyOutput {
    pub instrs: VInstrBlock,
    pub locals: VLocals,
    pub loops: Vec<LoopInfo>,
    pub vreg_count: u32,
    pub label_count: u32,
    pub loop_count: u32,
}

impl BodyOutput {
    pub fn new() -> Self {
        Self {
            instrs: VInstrBlock::new(),
            locals: VLocals::new(),
            loops: Vec::new(),
            vreg_count: 0,
            label_count: 0,
            loop_count: 0,
        }
    }
}

impl BodyBuilder {
    pub fn new() -> Self {
        Self {
            instrs: VInstrBlock::new(),
            locals: VLocals::new(),
            vreg_count: 0,
            label_count: 0,
            loop_count: 0,
        }
    }

    pub fn build(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
    ) -> BodyOutput {
        self.instrs.clear();
        self.vreg_count = a_body.locals.len() as u32;

        for expr in &a_body.exprs {
            match &expr.kind {
                anf::ExprKind::Unary { op, dst, val } => {
                    self.build_unary(program, a_body, a_types, op, dst, val)
                }
                anf::ExprKind::Binary { op, dst, lhs, rhs } => {
                    self.build_binary(program, a_body, a_types, op, dst, lhs, rhs)
                }
                anf::ExprKind::Match {
                    dst,
                    target,
                    arms,
                    default,
                } => self.build_match(program, a_body, a_types, dst, target, arms, default),
                anf::ExprKind::TagInit { dst, tag } => {
                    self.build_tag_init(program, a_body, a_types, dst, tag)
                }
                anf::ExprKind::TupleInit { dst, vals } => {
                    self.build_tuple_init(program, a_body, a_types, dst, vals)
                }
                anf::ExprKind::ArrayInit { dst, vals } => {
                    self.build_array_init(program, a_body, a_types, dst, vals)
                }
                anf::ExprKind::RecordInit { dst, vals } => {
                    self.build_record_init(program, a_body, a_types, dst, vals)
                }
                anf::ExprKind::UnionInit { dst, val, union } => {
                    self.build_union_init(program, a_body, a_types, dst, val, union)
                }
                anf::ExprKind::Closure { dst, func } => todo!(),
                anf::ExprKind::Call { dst, src, arg } => todo!(),
                anf::ExprKind::Mov { dst, src } => todo!(),
                anf::ExprKind::Read { dst, src, access } => todo!(),
                anf::ExprKind::Write { dst, access, src } => todo!(),
                anf::ExprKind::ReadIndex {
                    dst,
                    src,
                    index,
                    access,
                } => todo!(),
                anf::ExprKind::WriteIndex {
                    dst,
                    index,
                    access,
                    src,
                } => todo!(),
                anf::ExprKind::Loop { dst, body } => todo!(),
                anf::ExprKind::Continue {} => todo!(),
                anf::ExprKind::Break { value } => todo!(),
                anf::ExprKind::Return { val } => todo!(),
            }
        }

        self.build_output()
    }

    fn build_output(&self) -> BodyOutput {
        todo!()
    }

    fn build_unary(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        op: &anf::UnaryOp,
        dst: &anf::Local,
        val: &anf::Value,
    ) {
        todo!()
    }

    fn build_binary(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        op: &anf::BinaryOp,
        dst: &anf::Local,
        lhs: &anf::Value,
        rhs: &anf::Value,
    ) {
        todo!()
    }

    fn build_match(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        dst: &anf::Local,
        target: &anf::Value,
        arms: &Vec<(solve::Tag, anf::Arm)>,
        default: &Option<Box<anf::Arm>>,
    ) {
        todo!()
    }

    fn build_tag_init(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        dst: &anf::Local,
        tag: &solve::Tag,
    ) {
        todo!()
    }

    fn build_tuple_init(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        dst: &anf::Local,
        vals: &Vec<anf::Value>,
    ) {
        todo!()
    }

    fn build_array_init(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        dst: &anf::Local,
        vals: &Vec<anf::Value>,
    ) {
        todo!()
    }

    fn build_record_init(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        dst: &anf::Local,
        fields: &Vec<(&'static str, anf::Value)>,
    ) {
        todo!()
    }

    fn build_union_init(
        &mut self,
        program: &mut ProgramBuilder,
        a_body: &anf::Body,
        a_types: &anf::Types,
        dst: &anf::Local,
        val: &anf::Value,
        tags: &solve::Tags,
    ) {
        todo!()
    }
}

// utils
impl BodyBuilder {
    fn insert_local_call(&mut self) {
        todo!()
    }

    fn insert_extern_call(&mut self) {
        todo!()
    }
}

// vasm instructions
impl BodyBuilder {
    pub fn mov_ri(&mut self, dst: VirtualArg, src: Imm64) {
        self.instrs.put(VInstr::mov_ri(dst, src));
    }

    pub fn mov_rr(&mut self, dst: VirtualArg, src: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, src));
    }

    pub fn mov_rm(&mut self, dst: VirtualArg, mem: VirtualArg, scale: MemScale, offset: Imm32) {
        self.instrs.put(VInstr::mov_rm(dst, mem, scale, offset));
    }

    pub fn mov_mr(&mut self, mem: VirtualArg, scale: MemScale, offset: Imm32, src: VirtualArg) {
        self.instrs.put(VInstr::mov_mr(mem, scale, offset, src));
    }

    pub fn neg(&mut self, dst: VirtualArg, src: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, src));
        self.instrs.put(VInstr::neg_r(dst));
    }

    pub fn add(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::add_rr(dst, rhs));
    }

    pub fn sub(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::sub_rr(dst, rhs));
    }

    pub fn imul(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::imul_rr(dst, rhs));
    }

    pub fn idiv(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::idiv_rr(dst, rhs));
    }

    pub fn imod(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::imod_rr(dst, lhs, rhs));
    }

    pub fn not(&mut self, dst: VirtualArg, src: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, src));
        self.instrs.put(VInstr::not_r(dst));
    }

    pub fn and(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::and_rr(dst, rhs));
    }

    pub fn or(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::or_rr(dst, rhs));
    }

    pub fn xor_rr(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::xor_rr(dst, rhs));
    }

    pub fn xor_ri(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: Imm32) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::xor_ri(dst, rhs));
    }

    pub fn shl(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::shl_rr(dst, rhs));
    }

    pub fn shr(&mut self, dst: VirtualArg, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::mov_rr(dst, lhs));
        self.instrs.put(VInstr::shr_rr(dst, rhs));
    }

    pub fn cmp(&mut self, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::cmp_rr(lhs, rhs));
    }

    pub fn test_ri(&mut self, lhs: VirtualArg, rhs: Imm32) {
        self.instrs.put(VInstr::test_ri(lhs, rhs));
    }

    pub fn test_rr(&mut self, lhs: VirtualArg, rhs: VirtualArg) {
        self.instrs.put(VInstr::test_rr(lhs, rhs));
    }

    pub fn sete(&mut self, dst: VirtualArg) {
        self.instrs.put(VInstr::sete_r(dst));
    }

    pub fn setne(&mut self, dst: VirtualArg) {
        self.instrs.put(VInstr::setne_r(dst));
    }

    pub fn setg(&mut self, dst: VirtualArg) {
        self.instrs.put(VInstr::setg_r(dst));
    }

    pub fn setge(&mut self, dst: VirtualArg) {
        self.instrs.put(VInstr::setge_r(dst));
    }

    pub fn setl(&mut self, dst: VirtualArg) {
        self.instrs.put(VInstr::setl_r(dst));
    }

    pub fn setle(&mut self, dst: VirtualArg) {
        self.instrs.put(VInstr::setle_r(dst));
    }

    pub fn jmp(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::jmp_rel(rel));
    }

    pub fn jmp_r(&mut self, reg: VirtualArg) {
        self.instrs.put(VInstr::jmp_r(reg));
    }

    pub fn jmp_m(&mut self, mem: VirtualArg, scale: MemScale, offset: Imm32) {
        self.instrs.put(VInstr::jmp_m(mem, scale, offset));
    }

    pub fn je(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::je_rel(rel));
    }

    pub fn jne(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::jne_rel(rel));
    }

    pub fn jg(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::jg_rel(rel));
    }

    pub fn jge(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::jge_rel(rel));
    }

    pub fn jl(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::jl_rel(rel));
    }

    pub fn jle(&mut self, rel: Imm32) {
        self.instrs.put(VInstr::jle_rel(rel));
    }
}
