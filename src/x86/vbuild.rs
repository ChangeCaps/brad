use solve::Tag;

use crate::anf::{self, Local};

use super::{
    common::{Imm32, Imm64, LocalId, MemScale, PrimitiveType},
    vinstr::{VInstr, VInstrBlock, VirtualArg, VirtualReg},
};

#[derive(Debug, Clone)]
pub struct BodyBuilder {
    instrs: VInstrBlock,
    valc: u32,
    locc: u32,
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
    pub id: LocalId,
    // both const and mut
    // const may get pushed onto stack to make room
    // needs to be placed back in register at end of loop
    pub vars: Vec<VirtualReg>,
}

#[derive(Debug, Clone)]
pub struct BodyOutput {
    pub instrs: VInstrBlock,
    pub loops: Vec<LoopInfo>,
    pub valc: u32,
    pub locc: u32,
}

impl BodyOutput {
    pub fn new() -> Self {
        Self {
            instrs: VInstrBlock::new(),
            loops: Vec::new(),
            valc: 0,
            locc: 0,
        }
    }
}

impl BodyBuilder {
    pub fn new() -> Self {
        Self {
            instrs: VInstrBlock::new(),
            valc: 0,
            locc: 0,
        }
    }

    pub fn virtual_from_local(local: &anf::Local) -> VirtualArg {
        VirtualArg::Reg {
            local: VirtualReg::new(local.0 as u32),
        }
    }

    pub fn get_value_type(val: &anf::Value, body: &anf::Body, types: &anf::Types) -> PrimitiveType {
        match val {
            anf::Value::Local(local) => Self::get_local_type(local, body, types),
            anf::Value::Int(_) => PrimitiveType::Int,
            anf::Value::Float(_) => PrimitiveType::Float,
            anf::Value::String(_) => PrimitiveType::String,
        }
    }

    pub fn get_local_type(
        local: &anf::Local,
        body: &anf::Body,
        types: &anf::Types,
    ) -> PrimitiveType {
        let tid = body.locals[local.0];
        let terms = &types[tid].terms;

        if terms.len() == 2 {
            if (terms[0].tags.contains(Tag::TRUE) && terms[1].tags.contains(Tag::FALSE))
                || (terms[0].tags.contains(Tag::FALSE) && terms[1].tags.contains(Tag::TRUE))
            {
                return PrimitiveType::Bool;
            }
        };

        if terms.len() == 1 {
            if terms[0].tags.contains(Tag::INT) {
                return PrimitiveType::Int;
            }
            if terms[0].tags.contains(Tag::FLOAT) {
                return PrimitiveType::Float;
            }
            if terms[0].tags.contains(Tag::STR) {
                return PrimitiveType::String;
            }
            if terms[0].tags.contains(Tag::NONE) {
                return PrimitiveType::None;
            }
        }

        todo!()
    }

    pub fn next_vreg(&mut self) -> VirtualArg {
        let ret = VirtualArg::Reg {
            local: VirtualReg::new(self.valc),
        };
        self.valc = self.valc + 1;
        ret
    }

    pub fn build_unary(
        &mut self,
        body: &anf::Body,
        types: &anf::Types,
        op: &anf::UnaryOp,
        l_dst: &anf::Local,
        l_val: &anf::Value,
    ) {
        let p_type = Self::get_value_type(l_val, body, types);

        let dst = Self::virtual_from_local(l_dst);

        let val = match l_val {
            anf::Value::Local(local) => Self::virtual_from_local(local),
            anf::Value::Int(val) => {
                let virt = self.next_vreg();
                self.mov_ri(virt, Imm64::Const(val.clone()));
                virt
            }
            anf::Value::Float(_) => {
                todo!()
            }
            anf::Value::String(_) => {
                panic!("Attempted unary op on string");
            }
        };

        match p_type {
            PrimitiveType::Bool => match op {
                anf::UnaryOp::Neg => panic!("Negation on bool"),
                anf::UnaryOp::Not => self.xor_ri(dst, val, Imm32::Const(1)),
                anf::UnaryOp::BitNot => panic!("BitNot on bool"),
            },
            PrimitiveType::Int => match op {
                anf::UnaryOp::Neg => self.neg(dst, val),
                anf::UnaryOp::Not => todo!("not zero?"),
                anf::UnaryOp::BitNot => self.not(dst, val),
            },
            PrimitiveType::Float => match op {
                anf::UnaryOp::Neg => todo!(),
                anf::UnaryOp::Not => todo!("not zero?"),
                anf::UnaryOp::BitNot => panic!("BitNot on float"),
            },
            PrimitiveType::String => panic!("Unary op on string"),
            PrimitiveType::None => panic!("Unary op on none"),
        };
    }

    pub fn build_binary(
        &mut self,
        body: &anf::Body,
        types: &anf::Types,
        op: &anf::BinaryOp,
        l_dst: &anf::Local,
        l_lhs: &anf::Value,
        l_rhs: &anf::Value,
    ) {
        let p_type = Self::get_value_type(l_lhs, body, types);
        assert!(
            p_type == Self::get_value_type(l_rhs, body, types),
            "Incompatible types in binary expression"
        );

        let dst = Self::virtual_from_local(l_dst);

        let lhs = match l_lhs {
            anf::Value::Local(local) => Self::virtual_from_local(local),
            anf::Value::Int(val) => {
                let virt = self.next_vreg();
                self.mov_ri(virt, Imm64::Const(val.clone()));
                virt
            }
            anf::Value::Float(_) => {
                todo!()
            }
            anf::Value::String(_) => {
                panic!("Attempted binary op on string");
            }
        };
        let rhs = match l_rhs {
            anf::Value::Local(local) => Self::virtual_from_local(local),
            anf::Value::Int(val) => {
                let virt = self.next_vreg();
                self.mov_ri(virt, Imm64::Const(val.clone()));
                virt
            }
            anf::Value::Float(_) => {
                todo!()
            }
            anf::Value::String(_) => {
                panic!("Attempted binary op on string");
            }
        };

        match p_type {
            PrimitiveType::Bool => match op {
                anf::BinaryOp::Add => panic!("Invalid operation for bool"),
                anf::BinaryOp::Sub => panic!("Invalid operation for bool"),
                anf::BinaryOp::Mul => panic!("Invalid operation for bool"),
                anf::BinaryOp::Div => panic!("Invalid operation for bool"),
                anf::BinaryOp::Mod => panic!("Invalid operation for bool"),
                anf::BinaryOp::And => panic!("Invalid operation for bool"),
                anf::BinaryOp::Or => panic!("Invalid operation for bool"),
                anf::BinaryOp::BitAnd => panic!("Invalid operation for bool"),
                anf::BinaryOp::BitOr => panic!("Invalid operation for bool"),
                anf::BinaryOp::BitXor => panic!("Invalid operation for bool"),
                anf::BinaryOp::Shl => panic!("Invalid operation for bool"),
                anf::BinaryOp::Shr => panic!("Invalid operation for bool"),
                anf::BinaryOp::Eq => {
                    self.xor_rr(dst, lhs, rhs);
                    self.xor_ri(dst, dst, Imm32::Const(1));
                }
                anf::BinaryOp::Ne => self.xor_rr(dst, lhs, rhs),
                anf::BinaryOp::Lt => panic!("Invalid operation for bool"),
                anf::BinaryOp::Le => panic!("Invalid operation for bool"),
                anf::BinaryOp::Gt => panic!("Invalid operation for bool"),
                anf::BinaryOp::Ge => panic!("Invalid operation for bool"),
            },
            PrimitiveType::Int => match op {
                anf::BinaryOp::Add => self.add(dst, lhs, rhs),
                anf::BinaryOp::Sub => self.sub(dst, lhs, rhs),
                anf::BinaryOp::Mul => self.imul(dst, lhs, rhs),
                anf::BinaryOp::Div => self.idiv(dst, lhs, rhs),
                anf::BinaryOp::Mod => self.imod(dst, lhs, rhs),
                anf::BinaryOp::And => {
                    todo!("not zero?")
                }
                anf::BinaryOp::Or => {
                    todo!("not zero?")
                }
                anf::BinaryOp::BitAnd => self.and(dst, lhs, rhs),
                anf::BinaryOp::BitOr => self.or(dst, lhs, rhs),
                anf::BinaryOp::BitXor => self.xor_rr(dst, lhs, rhs),
                anf::BinaryOp::Shl => self.shl(dst, lhs, rhs),
                anf::BinaryOp::Shr => self.shr(dst, lhs, rhs),
                anf::BinaryOp::Eq => {
                    self.cmp(lhs, rhs);
                    self.sete(dst);
                }
                anf::BinaryOp::Ne => {
                    self.cmp(lhs, rhs);
                    self.setne(dst);
                }
                anf::BinaryOp::Lt => {
                    self.cmp(lhs, rhs);
                    self.setl(dst);
                }
                anf::BinaryOp::Le => {
                    self.cmp(lhs, rhs);
                    self.setle(dst);
                }
                anf::BinaryOp::Gt => {
                    self.cmp(lhs, rhs);
                    self.setg(dst);
                }
                anf::BinaryOp::Ge => {
                    self.cmp(lhs, rhs);
                    self.setge(dst);
                }
            },
            PrimitiveType::Float => match op {
                anf::BinaryOp::Add => todo!(),
                anf::BinaryOp::Sub => todo!(),
                anf::BinaryOp::Mul => todo!(),
                anf::BinaryOp::Div => todo!(),
                anf::BinaryOp::Mod => todo!(),
                anf::BinaryOp::And => todo!(),
                anf::BinaryOp::Or => todo!(),
                anf::BinaryOp::BitAnd => todo!(),
                anf::BinaryOp::BitOr => todo!(),
                anf::BinaryOp::BitXor => todo!(),
                anf::BinaryOp::Shl => todo!(),
                anf::BinaryOp::Shr => todo!(),
                anf::BinaryOp::Eq => todo!(),
                anf::BinaryOp::Ne => todo!(),
                anf::BinaryOp::Lt => todo!(),
                anf::BinaryOp::Le => todo!(),
                anf::BinaryOp::Gt => todo!(),
                anf::BinaryOp::Ge => todo!(),
            },
            PrimitiveType::String => panic!("Unary op on string"),
            PrimitiveType::None => panic!("Unary op on none"),
        }
    }

    pub fn build_match(
        &mut self,
        body: &anf::Body,
        types: &anf::Types,
        dst: &Local,
        target: &anf::Value,
        arms: &Vec<(Tag, anf::Arm)>,
        default: &Option<Box<anf::Arm>>,
    ) {
        todo!()
    }

    pub fn build(&mut self, body: &anf::Body, types: &anf::Types) -> BodyOutput {
        self.instrs.clear();
        self.valc = body.locals.len() as u32;

        for expr in &body.exprs {
            match &expr.kind {
                anf::ExprKind::Unary { op, dst, val } => {
                    self.build_unary(body, types, op, dst, val);
                }
                anf::ExprKind::Binary { op, dst, lhs, rhs } => {
                    self.build_binary(body, types, op, dst, lhs, rhs);
                }
                anf::ExprKind::Match {
                    dst,
                    target,
                    arms,
                    default,
                } => self.build_match(body, types, dst, target, arms, default),
                anf::ExprKind::TagInit { dst, tag } => todo!(),
                anf::ExprKind::TupleInit { dst, vals } => todo!(),
                anf::ExprKind::ArrayInit { dst, vals } => todo!(),
                anf::ExprKind::RecordInit { dst, vals } => todo!(),
                anf::ExprKind::UnionInit { dst, val, union } => todo!(),
                anf::ExprKind::Closure { dst, func } => todo!(),
                anf::ExprKind::Call { dst, src, arg } => todo!(),
                anf::ExprKind::Mov { dst, src } => {
                    let v_dst = VirtualArg::Mem {
                        index: None,
                        base: Some(VirtualReg::new(dst.0 as u32)),
                    };
                    let v_src = match src {
                        anf::Value::Local(local) => Self::virtual_from_local(local),
                        anf::Value::Int(val) => {
                            let virt = self.next_vreg();
                            self.mov_ri(virt, Imm64::Const(val.clone()));
                            virt
                        }
                        anf::Value::Float(_) => {
                            todo!()
                        }
                        anf::Value::String(_) => {
                            todo!()
                        }
                    };

                    self.mov_rr(v_dst, v_src);
                }
                anf::ExprKind::Read { dst, src, access } => {
                    let offset = if access.len() > 0 {
                        // calculate field offset
                        todo!()
                    } else {
                        0
                    };

                    let v_dst = Self::virtual_from_local(dst);
                    let v_src = VirtualArg::Mem {
                        index: None,
                        base: Some(VirtualReg::new(src.0 as u32)),
                    };

                    self.mov_rm(v_dst, v_src, MemScale::S1, Imm32::Const(offset));
                }
                anf::ExprKind::Write { dst, access, src } => {
                    let offset = if access.len() > 0 {
                        // calculate field offset
                        todo!()
                    } else {
                        0
                    };

                    let v_dst = Self::virtual_from_local(dst);

                    let v_src = match src {
                        anf::Value::Local(local) => Self::virtual_from_local(local),
                        anf::Value::Int(val) => {
                            let virt = self.next_vreg();
                            self.mov_ri(virt, Imm64::Const(val.clone()));
                            virt
                        }
                        anf::Value::Float(_) => {
                            todo!()
                        }
                        anf::Value::String(_) => {
                            panic!("Attempted to move a string");
                        }
                    };

                    self.mov_mr(v_dst, MemScale::S1, Imm32::Const(offset), v_src);
                }
                anf::ExprKind::ReadIndex {
                    dst,
                    src,
                    index,
                    access,
                } => {
                    let _offset = if access.len() > 0 {
                        // calculate field offset
                        todo!()
                    } else {
                        0
                    };

                    let _v_dst = Self::virtual_from_local(dst);

                    let _v_index = match index {
                        anf::Value::Local(local) => Self::virtual_from_local(local),
                        anf::Value::Int(val) => {
                            let virt = self.next_vreg();
                            self.mov_ri(virt, Imm64::Const(val.clone()));
                            virt
                        }
                        anf::Value::Float(_) => {
                            panic!("Attempted to index with a float")
                        }
                        anf::Value::String(_) => {
                            panic!("Attempted to index with a string");
                        }
                    };

                    // calculate element size
                    todo!()
                }
                anf::ExprKind::WriteIndex {
                    dst,
                    index,
                    access,
                    src,
                } => {
                    let _offset = if access.len() > 0 {
                        // calculate field offset
                        todo!()
                    } else {
                        0
                    };

                    let _v_dst = Self::virtual_from_local(dst);

                    let _v_index = match index {
                        anf::Value::Local(local) => Self::virtual_from_local(local),
                        anf::Value::Int(val) => {
                            let virt = self.next_vreg();
                            self.mov_ri(virt, Imm64::Const(val.clone()));
                            virt
                        }
                        anf::Value::Float(_) => {
                            panic!("Attempted to index with a float")
                        }
                        anf::Value::String(_) => {
                            panic!("Attempted to index with a string");
                        }
                    };

                    // calculate element size
                    todo!()
                }
                anf::ExprKind::Return { val } => todo!(),
                anf::ExprKind::Loop { dst, body } => todo!(),
                anf::ExprKind::Continue {} => todo!(),
                anf::ExprKind::Break { value } => todo!(),
            };
        }

        let out = BodyOutput::new();

        todo!();

        out
    }

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
