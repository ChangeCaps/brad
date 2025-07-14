use crate::anf;

use super::{
    common::{Imm32, Imm64, MemScale},
    ibuild,
    instr::{Instr, InstrElement},
    reg::Reg,
    vbuild,
};

pub struct Builder<W: std::io::Write> {
    anf: anf::Program,
    writer: W,
}

impl<W: std::io::Write> Builder<W> {
    pub fn new(anf: anf::Program, writer: W) -> Self {
        Self { anf, writer }
    }

    pub fn build(&mut self) {
        let mut vbbuilder = vbuild::BodyBuilder::new();
        let mut ibbuilder = ibuild::BodyBuilder::new();

        let mut bodies: Vec<(anf::Bid, ibuild::BodyOutput)> = Vec::new();

        for (bid, body) in self.anf.bodies.iter() {
            if body.is_extern {
                continue;
            }

            let vbody = vbbuilder.build(body, &self.anf.types);
            let ibody = ibbuilder.build(vbody, &self.anf.types);

            bodies.push((bid, ibody));
        }

        self.build_asm(&bodies);
    }

    fn write(&mut self, buf: &[u8]) {
        let _ = self.writer.write(buf);
    }

    fn build_asm(&mut self, bodies: &Vec<(anf::Bid, ibuild::BodyOutput)>) {
        for (bid, body) in bodies.iter() {
            self.write(Self::bid_name(bid.clone()).as_bytes());
            self.write(b":\n");
            for instr in body.instrs.iter() {
                self.write(b"\t");
                match instr {
                    InstrElement::Instr(instr) => {
                        match instr.clone() {
                            Instr::MOV_RI { dst, src } => {
                                self.write(b"mov ");
                                self.write_reg(dst);
                                self.write(b", ");
                                self.write_imm64(src);
                            }
                            Instr::MOV_RR { dst, src } => {
                                self.write(b"mov ");
                                self.write_reg(dst);
                                self.write(b", ");
                                self.write_reg(src);
                            }
                            Instr::MOV_RM {
                                dst,
                                scale,
                                index,
                                base,
                                offset,
                            } => {
                                self.write(b"mov ");
                                self.write_reg(dst);
                                self.write(b", qword [");
                                match base {
                                    Some(b_reg) => match index {
                                        Some(i_reg) => {
                                            self.write_reg(b_reg);
                                            self.write(b" + ");
                                            self.write_reg(i_reg);
                                            self.write(b" * ");
                                            self.write_memscale(scale);
                                            match offset {
                                                Imm32::Const(val) => {
                                                    if val < 0 {
                                                        self.write(b" - ");
                                                        self.write(
                                                            val.abs().to_string().as_bytes(),
                                                        );
                                                    } else {
                                                        self.write(b" + ");
                                                        self.write(val.to_string().as_bytes());
                                                    }
                                                }
                                                Imm32::Comptime(_) => {
                                                    self.write(b" + ");
                                                    self.write_imm32(offset);
                                                }
                                            }
                                        }
                                        None => {
                                            self.write_reg(b_reg);
                                            match offset {
                                                Imm32::Const(val) => {
                                                    if val < 0 {
                                                        self.write(b" - ");
                                                        self.write(
                                                            val.abs().to_string().as_bytes(),
                                                        );
                                                    } else {
                                                        self.write(b" + ");
                                                        self.write(val.to_string().as_bytes());
                                                    }
                                                }
                                                Imm32::Comptime(_) => {
                                                    self.write(b" + ");
                                                    self.write_imm32(offset);
                                                }
                                            }
                                        }
                                    },
                                    None => match index {
                                        Some(i_reg) => {
                                            self.write_reg(i_reg);
                                            self.write(b" * ");
                                            self.write_memscale(scale);
                                            match offset {
                                                Imm32::Const(val) => {
                                                    if val < 0 {
                                                        self.write(b" - ");
                                                        self.write(
                                                            val.abs().to_string().as_bytes(),
                                                        );
                                                    } else {
                                                        self.write(b" + ");
                                                        self.write(val.to_string().as_bytes());
                                                    }
                                                }
                                                Imm32::Comptime(_) => {
                                                    self.write(b" + ");
                                                    self.write_imm32(offset);
                                                }
                                            }
                                        }
                                        None => {
                                            self.write_imm32(offset);
                                        }
                                    },
                                }
                                self.write(b"]");
                            }
                            Instr::MOV_MR {
                                scale,
                                index,
                                base,
                                offset,
                                src,
                            } => {
                                self.write(b"mov qword [");
                                match base {
                                    Some(b_reg) => match index {
                                        Some(i_reg) => {
                                            self.write_reg(b_reg);
                                            self.write(b" + ");
                                            self.write_reg(i_reg);
                                            self.write(b" * ");
                                            self.write_memscale(scale);
                                            match offset {
                                                Imm32::Const(val) => {
                                                    if val < 0 {
                                                        self.write(b" - ");
                                                        self.write(
                                                            val.abs().to_string().as_bytes(),
                                                        );
                                                    } else {
                                                        self.write(b" + ");
                                                        self.write(val.to_string().as_bytes());
                                                    }
                                                }
                                                Imm32::Comptime(_) => {
                                                    self.write(b" + ");
                                                    self.write_imm32(offset);
                                                }
                                            }
                                        }
                                        None => {
                                            self.write_reg(b_reg);
                                            match offset {
                                                Imm32::Const(val) => {
                                                    if val < 0 {
                                                        self.write(b" - ");
                                                        self.write(
                                                            val.abs().to_string().as_bytes(),
                                                        );
                                                    } else {
                                                        self.write(b" + ");
                                                        self.write(val.to_string().as_bytes());
                                                    }
                                                }
                                                Imm32::Comptime(_) => {
                                                    self.write(b" + ");
                                                    self.write_imm32(offset);
                                                }
                                            }
                                        }
                                    },
                                    None => match index {
                                        Some(i_reg) => {
                                            self.write_reg(i_reg);
                                            self.write(b" * ");
                                            self.write_memscale(scale);
                                            match offset {
                                                Imm32::Const(val) => {
                                                    if val < 0 {
                                                        self.write(b" - ");
                                                        self.write(
                                                            val.abs().to_string().as_bytes(),
                                                        );
                                                    } else {
                                                        self.write(b" + ");
                                                        self.write(val.to_string().as_bytes());
                                                    }
                                                }
                                                Imm32::Comptime(_) => {
                                                    self.write(b" + ");
                                                    self.write_imm32(offset);
                                                }
                                            }
                                        }
                                        None => {
                                            self.write_imm32(offset);
                                        }
                                    },
                                }
                                self.write(b"], ");
                                self.write_reg(src);
                            }
                            Instr::NEG_R { dst } => {
                                self.write(b"neg ");
                                self.write_reg(dst);
                            }
                            Instr::ADD_RI { dst, rhs } => {
                                self.write(b"add ");
                                self.write_reg(dst);
                                self.write(b", ");
                                self.write_imm32(rhs);
                            }
                            Instr::ADD_RR { dst, rhs } => {
                                self.write(b"add ");
                                self.write_reg(dst);
                                self.write(b", ");
                                self.write_reg(rhs);
                            }
                            Instr::SUB_RI { dst, rhs } => {
                                self.write(b"sub ");
                                self.write_reg(dst);
                                self.write(b", ");
                                self.write_imm32(rhs);
                            }
                            Instr::SUB_RR { dst, rhs } => {
                                self.write(b"sub ");
                                self.write_reg(dst);
                                self.write(b", ");
                                self.write_reg(rhs);
                            }
                            Instr::IMUL_RR { dst, rhs } => todo!(),
                            Instr::IDIV_RAX_R { rhs } => todo!(),
                            Instr::IMOD_RDX_R { rhs } => todo!(),
                            Instr::NOT_R { dst } => todo!(),
                            Instr::AND_RR { dst, rhs } => todo!(),
                            Instr::OR_RR { dst, rhs } => todo!(),
                            Instr::XOR_RI { dst, rhs } => todo!(),
                            Instr::XOR_RR { dst, rhs } => todo!(),
                            Instr::SHL_R_RCX { dst } => todo!(),
                            Instr::SHR_R_RCX { dst } => todo!(),
                            Instr::TEST { lhs, rhs } => todo!(),
                            Instr::CMP { lhs, rhs } => todo!(),
                            Instr::SETE_R { dst } => todo!(),
                            Instr::SETNE_R { dst } => todo!(),
                            Instr::SETG { dst } => todo!(),
                            Instr::SETGE { dst } => todo!(),
                            Instr::SETL { dst } => todo!(),
                            Instr::SETLE { dst } => todo!(),
                            Instr::CALL_M {} => todo!(),
                            Instr::RET => {
                                self.write(b"ret");
                            }
                        };
                    }
                    InstrElement::Label(comptime_val) => {
                        todo!()
                    }
                };
                self.write(b"\n");
            }
        }

        match self.writer.flush() {
            Ok(_) => {}
            Err(_) => panic!(),
        };
    }

    fn bid_name(bid: anf::Bid) -> String {
        if bid == anf::Bid(0) {
            String::from("brad_main")
        } else {
            format!("func_{}", bid.0)
        }
    }

    fn write_imm32(&mut self, imm: Imm32) {
        match imm {
            Imm32::Const(val) => {
                self.write(val.to_string().as_bytes());
            }
            Imm32::Comptime(comptime_val) => todo!(),
        }
    }

    fn write_imm64(&mut self, imm: Imm64) {
        match imm {
            Imm64::Const(val) => {
                self.write(val.to_string().as_bytes());
            }
            Imm64::Comptime(comptime_val) => todo!(),
        }
    }

    fn write_memscale(&mut self, scale: MemScale) {
        match scale {
            MemScale::S1 => {
                self.write(b"0x1");
            }
            MemScale::S2 => {
                self.write(b"0x2");
            }
            MemScale::S4 => {
                self.write(b"0x4");
            }
            MemScale::S8 => {
                self.write(b"0x8");
            }
        }
    }

    fn write_reg(&mut self, reg: Reg) {
        match reg {
            Reg::RAX => self.write(b"RAX"),
            Reg::RBX => self.write(b"RBX"),
            Reg::RCX => self.write(b"RCX"),
            Reg::RDX => self.write(b"RDX"),
            Reg::RSP => self.write(b"RSP"),
            Reg::RBP => self.write(b"RBP"),
            Reg::RSI => self.write(b"RSI"),
            Reg::RDI => self.write(b"RDI"),
            Reg::R8 => self.write(b"R8"),
            Reg::R9 => self.write(b"R9"),
            Reg::R10 => self.write(b"R10"),
            Reg::R11 => self.write(b"R11"),
            Reg::R12 => self.write(b"R12"),
            Reg::R13 => self.write(b"R13"),
            Reg::R14 => self.write(b"R14"),
            Reg::R15 => self.write(b"R15"),
            Reg::XMM0 => self.write(b"XMM0"),
            Reg::XMM1 => self.write(b"XMM1"),
            Reg::XMM2 => self.write(b"XMM2"),
            Reg::XMM3 => self.write(b"XMM3"),
            Reg::XMM4 => self.write(b"XMM4"),
            Reg::XMM5 => self.write(b"XMM5"),
            Reg::XMM6 => self.write(b"XMM6"),
            Reg::XMM7 => self.write(b"XMM7"),
            Reg::XMM8 => self.write(b"XMM8"),
            Reg::XMM9 => self.write(b"XMM9"),
            Reg::XMM10 => self.write(b"XMM10"),
            Reg::XMM11 => self.write(b"XMM11"),
            Reg::XMM12 => self.write(b"XMM12"),
            Reg::XMM13 => self.write(b"XMM13"),
            Reg::XMM14 => self.write(b"XMM14"),
            Reg::XMM15 => self.write(b"XMM15"),
            _ => panic!(),
        };
    }
}
