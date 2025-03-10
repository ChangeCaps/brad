use std::collections::HashMap;

use crate::{attribute::Attributes, diagnostic::Diagnostic, lir, sir};

pub fn build(
    source: &sir::Program,
    entry: sir::Bid,
) -> Result<(lir::Program, lir::Bid), Diagnostic> {
    let mut program = lir::Program {
        bodies: lir::Bodies::new(),
        types: lir::Types::new(),
    };

    let mut body_map: HashMap<sir::Bid, lir::Bid> = HashMap::new();
    let mut type_map: HashMap<sir::Ty, lir::Tid> = HashMap::new();

    let mut main = Builder::new(&mut body_map, &mut type_map, source, &mut program, entry);
    let lentry = main.build();

    println!("{:#?}", program);

    Ok((program, lentry))
}

struct Builder<'a> {
    body_map: &'a mut HashMap<sir::Bid, lir::Bid>,
    type_map: &'a mut HashMap<sir::Ty, lir::Tid>,

    sir: &'a sir::Program,
    lir: &'a mut lir::Program,

    source: sir::Bid,
    body: lir::Body,

    local_map: HashMap<sir::Local, lir::Local>,
}

impl<'a> Builder<'a> {
    fn new(
        body_map: &'a mut HashMap<sir::Bid, lir::Bid>,
        type_map: &'a mut HashMap<sir::Ty, lir::Tid>,
        sir: &'a sir::Program,
        lir: &'a mut lir::Program,
        source: sir::Bid,
    ) -> Self {
        Self {
            body_map,
            type_map,
            sir,
            lir,
            source,
            body: lir::Body {
                tid: lir::Tid(0),
                locals: Vec::new(),
                block: lir::Block::new(),
                attrs: Attributes::new(),
                is_extern: false,
                name: None,
            },
            local_map: HashMap::new(),
        }
    }

    fn build(&mut self) -> lir::Bid {
        self.build_body(self.source)
    }

    fn build_body(&mut self, bid: sir::Bid) -> lir::Bid {
        match self.body_map.get(&bid) {
            Some(id) => return id.clone(),
            None => {}
        };

        let argc = self.sir.bodies[bid].arguments;

        let id = {
            let body = lir::Body {
                tid: {
                    // get correct function type based on sir body
                    let body = &self.sir.bodies[bid];

                    let ty = lir::Ty::Func(
                        (0..argc)
                            .rev()
                            .fold(self.lir.types.get_id(lir::Ty::Empty), |acc, i| {
                                if i == body.arguments - 1 {
                                    self.map_tid(body.locals[sir::Local { 0: i }])
                                } else {
                                    let input = self.map_tid(body.locals[sir::Local { 0: i }]);
                                    self.lir.types.get_id(lir::Ty::Func(input, acc))
                                }
                            }),
                        self.map_tid(body.output),
                    );

                    self.lir.types.get_id(ty)
                },
                locals: Vec::new(),
                block: lir::Block::new(),
                attrs: self.sir.bodies[bid].attrs.clone(),
                is_extern: self.sir.bodies[bid].is_extern,
                name: self.sir.bodies[bid].name.clone(),
            };
            self.lir.bodies.push(body)
        };

        self.body_map.insert(bid, id);

        // return if extern
        if self.lir.bodies[id].is_extern {
            return id;
        }

        // push args into locals
        for i in 0..argc {
            let sid = self.sir.bodies[bid].locals[sir::Local { 0: i }];
            let lid = self.map_tid(sid);
            self.lir.bodies[id].locals.push(lid);
        }

        // if function is not extern, body should be Some
        let sblock = self.sir.bodies[bid].block.as_ref().unwrap();

        // build lir body
        let lblock = self.decompose_block(id, bid, sblock);
        self.lir.bodies[id].block = lblock;

        id
    }

    fn push_local(&mut self, bid: lir::Bid, tid: lir::Tid) -> lir::Local {
        let locals = &mut self.lir.bodies[bid].locals;
        let index = locals.len();
        locals.push(tid);
        lir::Local(index as u32)
    }

    fn read_place(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        block: &mut lir::Block,
        place: &sir::Place,
    ) -> lir::Local {
        let lplace = self.decompose_place(bid, sbid, block, place);
        self.read_lplace(bid, block, &lplace)
    }

    fn write_place(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        block: &mut lir::Block,
        place: &sir::Place,
        value: lir::Value,
    ) -> lir::Stmt {
        let lplace = self.decompose_place(bid, sbid, block, place);
        self.write_lplace(bid, block, &lplace, value)
    }

    fn read_lplace(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        place: &lir::Place,
    ) -> lir::Local {
        let dst = {
            let tid = match place.access.last() {
                Some((_, tid)) => tid.clone(),
                None => self.lir.bodies[bid][place.local],
            };
            self.push_local(bid, tid)
        };

        let tid = self.lir.bodies[bid][place.local];

        if place.deref {
            let pid = self.lir.types.get_id(lir::Ty::Ref(tid));
            let src = self.push_local(bid, pid);
            block.stmts.push(lir::Stmt::Eval {
                dst: lir::Var::from(src),
                src: lir::Value::Use(lir::Operand::Var(lir::Var::from(place.local))),
            });
            block.stmts.push(lir::Stmt::ReadRef {
                dst: lir::Var::from(dst),
                mem: lir::Var::from(src),
                access: place.access.clone(),
            });
        } else if let Some(ref index) = place.index {
            let lid = self.lir.types.get_id(lir::Ty::List(tid));
            let src = self.push_local(bid, lid);
            block.stmts.push(lir::Stmt::Eval {
                dst: lir::Var::from(src),
                src: lir::Value::Use(lir::Operand::Var(lir::Var::from(place.local))),
            });
            block.stmts.push(lir::Stmt::ReadIndex {
                dst: lir::Var::from(dst),
                array: lir::Var::from(src),
                index: index.clone(),
                access: place.access.clone(),
            });
        } else {
            block.stmts.push(lir::Stmt::Eval {
                dst: lir::Var::from(dst),
                src: lir::Value::Use(lir::Operand::Var(lir::Var {
                    local: place.local,
                    access: place.access.clone(),
                })),
            })
        }

        dst
    }

    fn write_lplace(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        place: &lir::Place,
        value: lir::Value,
    ) -> lir::Stmt {
        let dst = {
            let tid = match place.access.last() {
                Some((_, tid)) => tid.clone(),
                None => self.lir.bodies[bid][place.local],
            };
            self.push_local(bid, tid)
        };

        let tid = self.lir.bodies[bid][place.local];

        if place.deref {
            let pid = self.lir.types.get_id(lir::Ty::Ref(tid));
            let src = self.push_local(bid, pid);
            block.stmts.push(lir::Stmt::Eval {
                dst: lir::Var::from(src),
                src: value,
            });
            lir::Stmt::WriteRef {
                dst: lir::Var {
                    local: dst,
                    access: Vec::new(),
                },
                access: place.access.clone(),
                val: lir::Operand::Var(lir::Var::from(src)),
            }
        } else if let Some(ref index) = place.index {
            let lid = self.lir.types.get_id(lir::Ty::List(tid));
            let src = self.push_local(bid, lid);
            block.stmts.push(lir::Stmt::Eval {
                dst: lir::Var::from(src),
                src: value,
            });
            lir::Stmt::WriteIndex {
                dst: lir::Var {
                    local: dst,
                    access: Vec::new(),
                },
                index: index.clone(),
                access: place.access.clone(),
                val: lir::Operand::Var(lir::Var::from(src)),
            }
        } else {
            lir::Stmt::Eval {
                dst: lir::Var {
                    local: dst,
                    access: place.access.clone(),
                },
                src: value,
            }
        }
    }

    fn var_from_value(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        value: &lir::Value,
    ) -> lir::Var {
        let var = {
            let tid = self.tid_from_value(bid, value);
            let local = self.push_local(bid, tid);
            lir::Var::from(local)
        };

        block.stmts.push(lir::Stmt::Eval {
            dst: var.clone(),
            src: value.clone(),
        });

        var
    }

    fn var_from_operand(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        operand: &lir::Operand,
    ) -> lir::Var {
        match operand {
            lir::Operand::Var(var) => var.clone(),
            lir::Operand::Const(tid, cst) => {
                let local = self.push_local(bid, tid.clone());
                let var = lir::Var::from(local);
                block.push(lir::Stmt::Eval {
                    dst: var.clone(),
                    src: lir::Value::Use(lir::Operand::Const(tid.clone(), cst.clone())),
                });
                var
            }
        }
    }

    fn tid_from_value(&mut self, bid: lir::Bid, value: &lir::Value) -> lir::Tid {
        match value {
            lir::Value::Use(operand)
            | lir::Value::Binary(_, operand, _)
            | lir::Value::Unary(_, operand) => self.tid_from_operand(bid, operand),
            lir::Value::Promote {
                input: _,
                output,
                operand: _,
            } => output.clone(),
            lir::Value::Coerce {
                input: _,
                output,
                operand: _,
            } => output.clone(),
            lir::Value::Call(var, _) => {
                let pid = self.tid_from_var(bid, var);
                if let lir::Ty::Func(_, ret) = self.lir.types[pid] {
                    ret
                } else {
                    panic!("uh oh something did a fucky wucky")
                }
            }
        }
    }

    fn tid_from_operand(&mut self, bid: lir::Bid, operand: &lir::Operand) -> lir::Tid {
        match operand {
            lir::Operand::Var(lir::Var { local, access }) => match access.last() {
                Some((_, tid)) => tid.clone(),
                None => self.lir.bodies[bid][local.clone()],
            },
            lir::Operand::Const(tid, _) => tid.clone(),
        }
    }

    fn tid_from_var(&mut self, bid: lir::Bid, var: &lir::Var) -> lir::Tid {
        match var.access.last() {
            Some((_, tid)) => tid.clone(),
            None => self.lir.bodies[bid][var.local.clone()],
        }
    }

    fn decompose_operand(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        block: &mut lir::Block,
        operand: &sir::Operand,
    ) -> lir::Operand {
        match operand {
            // this is a copy
            // sir::Operand::DeepCopy(ref place) => {
            //     let src = self.read_place(bid, sbid, block, place);
            //     let tid = self.lir.bodies[bid][src];
            //     let copy = self.push_local(bid, tid);
            //     block.push(lir::Stmt::Copy {
            //         dst: lir::Var::from(copy),
            //         src: lir::Var::from(src),
            //     });
            //     lir::Operand::Var(lir::Var::from(copy))
            // }

            // this is a move
            sir::Operand::Load(ref place) => {
                lir::Operand::Var(lir::Var::from(self.read_place(bid, sbid, block, place)))
            }

            sir::Operand::Const(ref cst, tid) => lir::Operand::Const(
                self.map_tid(tid.clone()),
                match cst {
                    sir::Const::None => lir::Const::Empty,
                    sir::Const::Int(v) => lir::Const::Int(v.clone()),
                    sir::Const::Float(v) => lir::Const::Float(v.clone()),
                    sir::Const::String(v) => lir::Const::String(v),
                },
            ),
        }
    }

    fn decompose_value(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        block: &mut lir::Block,
        value: &sir::Value,
    ) -> lir::Value {
        match value {
            sir::Value::Use(ref operand) => {
                lir::Value::Use(self.decompose_operand(bid, sbid, block, operand))
            }
            sir::Value::Tuple(operands) => {
                let ty = lir::Ty::Tuple(operands.iter().fold(Vec::new(), |mut acc, op| {
                    let val = self.decompose_operand(bid, sbid, block, op);
                    acc.push(self.tid_from_operand(bid, &val));
                    acc
                }));
                let tid = self.lir.types.get_id(ty);
                let tuple = self.push_local(bid, tid);

                for (i, operand) in operands.iter().enumerate() {
                    let op = self.decompose_operand(bid, sbid, block, operand);
                    let id = self.tid_from_operand(bid, &op);
                    let src = lir::Value::Use(op);
                    block.push(lir::Stmt::Eval {
                        dst: lir::Var::from((tuple, lir::Access::Tuple(i as u32), id)),
                        src,
                    })
                }

                lir::Value::Use(lir::Operand::Var(lir::Var::from(tuple)))
            }
            sir::Value::Record(items) => {
                let ty = lir::Ty::Record(items.iter().fold(Vec::new(), |mut acc, (name, op)| {
                    let val = self.decompose_operand(bid, sbid, block, op);
                    acc.push((name, self.tid_from_operand(bid, &val)));
                    acc
                }));
                let tid = self.lir.types.get_id(ty);
                let record = self.push_local(bid, tid);

                for (name, operand) in items.iter() {
                    let op = self.decompose_operand(bid, sbid, block, operand);
                    let id = self.tid_from_operand(bid, &op);
                    let src = lir::Value::Use(op);
                    block.push(lir::Stmt::Eval {
                        dst: lir::Var::from((record, lir::Access::Field(name), id)),
                        src,
                    });
                }

                lir::Value::Use(lir::Operand::Var(lir::Var::from(record)))
            }
            sir::Value::Promote {
                variant,
                variants,
                operand,
            } => lir::Value::Promote {
                input: self.map_tid(variant.clone()),
                output: self.map_type(sir::Ty::Union(variants.clone())),
                operand: self.decompose_operand(bid, sbid, block, operand),
            },
            sir::Value::Coerce {
                inputs,
                variants,
                operand,
            } => lir::Value::Coerce {
                input: self.map_type(sir::Ty::Union(inputs.clone())),
                output: self.map_type(sir::Ty::Union(variants.clone())),
                operand: self.decompose_operand(bid, sbid, block, operand),
            },
            sir::Value::Call(ref place, ref operand) => {
                let op = self.decompose_operand(bid, sbid, block, operand);
                self::lir::Value::Call(lir::Var::from(self.read_place(bid, sbid, block, place)), op)
            }
            sir::Value::Binary(binary_op, operand, operand1) => {
                let lhs = self.decompose_operand(bid, sbid, block, operand);
                let rhs = self.decompose_operand(bid, sbid, block, operand1);
                match binary_op {
                    sir::BinaryOp::Add => lir::Value::Binary(lir::BinaryOp::Add, lhs, rhs),
                    sir::BinaryOp::Sub => lir::Value::Binary(lir::BinaryOp::Sub, lhs, rhs),
                    sir::BinaryOp::Mul => lir::Value::Binary(lir::BinaryOp::Mul, lhs, rhs),
                    sir::BinaryOp::Div => lir::Value::Binary(lir::BinaryOp::Div, lhs, rhs),
                    sir::BinaryOp::Mod => lir::Value::Binary(lir::BinaryOp::Mod, lhs, rhs),
                    sir::BinaryOp::BAnd => lir::Value::Binary(lir::BinaryOp::BAnd, lhs, rhs),
                    sir::BinaryOp::BOr => lir::Value::Binary(lir::BinaryOp::BOr, lhs, rhs),
                    sir::BinaryOp::BXor => lir::Value::Binary(lir::BinaryOp::BXor, lhs, rhs),
                    sir::BinaryOp::Eq => lir::Value::Binary(lir::BinaryOp::Eq, lhs, rhs),
                    sir::BinaryOp::Ne => lir::Value::Binary(lir::BinaryOp::Ne, lhs, rhs),
                    sir::BinaryOp::Lt => lir::Value::Binary(lir::BinaryOp::Lt, lhs, rhs),
                    sir::BinaryOp::Le => lir::Value::Binary(lir::BinaryOp::Le, lhs, rhs),
                    sir::BinaryOp::Gt => lir::Value::Binary(lir::BinaryOp::Gt, lhs, rhs),
                    sir::BinaryOp::Ge => lir::Value::Binary(lir::BinaryOp::Ge, lhs, rhs),
                    sir::BinaryOp::LShr => lir::Value::Binary(lir::BinaryOp::LShr, lhs, rhs),
                    sir::BinaryOp::LShl => lir::Value::Binary(lir::BinaryOp::LShl, lhs, rhs),
                    sir::BinaryOp::FAdd => lir::Value::Binary(lir::BinaryOp::FAdd, lhs, rhs),
                    sir::BinaryOp::FSub => lir::Value::Binary(lir::BinaryOp::FSub, lhs, rhs),
                    sir::BinaryOp::FMul => lir::Value::Binary(lir::BinaryOp::FMul, lhs, rhs),
                    sir::BinaryOp::FDiv => lir::Value::Binary(lir::BinaryOp::FDiv, lhs, rhs),
                    sir::BinaryOp::FMod => lir::Value::Binary(lir::BinaryOp::FMod, lhs, rhs),
                    sir::BinaryOp::FLt => lir::Value::Binary(lir::BinaryOp::FLt, lhs, rhs),
                    sir::BinaryOp::FLe => lir::Value::Binary(lir::BinaryOp::FLe, lhs, rhs),
                    sir::BinaryOp::FGt => lir::Value::Binary(lir::BinaryOp::FGt, lhs, rhs),
                    sir::BinaryOp::FGe => lir::Value::Binary(lir::BinaryOp::FGe, lhs, rhs),
                    sir::BinaryOp::And => lir::Value::Binary(lir::BinaryOp::And, lhs, rhs),
                    sir::BinaryOp::Or => lir::Value::Binary(lir::BinaryOp::Or, lhs, rhs),
                }
            }
            sir::Value::Unary(unary_op, operand) => {
                let op = self.decompose_operand(bid, sbid, block, operand);
                match unary_op {
                    sir::UnaryOp::Neg => lir::Value::Unary(lir::UnaryOp::Neg, op),
                    sir::UnaryOp::FNeg => lir::Value::Unary(lir::UnaryOp::FNeg, op),
                    sir::UnaryOp::BNot => lir::Value::Unary(lir::UnaryOp::BNot, op),
                    sir::UnaryOp::Not => lir::Value::Unary(lir::UnaryOp::Not, op),
                    sir::UnaryOp::Deref => lir::Value::Unary(lir::UnaryOp::Deref, op),
                }
            }
            sir::Value::Closure {
                body,
                generics: _,
                captures,
            } => {
                let lbid = self.build_body(body.clone());
                let func = self.lir.bodies[lbid].tid;
                let closure = self.push_local(lbid, func);
                block.push(lir::Stmt::Closure {
                    dst: lir::Var::from(closure),
                    func: lbid,
                });
                for capture in captures {
                    let arg = self.decompose_operand(bid, sbid, block, capture);
                    let tid = self.tid_from_operand(bid, &arg);
                    let dst = self.push_local(bid, tid);
                    block.push(lir::Stmt::Eval {
                        dst: lir::Var::from(dst),
                        src: lir::Value::Call(lir::Var::from(closure), arg),
                    });
                }
                lir::Value::Use(lir::Operand::Var(lir::Var::from(closure)))
            }
        }
    }

    fn decompose_place(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        block: &mut lir::Block,
        place: &sir::Place,
    ) -> lir::Place {
        let mut lplace = lir::Place::new();
        lplace.from({
            let sid = self.sir.bodies[sbid].locals[place.local];
            let base = {
                let tid = self.map_tid(sid);
                self.push_local(bid, tid)
            };
            self.local_map.insert(place.local.clone(), base);
            base
        });
        for (proj, id) in &place.proj {
            match proj {
                sir::Proj::Field(name) => {
                    lplace
                        .access
                        .push((lir::Access::Field(name), self.map_tid(id.clone())));
                }
                sir::Proj::Tuple(index) => {
                    lplace.access.push((
                        lir::Access::Tuple(index.clone() as u32),
                        self.map_tid(id.clone()),
                    ));
                }
                sir::Proj::Index(ref op) => {
                    let index = self.decompose_operand(bid, sbid, block, op);
                    if lplace.index.is_some() || lplace.deref || !lplace.access.is_empty() {
                        let new = self.read_lplace(bid, block, &lplace);
                        lplace.from(new);
                    }
                    lplace.index = Some(index);
                }
                sir::Proj::Deref => {
                    if lplace.index.is_some() || lplace.deref || !lplace.access.is_empty() {
                        let new = self.read_lplace(bid, block, &lplace);
                        lplace.from(new);
                    }
                    lplace.deref = true;
                }
            }
        }

        lplace
    }

    fn decompose_case(&mut self, bid: lir::Bid, sbid: sir::Bid, case: &sir::Case) -> lir::Case {
        let tid = self.map_tid(case.ty);
        lir::Case {
            tid,
            local: lir::Var::from(self.push_local(bid, tid)),
            block: self.decompose_block(bid, sbid, &case.block),
        }
    }

    fn decompose_block(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        sblock: &sir::Block,
    ) -> lir::Block {
        let mut lblock = lir::Block::new();

        for stmt in &sblock.stmts {
            self.build_stmt(bid, sbid, &mut lblock, &stmt);
        }

        if let Some(ref term) = sblock.term {
            let stmt = match term {
                sir::Term::Return(ref value) => lir::Stmt::Return {
                    val: {
                        let val = self.decompose_value(bid, sbid, &mut lblock, value);
                        lir::Operand::Var(self.var_from_value(bid, &mut lblock, &val))
                    },
                },
                sir::Term::Break => lir::Stmt::Break {},
            };
            lblock.push(stmt);
        }
        lblock
    }

    fn build_stmt(
        &mut self,
        bid: lir::Bid,
        sbid: sir::Bid,
        block: &mut lir::Block,
        stmt: &sir::Stmt,
    ) {
        match stmt {
            // do nothing with drop (calculate later)
            sir::Stmt::Drop(local) => {}
            sir::Stmt::Assign(place, value) => {
                let src = self.decompose_value(bid, sbid, block, value);
                let st = self.write_place(bid, sbid, block, place, src);
                block.push(st);
            }
            sir::Stmt::Loop(sblock) => {
                let st = lir::Stmt::Loop {
                    body: self.decompose_block(bid, sbid, sblock),
                };
                block.push(st);
            }
            sir::Stmt::Match {
                target,
                cases,
                default,
            } => {
                let st = lir::Stmt::Match {
                    target: lir::Var::from(self.read_place(bid, sbid, block, target)),
                    cases: cases.iter().fold(Vec::new(), |mut vec, case| {
                        vec.push(self.decompose_case(bid, sbid, case));
                        vec
                    }),
                    default: self.decompose_block(bid, sbid, default),
                };
                block.push(st);
            }
        }
    }

    fn map_tid(&mut self, sid: sir::Tid) -> lir::Tid {
        match self.type_map.get(&self.sir.types[sid]) {
            Some(id) => return id.clone(),
            None => {}
        };

        self.map_type(self.sir.types[sid].clone())
    }

    fn map_type(&mut self, ty: sir::Ty) -> lir::Tid {
        match self.type_map.get(&ty) {
            Some(id) => return id.clone(),
            None => {}
        };

        let lt = match ty {
            sir::Ty::Int => lir::Ty::Int,
            sir::Ty::Float => lir::Ty::Float,
            sir::Ty::Str => lir::Ty::Str,
            sir::Ty::None => lir::Ty::Empty,
            sir::Ty::True => lir::Ty::True,
            sir::Ty::False => lir::Ty::False,
            sir::Ty::Never => lir::Ty::Never,
            sir::Ty::Ref(tid) => lir::Ty::Ref(self.map_tid(tid)),
            sir::Ty::List(tid) => lir::Ty::List(self.map_tid(tid)),
            sir::Ty::Func(tid, tid1) => lir::Ty::Func(self.map_tid(tid), self.map_tid(tid1)),
            sir::Ty::Tuple(ref tids) => lir::Ty::Tuple {
                0: tids.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_tid(tid.clone()));
                    vec
                }),
            },
            sir::Ty::Record(ref items) => lir::Ty::Record {
                0: items.iter().fold(Vec::new(), |mut vec, (name, tid)| {
                    vec.push((name, self.map_tid(tid.clone())));
                    vec
                }),
            },
            sir::Ty::Union(ref btree_set) => lir::Ty::Union {
                0: btree_set.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_tid(tid.clone()));
                    vec
                }),
            },
        };

        let id = self.lir.types.get_id(lt);
        self.type_map.insert(ty, id);

        id
    }
}
