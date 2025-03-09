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
    main.build();

    println!("{:#?}", program);

    todo!()
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

    fn build(&mut self) {
        self.build_body(self.source);
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
        let mut lblock = lir::Block::new();

        lblock = self.decompose_block(id, &mut lblock, sblock);
        self.lir.bodies[id].block = lblock;

        id
    }

    fn push_local(&mut self, bid: lir::Bid, tid: lir::Tid) -> lir::Local {
        let locals = &mut self.lir.bodies[bid].locals;
        let index = locals.len();
        locals.push(tid);
        lir::Local(index as u32)
    }

    fn var_from_value(&mut self, bid: lir::Bid, value: &lir::Value) -> lir::Var {
        todo!()
    }

    fn operand_from_value(&mut self, bid: lir::Bid, value: &lir::Value) -> lir::Operand {
        todo!()
    }

    fn var_from_operand(&mut self, bid: lir::Bid, operand: &lir::Operand) -> lir::Var {
        todo!()
    }

    fn tid_from_operand(&mut self, bid: lir::Bid, operand: &lir::Operand) -> lir::Tid {
        match operand {
            lir::Operand::Var(lir::Var {
                local: lir::Local(local),
                access: lir::Access(access),
            }) => {
                let lid = self.lir.bodies[bid].locals[local.clone() as usize];
                access.iter().fold(lid, |acc, index| {
                    if let lir::Ty::Record(elems) = &self.lir.types[lid] {
                        elems[index.clone() as usize]
                    } else {
                        panic!("uh oh, we did a fucky wucky")
                    }
                })
            }
            lir::Operand::Const(tid, _) => tid.clone(),
        }
    }

    fn decompose_operand(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        operand: &sir::Operand,
    ) -> lir::Operand {
        match operand {
            sir::Operand::Copy(ref place) => {
                lir::Operand::Var(self.decompose_place(bid, block, place))
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
        block: &mut lir::Block,
        value: &sir::Value,
    ) -> lir::Value {
        match value {
            sir::Value::Use(ref operand) => {
                lir::Value::Use(self.decompose_operand(bid, block, operand))
            }
            sir::Value::Tuple(operands) => {
                let ty = lir::Ty::Record(operands.iter().fold(Vec::new(), |mut acc, op| {
                    let val = self.decompose_operand(bid, block, op);
                    acc.push(self.tid_from_operand(bid, &val));
                    acc
                }));
                let tid = self.lir.types.get_id(ty);
                let tuple = self.push_local(bid, tid);

                // rewrite this to work with ref type
                for (i, operand) in operands.iter().enumerate() {
                    let src = lir::Value::Use(self.decompose_operand(bid, block, operand));
                    block.push(lir::Stmt::Eval {
                        dst: lir::Var::from((tuple, i as u32)),
                        src,
                    });
                }
                lir::Value::Use(lir::Operand::Var(lir::Var::from(tuple)));

                // fix this shit
                todo!()
            }
            sir::Value::Record(items) => {
                let ty = lir::Ty::Record(items.iter().fold(Vec::new(), |mut acc, (_, op)| {
                    let val = self.decompose_operand(bid, block, op);
                    acc.push(self.tid_from_operand(bid, &val));
                    acc
                }));
                let tid = self.lir.types.get_id(ty);
                let record = self.push_local(bid, tid);

                // rewrite this to work with ref type
                for (i, (_, operand)) in items.iter().enumerate() {
                    let src = lir::Value::Use(self.decompose_operand(bid, block, operand));
                    block.push(lir::Stmt::Eval {
                        dst: lir::Var::from((record, i as u32)),
                        src,
                    });
                }
                lir::Value::Use(lir::Operand::Var(lir::Var::from(record)));

                // fix this shit
                todo!()
            }
            sir::Value::Promote {
                variant,
                variants,
                operand,
            } => lir::Value::Promote {
                input: self.map_tid(variant.clone()),
                output: self.map_type(sir::Ty::Union(variants.clone())),
                operand: self.decompose_operand(bid, block, operand),
            },
            sir::Value::Coerce {
                inputs,
                variants,
                operand,
            } => lir::Value::Coerce {
                input: self.map_type(sir::Ty::Union(inputs.clone())),
                output: self.map_type(sir::Ty::Union(variants.clone())),
                operand: self.decompose_operand(bid, block, operand),
            },
            sir::Value::Call(ref place, ref operand) => {
                let var = self.decompose_place(bid, block, place);
                let op = self.decompose_operand(bid, block, operand);
                self::lir::Value::Call(var, op)
            }
            sir::Value::Binary(binary_op, operand, operand1) => {
                todo!()
            }
            sir::Value::Unary(unary_op, operand) => {
                todo!()
            }
            sir::Value::Closure {
                body: sbid,
                generics,
                captures,
            } => {
                let lbid = self.build_body(sbid.clone());
                let func = self.lir.bodies[lbid].tid;
                let closure = self.push_local(lbid, func);
                block.push(lir::Stmt::Closure {
                    dst: lir::Var::from(closure),
                    func: lbid,
                });
                for capture in captures {
                    let arg = self.decompose_operand(bid, block, capture);
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
        block: &mut lir::Block,
        place: &sir::Place,
    ) -> lir::Var {
        todo!()
    }

    fn decompose_case(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        case: &sir::Case,
    ) -> lir::Case {
        let tid = self.map_tid(case.ty);
        lir::Case {
            tid,
            local: lir::Var::from(self.push_local(bid, tid)),
            block: self.decompose_block(bid, block, &case.block),
        }
    }

    fn decompose_block(
        &mut self,
        bid: lir::Bid,
        block: &mut lir::Block,
        sblock: &sir::Block,
    ) -> lir::Block {
        let mut lblock = lir::Block::new();

        for stmt in &sblock.stmts {
            let val = self.build_stmt(bid, &mut lblock, &stmt);
            lblock.push(val);
        }

        if let Some(ref term) = sblock.term {
            let stmt = match term {
                sir::Term::Return(ref value) => lir::Stmt::Return {
                    val: {
                        let val = self.decompose_value(bid, &mut lblock, value);
                        self.operand_from_value(bid, &val)
                    },
                },
                sir::Term::Break => lir::Stmt::Break {},
            };
            lblock.push(stmt);
        }
        lblock
    }

    fn build_stmt(&mut self, bid: lir::Bid, block: &mut lir::Block, stmt: &sir::Stmt) -> lir::Stmt {
        match stmt {
            sir::Stmt::Drop(value) => lir::Stmt::Drop {
                var: {
                    let val = self.decompose_value(bid, block, value);
                    self.var_from_value(bid, &val)
                },
            },
            sir::Stmt::Assign(place, value) => lir::Stmt::Eval {
                dst: self.decompose_place(bid, block, place),
                src: self.decompose_value(bid, block, value),
            },
            sir::Stmt::Loop(sblock) => lir::Stmt::Loop {
                body: self.decompose_block(bid, block, sblock),
            },
            sir::Stmt::Match {
                target,
                cases,
                default,
            } => lir::Stmt::Match {
                target: self.decompose_place(bid, block, target),
                cases: cases.iter().fold(Vec::new(), |mut vec, case| {
                    vec.push(self.decompose_case(bid, block, case));
                    vec
                }),
                default: self.decompose_block(bid, block, default),
            },
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
            sir::Ty::None => lir::Ty::Named(0xffffffff, self.lir.types.get_id(lir::Ty::Empty)),
            sir::Ty::True => lir::Ty::True,
            sir::Ty::False => lir::Ty::False,
            sir::Ty::Never => lir::Ty::Never,
            sir::Ty::Ref(tid) => lir::Ty::Ref(self.map_tid(tid)),
            sir::Ty::List(tid) => lir::Ty::List(self.map_tid(tid)),
            sir::Ty::Func(tid, tid1) => lir::Ty::Func(self.map_tid(tid), self.map_tid(tid1)),
            sir::Ty::Tuple(ref tids) => lir::Ty::Record {
                0: tids.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_tid(tid.clone()));
                    vec
                }),
            },
            sir::Ty::Record(ref items) => lir::Ty::Record {
                0: items.iter().fold(Vec::new(), |mut vec, (_, tid)| {
                    vec.push(self.map_tid(tid.clone()));
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
