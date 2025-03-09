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
                                    self.map_type(body.locals[sir::Local { 0: i }])
                                } else {
                                    let input = self.map_type(body.locals[sir::Local { 0: i }]);
                                    self.lir.types.get_id(lir::Ty::Func(input, acc))
                                }
                            }),
                        self.map_type(body.output),
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
            let lid = self.map_type(sid);
            self.lir.bodies[id].locals.push(lid);
        }

        // if function is not extern, body should be Some
        let sblock = self.sir.bodies[bid].block.as_ref().unwrap();

        // build lir body
        let lblock = self.decompose_block(id, sblock);
        self.lir.bodies[id].block = lblock;

        id
    }

    fn var_from_value(&mut self, bid: lir::Bid, value: lir::Value) -> lir::Var {
        todo!()
    }

    fn operand_from_value(&mut self, bid: lir::Bid, value: lir::Value) -> lir::Operand {
        todo!()
    }

    fn var_from_operand(&mut self, bid: lir::Bid, operand: lir::Operand) -> lir::Var {
        todo!()
    }

    fn decompose_operand(&mut self, bid: lir::Bid, operand: &sir::Operand) -> lir::Operand {
        match operand {
            sir::Operand::Copy(ref place) => lir::Operand::Var(self.decompose_place(bid, place)),
            sir::Operand::Const(ref cst, tid) => lir::Operand::Const(match cst {
                sir::Const::None => lir::Const::Empty(self.map_type(tid.clone())),
                sir::Const::Int(v) => lir::Const::Int(self.map_type(tid.clone()), v.clone()),
                sir::Const::Float(v) => lir::Const::Float(self.map_type(tid.clone()), v.clone()),
                sir::Const::String(v) => lir::Const::String(self.map_type(tid.clone()), v),
            }),
        }
    }

    fn decompose_value(&mut self, bid: lir::Bid, value: &sir::Value) -> lir::Value {
        match value {
            sir::Value::Use(ref operand) => lir::Value::Use({
                let op = self.decompose_operand(bid, operand);
                self.var_from_operand(bid, op)
            }),
            sir::Value::Tuple(operands) => {
                todo!()
            }
            sir::Value::Record(items) => {
                todo!()
            }
            sir::Value::Promote {
                variant,
                ref variants,
                ref operand,
            } => {
                todo!()
            }
            sir::Value::Coerce {
                ref inputs,
                ref variants,
                ref operand,
            } => {
                todo!()
            }
            sir::Value::Call(ref place, ref operand) => {
                let var = self.decompose_place(bid, place);
                let op = self.decompose_operand(bid, operand);
                self::lir::Value::Call(var, op)
            }
            sir::Value::Binary(binary_op, operand, operand1) => {
                todo!()
            }
            sir::Value::Unary(unary_op, operand) => {
                todo!()
            }
            sir::Value::Closure {
                body,
                generics,
                captures,
            } => todo!(),
        };
        todo!()
    }

    fn decompose_place(&mut self, bid: lir::Bid, place: &sir::Place) -> lir::Var {
        todo!()
    }

    fn decompose_case(&mut self, bid: lir::Bid, case: &sir::Case) -> lir::Case {
        todo!()
    }

    fn decompose_block(&mut self, bid: lir::Bid, block: &sir::Block) -> lir::Block {
        let mut lblock = block.stmts.iter().fold(lir::Block::new(), |mut acc, stmt| {
            acc.stmts.push(self.build_stmt(bid, stmt));
            acc
        });
        if let Some(ref term) = block.term {
            lblock.stmts.push({
                match term {
                    sir::Term::Return(ref value) => lir::Stmt::Return {
                        val: {
                            let val = self.decompose_value(bid, value);
                            self.operand_from_value(bid, val)
                        },
                    },
                    sir::Term::Break => lir::Stmt::Break {},
                }
            });
        }
        lblock
    }

    fn build_stmt(&mut self, bid: lir::Bid, stmt: &sir::Stmt) -> lir::Stmt {
        match stmt {
            sir::Stmt::Drop(value, _) => lir::Stmt::Drop {
                var: {
                    let val = self.decompose_value(bid, value);
                    self.var_from_value(bid, val)
                },
            },
            sir::Stmt::Assign(place, value) => lir::Stmt::Eval {
                dst: self.decompose_place(bid, place),
                src: self.decompose_value(bid, value),
            },
            sir::Stmt::Loop(block) => lir::Stmt::Loop {
                body: self.decompose_block(bid, block),
            },
            sir::Stmt::Match {
                target,
                cases,
                default,
            } => lir::Stmt::Match {
                target: self.decompose_place(bid, target),
                cases: cases.iter().fold(Vec::new(), |mut vec, case| {
                    vec.push(self.decompose_case(bid, case));
                    vec
                }),
                default: self.decompose_block(bid, default),
            },
        }
    }

    fn map_type(&mut self, sid: sir::Tid) -> lir::Tid {
        match self.type_map.get(&self.sir.types[sid]) {
            Some(id) => return id.clone(),
            None => {}
        };

        let ty = self.sir.types[sid].clone();

        let lt = match ty {
            sir::Ty::Int => lir::Ty::Int,
            sir::Ty::Float => lir::Ty::Float,
            sir::Ty::Str => lir::Ty::Str,
            sir::Ty::None => lir::Ty::Named(0xffffffff, self.lir.types.get_id(lir::Ty::Empty)),
            sir::Ty::True => lir::Ty::True,
            sir::Ty::False => lir::Ty::False,
            sir::Ty::Never => lir::Ty::Never,
            sir::Ty::Ref(tid) => lir::Ty::Ref(self.map_type(tid)),
            sir::Ty::List(tid) => lir::Ty::List(self.map_type(tid)),
            sir::Ty::Func(tid, tid1) => lir::Ty::Func(self.map_type(tid), self.map_type(tid1)),
            sir::Ty::Tuple(ref tids) => lir::Ty::Record {
                0: tids.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_type(tid.clone()));
                    vec
                }),
            },
            sir::Ty::Record(ref items) => lir::Ty::Record {
                0: items.iter().fold(Vec::new(), |mut vec, (_, tid)| {
                    vec.push(self.map_type(tid.clone()));
                    vec
                }),
            },
            sir::Ty::Union(ref btree_set) => lir::Ty::Union {
                0: btree_set.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_type(tid.clone()));
                    vec
                }),
            },
        };

        let id = self.lir.types.get_id(lt);
        self.type_map.insert(ty, id);

        id
    }
}
