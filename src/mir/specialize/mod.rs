use std::collections::{hash_map::Entry, HashMap};

use crate::{attribute::Attributes, mir, sir};

pub fn specialize(program: mir::Program, entry: mir::Bid) -> (sir::Program, sir::Bid) {
    let mut specializer = Specializer::new(&program);

    let body = specializer.body(entry, &[]);

    (specializer.finish(), body)
}

struct Specializer<'a> {
    mir: &'a mir::Program,
    sir: sir::Program,

    names: HashMap<sir::Tid, String>,
    named: HashMap<(u32, Vec<sir::Tid>), sir::Tid>,
    bodies: HashMap<(mir::Bid, Vec<sir::Tid>), sir::Bid>,
}

impl<'a> Specializer<'a> {
    fn new(generic: &'a mir::Program) -> Self {
        Self {
            mir: generic,
            sir: sir::Program::default(),

            names: HashMap::new(),
            named: HashMap::new(),
            bodies: HashMap::new(),
        }
    }

    fn body(&mut self, id: mir::Bid, generics: &[sir::Tid]) -> sir::Bid {
        if let Some(&body) = self.bodies.get(&(id, generics.to_vec())) {
            return body;
        }

        // we have to insert a dummy body and cache the id to avoid infinite recursion
        let spec = self.sir.bodies.push(sir::Body {
            attrs: Default::default(),
            is_extern: false,
            name: None,
            captures: 0,
            arguments: 0,
            output: self.sir.types.get_or_insert(&sir::Ty::None),
            locals: sir::Locals::new(),
            block: None,
        });

        self.bodies.insert((id, generics.to_vec()), spec);

        if self.mir.bodies[id].attrs.find_value("intrinsic") == Some("string::format") {
            self.sir.bodies[spec] = self.format(generics[0]);

            return spec;
        }

        let mut locals = sir::Locals::new();

        for (_, ty) in self.mir.bodies[id].locals.iter() {
            locals.push(self.ty(ty.clone(), generics));
        }

        let block = self.mir.bodies[id]
            .block
            .clone()
            .map(|b| self.block(b, generics));

        let name = {
            let generics: Vec<_> = generics
                .iter()
                .map(|tid| self.names[tid].as_str())
                .collect();

            if generics.is_empty() {
                self.mir.bodies[id].name.clone()
            } else {
                self.mir.bodies[id]
                    .name
                    .as_ref()
                    .map(|name| format!("{}<{}>", name, generics.join(", ")))
            }
        };

        let body = &self.mir.bodies[id];

        self.sir.bodies[spec] = sir::Body {
            attrs: body.attrs.clone(),
            is_extern: body.is_extern,
            name,
            captures: body.captures,
            arguments: body.arguments,
            output: self.ty(body.output.clone(), generics),
            locals,
            block,
        };

        spec
    }

    fn format(&mut self, tid: sir::Tid) -> sir::Body {
        let str_tid = self.sir.types.get_or_insert(&sir::Ty::Str);
        let int_tid = self.sir.types.get_or_insert(&sir::Ty::Int);

        match self.sir.types[tid] {
            sir::Ty::Int => sir::Body {
                attrs: Attributes::new().with(("link", "brad_int_to_str")),
                is_extern: true,
                name: None,
                captures: 0,
                arguments: 1,
                output: str_tid,
                locals: sir::Locals::new().with(int_tid),
                block: None,
            },

            sir::Ty::Float => sir::Body {
                attrs: Attributes::new().with(("link", "brad_float_to_str")),
                is_extern: true,
                name: None,
                captures: 0,
                arguments: 1,
                output: str_tid,
                locals: sir::Locals::new().with(int_tid),
                block: None,
            },

            sir::Ty::Str => {
                let mut locals = sir::Locals::new();
                let input = locals.push(str_tid);

                let mut block = sir::Block::new();

                block.term(sir::Term::Return(sir::Value::local(input)));

                sir::Body {
                    attrs: Attributes::new(),
                    is_extern: false,
                    name: None,
                    captures: 0,
                    arguments: 1,
                    output: str_tid,
                    locals,
                    block: Some(block),
                }
            }

            sir::Ty::True => {
                let mut block = sir::Block::new();
                block.term(sir::Term::Return(sir::Value::Use(sir::Operand::Const(
                    sir::Const::String("true"),
                    str_tid,
                ))));

                sir::Body {
                    attrs: Attributes::new(),
                    is_extern: false,
                    name: None,
                    captures: 0,
                    arguments: 0,
                    output: str_tid,
                    locals: sir::Locals::new(),
                    block: Some(block),
                }
            }

            sir::Ty::False => {
                let mut block = sir::Block::new();
                block.term(sir::Term::Return(sir::Value::Use(sir::Operand::Const(
                    sir::Const::String("false"),
                    str_tid,
                ))));

                sir::Body {
                    attrs: Attributes::new(),
                    is_extern: false,
                    name: None,
                    captures: 0,
                    arguments: 0,
                    output: str_tid,
                    locals: sir::Locals::new(),
                    block: Some(block),
                }
            }

            sir::Ty::None => {
                let mut block = sir::Block::new();
                block.term(sir::Term::Return(sir::Value::Use(sir::Operand::Const(
                    sir::Const::String("none"),
                    str_tid,
                ))));

                sir::Body {
                    attrs: Attributes::new(),
                    is_extern: false,
                    name: None,
                    captures: 0,
                    arguments: 0,
                    output: str_tid,
                    locals: sir::Locals::new(),
                    block: Some(block),
                }
            }

            sir::Ty::Never => {
                let mut block = sir::Block::new();
                block.term(sir::Term::Return(sir::Value::Use(sir::Operand::Const(
                    sir::Const::String("!"),
                    str_tid,
                ))));

                sir::Body {
                    attrs: Attributes::new(),
                    is_extern: false,
                    name: None,
                    captures: 0,
                    arguments: 0,
                    output: str_tid,
                    locals: sir::Locals::new(),
                    block: Some(block),
                }
            }

            sir::Ty::Ref(_) => todo!(),
            sir::Ty::List(_) => todo!(),
            sir::Ty::Func(_, _) => todo!(),

            sir::Ty::Tuple(_) => todo!(),

            sir::Ty::Record(_) => todo!(),

            sir::Ty::Union(_) => todo!(),
        }
    }

    fn block(&mut self, block: mir::Block, generics: &[sir::Tid]) -> sir::Block {
        let stmts = block
            .stmts
            .into_iter()
            .map(|stmt| self.stmt(stmt, generics))
            .collect();

        let term = block.term.map(|term| self.term(term, generics));

        sir::Block { stmts, term }
    }

    fn stmt(&mut self, stmt: mir::Stmt, generics: &[sir::Tid]) -> sir::Stmt {
        match stmt {
            mir::Stmt::Drop(operand) => {
                let operand = self.operand(operand, generics);
                sir::Stmt::Drop(operand)
            }

            mir::Stmt::Assign(place, value) => {
                let place = self.place(place, generics);
                let value = self.value(value, generics);
                sir::Stmt::Assign(place, value)
            }

            mir::Stmt::Loop(block) => sir::Stmt::Loop(self.block(block, generics)),

            mir::Stmt::Match {
                target,
                cases,
                default,
            } => {
                let target = self.place(target, generics);

                let cases = cases
                    .into_iter()
                    .map(|case| self.case(case, generics))
                    .collect();

                let default = self.block(default, generics);

                sir::Stmt::Match {
                    target,
                    cases,
                    default,
                }
            }
        }
    }

    fn case(&mut self, case: mir::Case, generics: &[sir::Tid]) -> sir::Case {
        sir::Case {
            ty: self.ty(case.ty, generics),
            local: case.local,
            block: self.block(case.block, generics),
        }
    }

    fn term(&mut self, term: mir::Term, generics: &[sir::Tid]) -> sir::Term {
        match term {
            mir::Term::Return(value) => sir::Term::Return(self.value(value, generics)),
            mir::Term::Break => sir::Term::Break,
        }
    }

    fn value(&mut self, value: mir::Value, generics: &[sir::Tid]) -> sir::Value {
        match value {
            mir::Value::Use(operand) => {
                let operand = self.operand(operand, generics);

                sir::Value::Use(operand)
            }

            mir::Value::Tuple(operands) => {
                let operands = operands
                    .into_iter()
                    .map(|operand| self.operand(operand, generics))
                    .collect();

                sir::Value::Tuple(operands)
            }

            mir::Value::Record(operands) => {
                let operands = operands
                    .into_iter()
                    .map(|(name, operand)| (name, self.operand(operand, generics)))
                    .collect();

                sir::Value::Record(operands)
            }

            mir::Value::Promote {
                variant: input,
                variants,
                operand,
            } => {
                let input = self.ty(input, generics);
                let variants = variants
                    .into_iter()
                    .map(|ty| self.ty(ty, generics))
                    .collect();
                let operand = self.operand(operand, generics);

                sir::Value::Promote {
                    variant: input,
                    variants,
                    operand,
                }
            }

            mir::Value::Coerce {
                inputs,
                variants,
                operand,
            } => {
                let inputs = inputs.into_iter().map(|ty| self.ty(ty, generics)).collect();

                let variants = variants
                    .into_iter()
                    .map(|ty| self.ty(ty, generics))
                    .collect();

                let operand = self.operand(operand, generics);

                sir::Value::Coerce {
                    inputs,
                    variants,
                    operand,
                }
            }

            mir::Value::Call(input, output) => {
                let input = self.place(input, generics);
                let output = self.operand(output, generics);

                sir::Value::Call(input, output)
            }

            mir::Value::Binary(op, lhs, rhs) => {
                let lhs = self.operand(lhs, generics);
                let rhs = self.operand(rhs, generics);

                sir::Value::Binary(op, lhs, rhs)
            }

            mir::Value::Unary(op, operand) => {
                let operand = self.operand(operand, generics);

                sir::Value::Unary(op, operand)
            }

            mir::Value::Closure {
                body,
                generics: generics_,
                captures,
            } => {
                let captures = captures
                    .into_iter()
                    .map(|operand| self.operand(operand, generics))
                    .collect();

                let generics: Vec<_> = generics_
                    .into_iter()
                    .map(|ty| self.ty(ty, generics))
                    .collect();

                let body = self.body(body, generics.as_slice());

                sir::Value::Closure {
                    body,
                    generics,
                    captures,
                }
            }
        }
    }

    fn ty(&mut self, mir: mir::Ty, generics: &[sir::Tid]) -> sir::Tid {
        let sir = match mir.clone() {
            mir::Ty::Int => sir::Ty::Int,
            mir::Ty::Float => sir::Ty::Float,
            mir::Ty::Str => sir::Ty::Str,
            mir::Ty::True => sir::Ty::True,
            mir::Ty::False => sir::Ty::False,
            mir::Ty::None => sir::Ty::None,
            mir::Ty::Never => sir::Ty::Never,
            mir::Ty::Generic(idx) => return generics[idx as usize],

            mir::Ty::Named(idx, tys) => {
                let tys: Vec<_> = tys.into_iter().map(|ty| self.ty(ty, generics)).collect();
                let tid = self.ty(self.mir.types[idx].ty.clone(), &tys);

                match self.named.entry((idx, tys)) {
                    Entry::Occupied(entry) => return *entry.get(),
                    Entry::Vacant(entry) => {
                        let ty = self.sir.types[tid].clone();
                        let tid = self.sir.types.push(ty);
                        entry.insert(tid);

                        if let Entry::Vacant(e) = self.names.entry(tid) {
                            e.insert(self.mir.types.format(&mir));
                        }

                        return tid;
                    }
                }
            }

            mir::Ty::Ref(ty) => sir::Ty::Ref(self.ty(*ty, generics)),

            mir::Ty::List(ty) => sir::Ty::List(self.ty(*ty, generics)),

            mir::Ty::Func(i, o) => {
                let i = self.ty(*i, generics);
                let o = self.ty(*o, generics);

                sir::Ty::Func(i, o)
            }

            mir::Ty::Tuple(tys) => {
                let tys = tys.into_iter().map(|ty| self.ty(ty, generics)).collect();
                sir::Ty::Tuple(tys)
            }

            mir::Ty::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| (name, self.ty(ty, generics)))
                    .collect();

                sir::Ty::Record(fields)
            }

            mir::Ty::Union(tys) => {
                let tys = tys.into_iter().map(|ty| self.ty(ty, generics)).collect();
                sir::Ty::Union(tys)
            }
        };

        let tid = self.sir.types.get_or_insert(&sir);

        if let Entry::Vacant(e) = self.names.entry(tid) {
            e.insert(self.mir.types.format(&mir));
        }

        tid
    }

    fn operand(&mut self, operand: mir::Operand, generics: &[sir::Tid]) -> sir::Operand {
        match operand {
            mir::Operand::Copy(place) => sir::Operand::Copy(self.place(place, generics)),
            mir::Operand::Move(place) => sir::Operand::Move(self.place(place, generics)),
            mir::Operand::Const(r#const, ty) => sir::Operand::Const(r#const, self.ty(ty, generics)),
        }
    }

    fn place(&mut self, place: mir::Place, generics: &[sir::Tid]) -> sir::Place {
        let local = place.local;
        let is_mutable = place.is_mutable;

        let proj = place
            .proj
            .into_iter()
            .map(|(proj, ty)| (self.proj(proj, generics), self.ty(ty, generics)))
            .collect();

        sir::Place {
            local,
            proj,
            is_mutable,
        }
    }

    fn proj(&mut self, proj: mir::Proj, generics: &[sir::Tid]) -> sir::Proj {
        match proj {
            mir::Proj::Field(name) => sir::Proj::Field(name),
            mir::Proj::Tuple(element) => sir::Proj::Tuple(element),
            mir::Proj::Index(operand) => sir::Proj::Index(self.operand(operand, generics)),
            mir::Proj::Deref => sir::Proj::Deref,
        }
    }

    fn finish(self) -> sir::Program {
        self.sir
    }
}
