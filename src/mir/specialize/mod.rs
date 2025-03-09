use std::collections::{hash_map::Entry, HashMap};

use crate::{mir, sir};

pub fn specialize(program: mir::Program, entry: mir::Bid) -> (sir::Program, sir::Bid) {
    let mut specializer = Specializer::new(&program);

    let body = specializer.body(entry, &[]);

    (specializer.finish(), body)
}

struct Specializer<'a> {
    generic: &'a mir::Program,
    specialized: sir::Program,

    names: HashMap<sir::Tid, String>,
    named: HashMap<(u32, Vec<sir::Tid>), sir::Tid>,
    bodies: HashMap<(mir::Bid, Vec<sir::Tid>), sir::Bid>,
}

impl<'a> Specializer<'a> {
    fn new(generic: &'a mir::Program) -> Self {
        Self {
            generic,
            specialized: sir::Program::default(),

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
        let spec = self.specialized.bodies.push(sir::Body {
            attrs: Default::default(),
            is_extern: false,
            name: None,
            captures: 0,
            arguments: 0,
            output: self.specialized.types.get_or_insert(&sir::Ty::None),
            locals: sir::Locals::new(),
            block: None,
        });

        self.bodies.insert((id, generics.to_vec()), spec);

        let mut locals = sir::Locals::new();

        for (_, ty) in self.generic.bodies[id].locals.iter() {
            locals.push(self.ty(ty.clone(), generics));
        }

        let block = self.generic.bodies[id]
            .block
            .clone()
            .map(|b| self.block(b, generics));

        let name = {
            let generics: Vec<_> = generics
                .iter()
                .map(|tid| self.names[tid].as_str())
                .collect();

            if generics.is_empty() {
                self.generic.bodies[id].name.clone()
            } else {
                self.generic.bodies[id]
                    .name
                    .as_ref()
                    .map(|name| format!("{}<{}>", name, generics.join(", ")))
            }
        };

        let body = &self.generic.bodies[id];

        self.specialized.bodies[spec] = sir::Body {
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

    fn block(&mut self, block: mir::Block, generics: &[sir::Tid]) -> sir::Block {
        let stmts = block
            .stmts
            .into_iter()
            .map(|stmt| self.stmt(stmt, generics))
            .collect();

        let term = self.term(block.term, generics);

        sir::Block { stmts, term }
    }

    fn stmt(&mut self, stmt: mir::Stmt, generics: &[sir::Tid]) -> sir::Stmt {
        match stmt {
            mir::Stmt::Drop(value) => sir::Stmt::Drop(self.value(value, generics)),

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
            mir::Term::Exit => sir::Term::Exit,
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
                let tid = self.ty(self.generic.types[idx].ty.clone(), &tys);

                match self.named.entry((idx, tys)) {
                    Entry::Occupied(entry) => return *entry.get(),
                    Entry::Vacant(entry) => {
                        let ty = self.specialized.types[tid].clone();
                        let tid = self.specialized.types.push(ty);
                        entry.insert(tid);

                        if let Entry::Vacant(e) = self.names.entry(tid) {
                            e.insert(self.generic.types.format(&mir));
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

        let tid = self.specialized.types.get_or_insert(&sir);

        if let Entry::Vacant(e) = self.names.entry(tid) {
            e.insert(self.generic.types.format(&mir));
        }

        tid
    }

    fn operand(&mut self, operand: mir::Operand, generics: &[sir::Tid]) -> sir::Operand {
        match operand {
            mir::Operand::Copy(place) => sir::Operand::Copy(self.place(place, generics)),
            mir::Operand::Const(r#const, ty) => sir::Operand::Const(r#const, self.ty(ty, generics)),
        }
    }

    fn place(&mut self, place: mir::Place, generics: &[sir::Tid]) -> sir::Place {
        let local = place.local;
        let is_mutable = place.is_mutable;

        let proj = place
            .proj
            .into_iter()
            .map(|(proj, ty)| (proj, self.ty(ty, generics)))
            .collect();

        sir::Place {
            local,
            proj,
            is_mutable,
        }
    }

    fn finish(self) -> sir::Program {
        self.specialized
    }
}
