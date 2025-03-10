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

    fn body(&mut self, bid: mir::Bid, generics: &[sir::Tid]) -> sir::Bid {
        if let Some(&body) = self.bodies.get(&(bid, generics.to_vec())) {
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

        self.bodies.insert((bid, generics.to_vec()), spec);

        if self.mir.bodies[bid].attrs.find_value("intrinsic") == Some("string::format") {
            self.sir.bodies[spec] = self.format(bid, generics[0]);

            return spec;
        }

        let mut locals = sir::Locals::new();

        for (_, ty) in self.mir.bodies[bid].locals.iter() {
            locals.push(self.ty(ty.clone(), generics));
        }

        let block = self.mir.bodies[bid]
            .block
            .clone()
            .map(|b| self.block(b, generics));

        let name = {
            let generics: Vec<_> = generics
                .iter()
                .map(|tid| self.names[tid].as_str())
                .collect();

            if generics.is_empty() {
                self.mir.bodies[bid].name.clone()
            } else {
                self.mir.bodies[bid]
                    .name
                    .as_ref()
                    .map(|name| format!("{}<{}>", name, generics.join(", ")))
            }
        };

        let body = &self.mir.bodies[bid];

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

    fn format(&mut self, bid: mir::Bid, tid: sir::Tid) -> sir::Body {
        let str_tid = self.sir.types.get_or_insert(&sir::Ty::Str);
        let int_tid = self.sir.types.get_or_insert(&sir::Ty::Int);

        match self.sir.types[tid].clone() {
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

                block.push(sir::Stmt::Drop(input));
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

            sir::Ty::Func(_, _) => {
                let mut block = sir::Block::new();
                block.term(sir::Term::Return(sir::Value::Use(sir::Operand::Const(
                    sir::Const::String("fn"),
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

            sir::Ty::Tuple(fields) => {
                let mut locals = sir::Locals::new();
                let input = locals.push(tid);

                let mut block = sir::Block::new();

                let concat = self.str_concat_func(&mut locals, &mut block);

                let comma = sir::Operand::Const(sir::Const::String(","), str_tid);
                let space = sir::Operand::Const(sir::Const::String(" "), str_tid);

                let mut output = sir::Operand::Const(sir::Const::String(""), str_tid);

                for (i, tid) in fields.iter().copied().enumerate() {
                    let mut field = sir::Place::local(input);
                    field.proj.push((sir::Proj::Tuple(i), tid));

                    let format = self.body(bid, &[tid]);
                    let format = sir::Value::Closure {
                        body: format,
                        generics: Vec::new(),
                        captures: Vec::new(),
                    };

                    let format_ty = sir::Ty::Func(tid, str_tid);
                    let format_tid = self.sir.types.get_or_insert(&format_ty);

                    let temp = locals.push(format_tid);

                    block.push(sir::Stmt::Assign(sir::Place::local(temp), format));

                    let field = sir::Value::Call(
                        sir::Place::local(temp),   // format function
                        sir::Operand::Load(field), // field value
                    );

                    let temp = locals.push(str_tid);

                    block.push(sir::Stmt::Assign(sir::Place::local(temp), field));

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        sir::Operand::Load(sir::Place::local(temp)),
                    );

                    if i + 1 < fields.len() {
                        output = self.str_concat(
                            &mut locals,
                            &mut block,
                            concat.clone(),
                            output,
                            comma.clone(),
                        );

                        output = self.str_concat(
                            &mut locals,
                            &mut block,
                            concat.clone(),
                            output,
                            space.clone(),
                        );
                    }
                }

                for (local, _) in locals.iter() {
                    block.push(sir::Stmt::Drop(local));
                }

                block.term(sir::Term::Return(sir::Value::Use(output)));

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

            sir::Ty::Record(fields) => {
                let mut locals = sir::Locals::new();
                let input = locals.push(tid);

                let mut block = sir::Block::new();

                let concat = self.str_concat_func(&mut locals, &mut block);

                let open = sir::Operand::Const(sir::Const::String("{"), str_tid);
                let close = sir::Operand::Const(sir::Const::String("}"), str_tid);
                let space = sir::Operand::Const(sir::Const::String(" "), str_tid);
                let colon = sir::Operand::Const(sir::Const::String(":"), str_tid);
                let semi = sir::Operand::Const(sir::Const::String(";"), str_tid);

                let mut output = open;

                for (name, tid) in fields.into_iter() {
                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        space.clone(),
                    );

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        sir::Operand::Const(sir::Const::String(name), str_tid),
                    );

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        colon.clone(),
                    );

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        space.clone(),
                    );

                    let mut field = sir::Place::local(input);
                    field.proj.push((sir::Proj::Field(name), tid));

                    let format = self.body(bid, &[tid]);
                    let format = sir::Value::Closure {
                        body: format,
                        generics: Vec::new(),
                        captures: Vec::new(),
                    };

                    let format_ty = sir::Ty::Func(tid, str_tid);
                    let format_tid = self.sir.types.get_or_insert(&format_ty);

                    let temp = locals.push(format_tid);

                    block.push(sir::Stmt::Assign(sir::Place::local(temp), format));

                    let field = sir::Value::Call(
                        sir::Place::local(temp),   // format function
                        sir::Operand::Load(field), // field value
                    );

                    let temp = locals.push(str_tid);

                    block.push(sir::Stmt::Assign(sir::Place::local(temp), field));

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        sir::Operand::Load(sir::Place::local(temp)),
                    );

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        semi.clone(),
                    );

                    output = self.str_concat(
                        &mut locals,
                        &mut block,
                        concat.clone(),
                        output,
                        space.clone(),
                    );
                }

                output = self.str_concat(&mut locals, &mut block, concat.clone(), output, close);

                for (local, _) in locals.iter() {
                    block.push(sir::Stmt::Drop(local));
                }

                block.term(sir::Term::Return(sir::Value::Use(output)));

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

            sir::Ty::Union(variants) => {
                let mut locals = sir::Locals::new();
                let input = locals.push(tid);

                let mut block = sir::Block::new();

                let mut cases = Vec::new();

                for variant in variants {
                    let mut block = sir::Block::new();
                    let local = locals.push(variant);

                    let format = self.body(bid, &[variant]);
                    let format = sir::Value::Closure {
                        body: format,
                        generics: Vec::new(),
                        captures: Vec::new(),
                    };

                    let format_ty = sir::Ty::Func(variant, str_tid);
                    let format_tid = self.sir.types.get_or_insert(&format_ty);

                    let temp = locals.push(format_tid);

                    block.push(sir::Stmt::Assign(sir::Place::local(temp), format));

                    let value = sir::Value::Call(
                        sir::Place::local(temp),
                        sir::Operand::Load(sir::Place::local(local)),
                    );

                    block.push(sir::Stmt::Drop(temp));
                    block.push(sir::Stmt::Drop(local));
                    block.push(sir::Stmt::Drop(input));

                    block.term(sir::Term::Return(value));

                    cases.push(sir::Case {
                        ty: variant,
                        local,
                        block,
                    });
                }

                block.push(sir::Stmt::Match {
                    target: sir::Place::local(input),
                    cases,
                    default: sir::Block::new(),
                });

                block.term(sir::Term::Return(sir::Value::Use(sir::Operand::Const(
                    sir::Const::String(""),
                    str_tid,
                ))));

                sir::Body {
                    attrs: Attributes::new(),
                    is_extern: false,
                    name: Some(format!("std::string::format<{}>", self.names[&tid].clone())),
                    captures: 0,
                    arguments: 1,
                    output: str_tid,
                    locals,
                    block: Some(block),
                }
            }
        }
    }

    fn str_concat(
        &mut self,
        locals: &mut sir::Locals,
        block: &mut sir::Block,
        func: sir::Place,
        lhs: sir::Operand,
        rhs: sir::Operand,
    ) -> sir::Operand {
        let str_tid = self.sir.types.get_or_insert(&sir::Ty::Str);

        let str_str_ty = sir::Ty::Func(str_tid, str_tid);
        let str_str_tid = self.sir.types.get_or_insert(&str_str_ty);

        let func_temp = locals.push(str_str_tid);

        block.push(sir::Stmt::Assign(
            sir::Place::local(func_temp),
            sir::Value::Call(func, lhs),
        ));

        let output_temp = locals.push(str_tid);

        block.push(sir::Stmt::Assign(
            sir::Place::local(output_temp),
            sir::Value::Call(sir::Place::local(func_temp), rhs),
        ));

        sir::Operand::Load(sir::Place::local(output_temp))
    }

    fn str_concat_func(&mut self, locals: &mut sir::Locals, block: &mut sir::Block) -> sir::Place {
        let bid = self
            .mir
            .bodies
            .iter()
            .find_map(|(id, body)| {
                body.attrs
                    .find_value("link")
                    .filter(|&v| v == "brad_str_concat")
                    .map(|_| id)
            })
            .unwrap();

        let bid = self.body(bid, &[]);

        let str_tid = self.sir.types.get_or_insert(&sir::Ty::Str);

        let str_str_ty = sir::Ty::Func(str_tid, str_tid);
        let str_str_tid = self.sir.types.get_or_insert(&str_str_ty);

        let str_str_str_ty = sir::Ty::Func(str_tid, str_str_tid);
        let str_str_str_tid = self.sir.types.get_or_insert(&str_str_str_ty);

        let local = locals.push(str_str_str_tid);
        block.push(sir::Stmt::Assign(
            sir::Place::local(local),
            sir::Value::Closure {
                body: bid,
                generics: Vec::new(),
                captures: Vec::new(),
            },
        ));

        sir::Place::local(local)
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
            mir::Stmt::Drop(local) => sir::Stmt::Drop(local),

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
            mir::Operand::Load(place) => sir::Operand::Load(self.place(place, generics)),
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
