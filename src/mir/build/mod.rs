use std::collections::HashMap;

use crate::hir::BinaryOp;
use crate::{diagnostic::Diagnostic, hir, mir};

macro_rules! unpack {
    ($block:ident = $block_and:expr) => {{
        let block_and = $block_and;
        $block = block_and.block;
        block_and.value
    }};
}

struct BlockAnd<T> {
    block: mir::Block,
    value: T,
}

pub fn build(hir: &hir::Program) -> Result<(mir::Program, mir::BodyId), Diagnostic> {
    let mut bodies = HashMap::new();
    let mut types = HashMap::new();
    let mut mir = mir::Program::new();

    for (hir_id, _) in hir.bodies.iter() {
        let mir_id = mir.bodies.push(mir::Body {
            captures: Vec::new(),
            locals: mir::Locals::new(),
            block: mir::Block::new(),
        });

        bodies.insert(hir_id, mir_id);
    }

    for (hir_id, hir_body) in hir.bodies.iter() {
        let mut builder = Builder::new(&mut bodies, &mut types, &mut mir, hir, hir_body);
        let mut inputs = Vec::new();
        let mut block = mir::Block::new();

        for input in hir_body.input.iter() {
            let ty = builder.build_tid(input.ty.clone());
            let local = builder.locals.push(ty);
            inputs.push(local);
        }

        for (input, local) in hir_body.input.iter().zip(inputs) {
            let place = mir::Place {
                local,
                proj: Vec::new(),
            };

            unpack!(block = builder.build_binding(block, input.binding.clone(), place)?);
        }

        let value = unpack!(block = builder.build_value(block, hir_body.expr.clone())?);

        block.term = mir::Term::Return(value);

        let mut body = mir::Body {
            captures: Vec::new(),
            locals: builder.locals.clone(),
            block,
        };

        // generate nested closures to capture outer arguments
        for (i, _) in hir_body.input.iter().enumerate().rev().skip(1) {
            let mut locals = mir::Locals::new();
            let mut captures = Vec::new();

            for input in hir_body.input[..=i].iter() {
                let tid = builder.build_tid(input.ty.clone());
                body.captures.push(tid);
                let local = locals.push(tid);

                captures.push(mir::Operand::Place(mir::Place {
                    local,
                    proj: Vec::new(),
                }));
            }

            let id = builder.mir.bodies.push(body.clone());

            let generics = hir_body
                .generics
                .params
                .iter()
                .map(|param| builder.build_tid(hir::Ty::Generic(param.generic)))
                .collect();

            body = mir::Body {
                captures: Vec::new(),
                locals,
                block: mir::Block {
                    stmts: Vec::new(),
                    term: mir::Term::Return(mir::Value::Closure(id, captures, generics)),
                },
            };
        }

        mir.bodies[bodies[&hir_id]] = body;
    }

    let (_, &main) = bodies
        .iter()
        .find(|(&id, _)| hir[id].name == "main")
        .unwrap();

    Ok((mir, main))
}

impl<T> BlockAnd<T> {
    fn new(block: mir::Block, value: T) -> Self {
        Self { block, value }
    }

    fn unpack(self) -> (mir::Block, T) {
        (self.block, self.value)
    }
}

type BuildResult<T> = Result<BlockAnd<T>, Diagnostic>;

struct Builder<'a> {
    bodies: &'a mut HashMap<hir::BodyId, mir::BodyId>,
    types: &'a mut HashMap<hir::Ty, mir::Tid>,
    mir: &'a mut mir::Program,
    hir: &'a hir::Program,
    body: &'a hir::Body,

    locals: mir::Locals,
    local_map: HashMap<hir::LocalId, mir::Local>,
}

impl<'a> Builder<'a> {
    fn new(
        bodies: &'a mut HashMap<hir::BodyId, mir::BodyId>,
        types: &'a mut HashMap<hir::Ty, mir::Tid>,
        mir: &'a mut mir::Program,
        hir: &'a hir::Program,
        body: &'a hir::Body,
    ) -> Self {
        let locals = mir::Locals::new();

        Self {
            bodies,
            types,
            mir,
            hir,
            locals,
            body,
            local_map: HashMap::new(),
        }
    }

    fn build_place(&mut self, mut block: mir::Block, expr: hir::Expr) -> BuildResult<mir::Place> {
        match expr.kind {
            hir::ExprKind::Local(local) => {
                let local = self.build_local(local);
                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                };

                Ok(BlockAnd::new(block, place))
            }

            hir::ExprKind::Field(target, field) => {
                let hir::Ty::Record(fields) = target.ty.clone() else {
                    panic!()
                };

                let mut target = unpack!(block = self.build_place(block, *target)?);

                let index = fields.iter().position(|f| f.name == field).unwrap();

                target.proj.push(mir::Proj::Field(index));
                Ok(BlockAnd::new(block, target))
            }

            hir::ExprKind::Int(_)
            | hir::ExprKind::Float(_)
            | hir::ExprKind::True
            | hir::ExprKind::False
            | hir::ExprKind::None
            | hir::ExprKind::String(_)
            | hir::ExprKind::Func(_, _)
            | hir::ExprKind::List(_)
            | hir::ExprKind::Record(_)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::Unary(_, _)
            | hir::ExprKind::Binary(_, _, _)
            | hir::ExprKind::Call(_, _)
            | hir::ExprKind::Assign(_, _)
            | hir::ExprKind::Ref(_)
            | hir::ExprKind::Match(_, _)
            | hir::ExprKind::Loop(_)
            | hir::ExprKind::Break(_)
            | hir::ExprKind::Let(_, _, _)
            | hir::ExprKind::Block(_) => {
                let ty = self.build_tid(expr.ty.clone());
                let local = self.locals.push(ty);

                let value = unpack!(block = self.build_value(block, expr)?);

                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                };

                block.stmts.push(mir::Stmt::Assign(place.clone(), value));

                Ok(BlockAnd::new(block, place))
            }
        }
    }

    fn build_operand(
        &mut self,
        mut block: mir::Block,
        expr: hir::Expr,
    ) -> BuildResult<mir::Operand> {
        match expr.kind {
            hir::ExprKind::Int(int) => {
                let operand = mir::Operand::Const(mir::Const::Int(int));
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::Float(float) => {
                let operand = mir::Operand::Const(mir::Const::Float(float));
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::True | hir::ExprKind::False | hir::ExprKind::None => {
                let operand = mir::Operand::ZST;
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::String(string) => {
                let operand = mir::Operand::Const(mir::Const::String(string));
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::Assign(lhs, rhs) => {
                let l = unpack!(block = self.build_place(block, lhs.as_ref().clone())?);
                let r = unpack!(block = self.build_value(block, rhs.as_ref().clone())?);
                let r = unpack!(block = self.coerce_value(block, r, rhs.ty, lhs.ty)?);

                block.stmts.push(mir::Stmt::Assign(l, r));

                Ok(BlockAnd::new(block, mir::Operand::ZST))
            }

            hir::ExprKind::Let(binding, ty, value) => {
                let place = unpack!(block = self.build_place(block, value.as_ref().clone())?);
                let place = unpack!(block = self.coerce_place(block, place, value.ty, ty)?);
                unpack!(block = self.build_binding(block, binding, place)?);

                Ok(BlockAnd::new(block, mir::Operand::ZST))
            }

            hir::ExprKind::Local(_)
            | hir::ExprKind::List(_)
            | hir::ExprKind::Record(_)
            | hir::ExprKind::Func(_, _)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::Field(_, _)
            | hir::ExprKind::Unary(_, _)
            | hir::ExprKind::Binary(_, _, _)
            | hir::ExprKind::Call(_, _)
            | hir::ExprKind::Ref(_)
            | hir::ExprKind::Match(_, _)
            | hir::ExprKind::Loop(_)
            | hir::ExprKind::Break(_)
            | hir::ExprKind::Block(_) => {
                let place = unpack!(block = self.build_place(block, expr)?);
                Ok(BlockAnd::new(block, mir::Operand::Place(place)))
            }
        }
    }

    fn build_value(&mut self, mut block: mir::Block, expr: hir::Expr) -> BuildResult<mir::Value> {
        match expr.kind {
            hir::ExprKind::Block(exprs) => {
                let mut value = mir::Value::ZST;

                for expr in exprs {
                    value = unpack!(block = self.build_value(block, expr)?);
                }

                Ok(BlockAnd::new(block, value))
            }

            hir::ExprKind::Record(fields) => {
                let mut values = Vec::new();

                for field in fields {
                    values.push(unpack!(block = self.build_operand(block, field.value)?));
                }

                Ok(BlockAnd::new(block, mir::Value::Record(values)))
            }

            hir::ExprKind::Func(body, tys) => {
                let body = self.bodies[&body];
                let tys = tys.iter().map(|ty| self.build_tid(ty.clone())).collect();

                let value = mir::Value::Closure(body, Vec::new(), tys);
                Ok(BlockAnd::new(block, value))
            }

            hir::ExprKind::Unary(op, expr) => {
                let hir_ty = expr.ty.clone();
                let mir_ty = self.build_ty(hir_ty.clone());

                let op = match (op, mir_ty, hir_ty) {
                    (hir::UnaryOp::Neg, mir::Ty::Int, _) => mir::UnaryOp::Negi,
                    (hir::UnaryOp::Neg, mir::Ty::Float, _) => mir::UnaryOp::Negf,
                    (hir::UnaryOp::BitNot, mir::Ty::Int, _) => mir::UnaryOp::BitNoti,
                    (hir::UnaryOp::Deref, mir::Ty::Ref(_), _) => mir::UnaryOp::Deref,
                    (hir::UnaryOp::Not, _, hir::Ty::True | hir::Ty::False | hir::Ty::None) => {
                        mir::UnaryOp::Not
                    }
                    (_, expr_ty, _) => {
                        return Err(Diagnostic::error("invalid::type::operator")
                            .message(format!("Type {:?} is not valid for {:?}", expr_ty, op))
                            .span(expr.span));
                    }
                };

                let operand = unpack!(block = self.build_operand(block, *expr)?);
                let value = mir::Value::Unary(op, operand);
                Ok(BlockAnd::new(block, value))
            }

            hir::ExprKind::Binary(op, lhs, rhs) => {
                let lhs_ty = self.build_ty(lhs.ty.clone());

                let lhs = unpack!(block = self.build_operand(block, *lhs)?);

                let rhs_ty = self.build_ty(rhs.ty.clone());
                let rhs = unpack!(block = self.build_operand(block, *rhs)?);

                let op = match (op, rhs_ty, lhs_ty) {
                    (BinaryOp::Add, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Addi,
                    (BinaryOp::Add, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Addf,
                    (BinaryOp::Sub, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Subi,
                    (BinaryOp::Sub, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Subf,
                    (BinaryOp::Mul, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Muli,
                    (BinaryOp::Mul, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Mulf,
                    (BinaryOp::Div, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Divi,
                    (BinaryOp::Div, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Divf,
                    (BinaryOp::Mod, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Modi,
                    (BinaryOp::Mod, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Modf,
                    (BinaryOp::Eq, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Eqi,
                    (BinaryOp::Eq, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Eqf,
                    (BinaryOp::Ne, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Nei,
                    (BinaryOp::Ne, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Nef,
                    (BinaryOp::Lt, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Lti,
                    (BinaryOp::Lt, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Ltf,
                    (BinaryOp::Le, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Lei,
                    (BinaryOp::Le, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Lef,
                    (BinaryOp::Gt, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Gti,
                    (BinaryOp::Gt, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Gtf,
                    (BinaryOp::Ge, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Gei,
                    (BinaryOp::Ge, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::Gef,
                    (BinaryOp::And, _, _) => todo!(),
                    (BinaryOp::Or, _, _) => todo!(),
                    (_, lhs_ty, rhs_ty) => {
                        return Err(Diagnostic::error("invalid::type::operator")
                            .message(format!(
                                "Type and operation {:?} {:?} {:?} is not valid",
                                lhs_ty, op, rhs_ty
                            ))
                            .span(expr.span));
                    }
                };

                let value = mir::Value::Binary(op, lhs, rhs);

                Ok(BlockAnd::new(block, value))
            }

            hir::ExprKind::Call(func, input) => {
                let hir::Ty::Func(i_ty, _) = func.ty.clone() else {
                    panic!()
                };

                let f = unpack!(block = self.build_operand(block, func.as_ref().clone())?);
                let i = unpack!(block = self.build_operand(block, input.as_ref().clone())?);
                let i = unpack!(block = self.coerce_operand(block, i, input.ty, *i_ty)?);

                let value = mir::Value::Call(f, i);
                Ok(BlockAnd::new(block, value))
            }

            hir::ExprKind::Int(_)
            | hir::ExprKind::Float(_)
            | hir::ExprKind::True
            | hir::ExprKind::False
            | hir::ExprKind::None
            | hir::ExprKind::String(_)
            | hir::ExprKind::Local(_)
            | hir::ExprKind::List(_)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::Field(_, _)
            | hir::ExprKind::Assign(_, _)
            | hir::ExprKind::Ref(_)
            | hir::ExprKind::Match(_, _)
            | hir::ExprKind::Loop(_)
            | hir::ExprKind::Break(_)
            | hir::ExprKind::Let(_, _, _) => {
                let operand = unpack!(block = self.build_operand(block, expr)?);
                Ok(BlockAnd::new(block, mir::Value::Use(operand)))
            }
        }
    }

    fn build_binding(
        &mut self,
        mut block: mir::Block,
        binding: hir::Binding,
        mut value: mir::Place,
    ) -> BuildResult<()> {
        match binding {
            hir::Binding::Wild { .. } => Ok(BlockAnd::new(block, ())),

            hir::Binding::Bind { local, .. } => {
                let local = self.build_local(local);
                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                };

                let value = mir::Value::Use(mir::Operand::Place(value));
                block.stmts.push(mir::Stmt::Assign(place, value));

                Ok(BlockAnd::new(block, ()))
            }

            hir::Binding::Tuple { bindings, .. } => {
                for (i, binding) in bindings.iter().enumerate() {
                    value.proj.push(mir::Proj::Field(i));

                    let result = self.build_binding(block, binding.clone(), value.clone())?;
                    unpack!(block = result);
                }

                Ok(BlockAnd::new(block, ()))
            }
        }
    }

    fn coerce_place(
        &mut self,
        mut block: mir::Block,
        place: mir::Place,
        from: hir::Ty,
        to: hir::Ty,
    ) -> BuildResult<mir::Place> {
        if from == to {
            return Ok(BlockAnd::new(block, place));
        }

        let tid = self.build_tid(to.clone());

        let operand = mir::Operand::Place(place.clone());
        let value = mir::Value::Use(operand);
        let value = unpack!(block = self.coerce_value(block, value, from, to)?);

        let local = self.locals.push(tid);
        let place = mir::Place {
            local,
            proj: Vec::new(),
        };

        block.stmts.push(mir::Stmt::Assign(place.clone(), value));

        Ok(BlockAnd::new(block, place))
    }

    fn coerce_operand(
        &mut self,
        mut block: mir::Block,
        operand: mir::Operand,
        from: hir::Ty,
        to: hir::Ty,
    ) -> BuildResult<mir::Operand> {
        if from == to {
            return Ok(BlockAnd::new(block, operand));
        }

        let tid = self.build_tid(to.clone());

        let value = mir::Value::Use(operand);
        let value = unpack!(block = self.coerce_value(block, value, from, to)?);

        let local = self.locals.push(tid);
        let place = mir::Place {
            local,
            proj: Vec::new(),
        };

        block.stmts.push(mir::Stmt::Assign(place.clone(), value));

        Ok(BlockAnd::new(block, mir::Operand::Place(place)))
    }

    fn coerce_value(
        &mut self,
        mut block: mir::Block,
        value: mir::Value,
        from: hir::Ty,
        to: hir::Ty,
    ) -> BuildResult<mir::Value> {
        if from == to {
            return Ok(BlockAnd::new(block, value));
        }

        let from = self.build_tid(from);

        match self.build_ty(to.clone()) {
            mir::Ty::Int
            | mir::Ty::Float
            | mir::Ty::Str
            | mir::Ty::Generic(_)
            | mir::Ty::Ref(_)
            | mir::Ty::List(_)
            | mir::Ty::Func(_, _)
            | mir::Ty::Record(_) => todo!(),

            mir::Ty::Union(tys) => {
                if let mir::Ty::Union(from_tys) = self.mir.types[from].clone() {
                    let local = self.locals.push(from);
                    let place = mir::Place {
                        local,
                        proj: Vec::new(),
                    };

                    block.stmts.push(mir::Stmt::Assign(place.clone(), value));

                    let operand = mir::Operand::Place(place);
                    let value = mir::Value::Coerce(from_tys, tys, operand);
                    return Ok(BlockAnd::new(block, value));
                }

                if !tys.contains(&from) {
                    panic!();
                }

                let local = self.locals.push(from);
                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                };

                block.stmts.push(mir::Stmt::Assign(place.clone(), value));

                let operand = mir::Operand::Place(place);
                let value = mir::Value::Promote(from, tys, operand);
                Ok(BlockAnd::new(block, value))
            }
        }
    }

    fn build_local(&mut self, hir: hir::LocalId) -> mir::Local {
        if let Some(&local) = self.local_map.get(&hir) {
            return local;
        }

        let ty = self.build_tid(self.body.locals[hir].ty.clone());
        let local = self.locals.push(ty);
        self.local_map.insert(hir, local);
        local
    }

    fn build_tid(&mut self, mut hir: hir::Ty) -> mir::Tid {
        match hir {
            hir::Ty::Union(ref mut tys) => {
                tys.sort_by(|a, b| a.cmp(b));
            }
            hir::Ty::Record(ref mut tys) => {
                tys.sort_by(|a, b| a.cmp(b));
            }
            _ => {}
        }

        if let Some(&tid) = self.types.get(&hir) {
            return tid;
        }

        let ty = self.build_ty(hir.clone());
        let tid = self.mir.types.push(ty);
        self.types.insert(hir, tid);
        tid
    }

    fn build_ty(&mut self, hir: hir::Ty) -> mir::Ty {
        match hir {
            hir::Ty::Int => mir::Ty::Int,
            hir::Ty::Float => mir::Ty::Float,
            hir::Ty::Str => mir::Ty::Str,
            hir::Ty::True | hir::Ty::False | hir::Ty::None | hir::Ty::Never => mir::Ty::ZST,

            hir::Ty::Generic(generic) => {
                let index = self
                    .body
                    .generics
                    .params
                    .iter()
                    .position(|param| param.generic == generic)
                    .unwrap();

                mir::Ty::Generic(index as u16)
            }

            hir::Ty::Named(id, ref tys) => {
                let spec = hir::Spec::new(&self.hir[id].generics, tys).unwrap();

                match self.hir[id].ty {
                    Some(ref ty) => {
                        let ty = spec.apply(ty.clone()).unwrap();
                        self.build_ty(ty)
                    }
                    None => mir::Ty::ZST,
                }
            }

            hir::Ty::Ref(ref ty) => mir::Ty::Ref(self.build_tid(ty.as_ref().clone())),

            hir::Ty::List(ref ty) => mir::Ty::List(self.build_tid(ty.as_ref().clone())),

            hir::Ty::Func(ref i, ref o) => {
                let i = self.build_tid(i.as_ref().clone());
                let o = self.build_tid(o.as_ref().clone());

                mir::Ty::Func(i, o)
            }

            hir::Ty::Tuple(ref tys) => {
                let tys = tys.iter().map(|ty| self.build_tid(ty.clone())).collect();

                mir::Ty::Record(tys)
            }

            hir::Ty::Union(ref tys) => {
                let tys = tys.iter().map(|ty| self.build_tid(ty.clone())).collect();

                mir::Ty::Union(tys)
            }

            hir::Ty::Record(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|f| self.build_tid(f.ty.clone()))
                    .collect();

                mir::Ty::Record(fields)
            }
        }
    }
}
