use std::collections::HashMap;

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

pub fn build(hir: &hir::Program) -> Result<mir::Program, Diagnostic> {
    let mut bodies = HashMap::new();
    let mut types = HashMap::new();

    let mut mir = mir::Program {
        bodies: mir::Bodies::new(),
        types: mir::Types::new(),
    };

    for (hir_id, _) in hir.bodies.iter() {
        let mir_id = mir.bodies.push(mir::Body::default());
        bodies.insert(hir_id, mir_id);
    }

    for (hir_id, hir_body) in hir.bodies.iter() {
        let mut builder = Builder::new(&mut bodies, &mut types, &mut mir, hir, hir_body);
        let mut inputs = Vec::new();
        let mut block = mir::Block::new();

        for input in hir_body.input.iter() {
            let ty = builder.build_ty(input.ty.clone());
            let local = builder.locals.push(ty);
            inputs.push(local);
        }

        for (input, local) in hir_body.input.iter().zip(inputs.iter()) {
            builder.scope.push(*local);

            let place = mir::Place {
                local: *local,
                proj: Vec::new(),
                is_mutable: false,
            };

            unpack!(block = builder.build_binding(block, input.binding.clone(), place)?);
        }

        let block = match hir_body.expr {
            Some(ref body) => {
                let value = unpack!(block = builder.build_value(block, body.clone())?);
                let value = unpack!(
                    block = builder.coerce_value(
                        block,
                        value,
                        body.ty.clone(),
                        hir_body.output.clone(),
                    )?
                );

                block = builder.drop_scope(block);
                block.term = Some(mir::Term::Return(value));

                Some(block)
            }
            None => None,
        };

        mir.bodies[bodies[&hir_id]] = mir::Body {
            attrs: hir_body.attrs.clone(),
            is_extern: hir_body.is_extern,
            name: Some(hir[hir_id].name.clone()),
            captures: 0,
            arguments: inputs.len(),
            output: builder.build_ty(hir_body.output.clone()),
            locals: builder.locals.clone(),
            block,
        };
    }

    Ok(mir)
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
    bodies: &'a mut HashMap<hir::BodyId, mir::Bid>,
    types: &'a mut HashMap<hir::NamedId, u32>,
    mir: &'a mut mir::Program,
    hir: &'a hir::Program,
    body: &'a hir::Body,

    scope: Vec<mir::Local>,

    locals: mir::Locals,
    local_map: HashMap<hir::LocalId, mir::Local>,

    break_local: Option<mir::Local>,
}

impl<'a> Builder<'a> {
    fn new(
        bodies: &'a mut HashMap<hir::BodyId, mir::Bid>,
        types: &'a mut HashMap<hir::NamedId, u32>,
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
            body,

            scope: Vec::new(),

            locals,
            local_map: HashMap::new(),

            break_local: None,
        }
    }

    fn drop_scope(&mut self, mut block: mir::Block) -> mir::Block {
        for local in self.scope.iter().rev().copied() {
            let value = mir::Operand::Move(mir::Place {
                local,
                proj: Vec::new(),
                is_mutable: false,
            });

            block.stmts.push(mir::Stmt::Drop(value));
        }

        block
    }

    fn build_place(&mut self, mut block: mir::Block, expr: hir::Expr) -> BuildResult<mir::Place> {
        match expr.kind {
            hir::ExprKind::Local(local) => {
                let place = mir::Place {
                    local: self.build_local(local),
                    proj: Vec::new(),
                    is_mutable: self.body.locals[local].is_mutable,
                };

                Ok(BlockAnd::new(block, place))
            }

            hir::ExprKind::Field(target, field) => {
                let ty = self.build_ty(target.ty.clone());

                let ty = match ty {
                    mir::Ty::Record(fields) => fields
                        .iter()
                        .find(|(name, _)| name == &field)
                        .map(|(_, ty)| ty.clone())
                        .unwrap(),
                    _ => todo!("{:?}", ty),
                };

                let mut target = unpack!(block = self.build_place(block, *target)?);

                target.proj.push((mir::Proj::Field(field), ty));
                Ok(BlockAnd::new(block, target))
            }

            hir::ExprKind::Match(target, arms) => {
                let target = unpack!(block = self.build_place(block, *target)?);

                let tid = self.build_ty(expr.ty.clone());
                let output = self.locals.push(tid);
                let mut cases = Vec::new();

                for arm in arms {
                    match arm.pattern {
                        hir::Pattern::Ty { ty, binding, .. } => {
                            let ty = self.build_ty(ty.clone());
                            let local = self.locals.push(ty.clone());

                            let place = mir::Place {
                                local,
                                proj: Vec::new(),
                                is_mutable: false,
                            };

                            // we need to keep track of the scope for each
                            // case so we don't drop things that aren't initialized
                            let scope = self.scope.len();

                            let BlockAnd { mut block, .. } =
                                self.build_binding(mir::Block::new(), binding, place.clone())?;

                            let value = unpack!(block = self.build_value(block, arm.expr)?);

                            for local in self.scope.drain(scope..).rev() {
                                let value = mir::Operand::Move(mir::Place {
                                    local,
                                    proj: Vec::new(),
                                    is_mutable: false,
                                });

                                block.stmts.push(mir::Stmt::Drop(value));
                            }

                            let output = mir::Place {
                                local: output,
                                proj: Vec::new(),
                                is_mutable: false,
                            };

                            block.stmts.push(mir::Stmt::Assign(output, value));

                            cases.push(mir::Case { ty, local, block });
                        }
                    }
                }

                block.stmts.push(mir::Stmt::Match {
                    target,
                    default: mir::Block::new(),
                    cases,
                });

                let output = mir::Place {
                    local: output,
                    proj: Vec::new(),
                    is_mutable: false,
                };

                Ok(BlockAnd::new(block, output))
            }

            hir::ExprKind::Loop(body) => {
                let ty = self.build_ty(expr.ty.clone());
                let local = self.locals.push(ty);

                let old_break_local = self.break_local;
                self.break_local = Some(local);

                let mut loop_block = mir::Block::new();

                let _ = unpack!(loop_block = self.build_place(loop_block, *body)?);

                self.break_local = old_break_local;

                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                    is_mutable: false,
                };

                block.stmts.push(mir::Stmt::Loop(loop_block));

                Ok(BlockAnd::new(block, place))
            }

            hir::ExprKind::Int(_)
            | hir::ExprKind::Float(_)
            | hir::ExprKind::True
            | hir::ExprKind::False
            | hir::ExprKind::None
            | hir::ExprKind::String(_)
            | hir::ExprKind::Func(_, _)
            | hir::ExprKind::List(_)
            | hir::ExprKind::Tuple(_)
            | hir::ExprKind::Record(_)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::Unary(_, _)
            | hir::ExprKind::Binary(_, _, _)
            | hir::ExprKind::Call(_, _)
            | hir::ExprKind::Assign(_, _)
            | hir::ExprKind::Ref(_)
            | hir::ExprKind::Break(_)
            | hir::ExprKind::Let(_, _, _)
            | hir::ExprKind::Block(_) => {
                let ty = self.build_ty(expr.ty.clone());
                let local = self.locals.push(ty);

                let value = unpack!(block = self.build_value(block, expr)?);

                self.scope.push(local);

                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                    is_mutable: false,
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
                let operand = mir::Operand::Const(mir::Const::Int(int), mir::Ty::Int);
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::Float(float) => {
                let operand = mir::Operand::Const(mir::Const::Float(float), mir::Ty::Float);
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::True | hir::ExprKind::False | hir::ExprKind::None => {
                let ty = match expr.kind {
                    hir::ExprKind::True => mir::Ty::True,
                    hir::ExprKind::False => mir::Ty::False,
                    hir::ExprKind::None => mir::Ty::None,
                    _ => unreachable!(),
                };

                let operand = mir::Operand::Const(mir::Const::None, ty);
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::String(string) => {
                let operand = mir::Operand::Const(mir::Const::String(string), mir::Ty::Str);
                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::Assign(lhs, rhs) => {
                let l = unpack!(block = self.build_place(block, lhs.as_ref().clone())?);
                let r = unpack!(block = self.build_value(block, rhs.as_ref().clone())?);
                let r = unpack!(block = self.coerce_value(block, r, rhs.ty, lhs.ty)?);

                if !l.is_mutable {
                    let diagnostic = Diagnostic::error("invalid::assign::immutable")
                        .message("Cannot assign to immutable place")
                        .span(lhs.span);

                    return Err(diagnostic);
                }

                block.stmts.push(mir::Stmt::Assign(l, r));

                Ok(BlockAnd::new(block, mir::Operand::NONE))
            }

            hir::ExprKind::Let(binding, ty, expr) => {
                let value = unpack!(block = self.build_value(block, expr.as_ref().clone())?);
                let value = unpack!(block = self.coerce_value(block, value, expr.ty, ty.clone())?);

                let ty = self.build_ty(ty);
                let local = self.locals.push(ty);

                self.scope.push(local);

                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                    is_mutable: false,
                };

                block.stmts.push(mir::Stmt::Assign(place.clone(), value));

                unpack!(block = self.build_binding(block, binding, place)?);

                Ok(BlockAnd::new(block, mir::Operand::NONE))
            }

            hir::ExprKind::Break(value) => {
                let local = self.break_local.unwrap();

                if let Some(value) = value {
                    let v = unpack!(block = self.build_value(block, *value)?);

                    let place = mir::Place {
                        local,
                        proj: Vec::new(),
                        is_mutable: false,
                    };

                    block.stmts.push(mir::Stmt::Assign(place, v));
                }

                block = self.drop_scope(block);

                block.term = Some(mir::Term::Break);

                Ok(BlockAnd::new(block, mir::Operand::NONE))
            }

            hir::ExprKind::Block(exprs) => {
                let mut operand = None;

                let scope = self.scope.len();

                for expr in exprs {
                    if let Some(operand) = operand {
                        block.stmts.push(mir::Stmt::Drop(operand));
                    }

                    operand = Some(unpack!(block = self.build_operand(block, expr)?));
                }

                for local in self.scope.drain(scope..).rev() {
                    let value = mir::Operand::Move(mir::Place {
                        local,
                        proj: Vec::new(),
                        is_mutable: false,
                    });

                    block.stmts.push(mir::Stmt::Drop(value));
                }

                let operand = operand.unwrap_or(mir::Operand::NONE);

                Ok(BlockAnd::new(block, operand))
            }

            hir::ExprKind::Local(_)
            | hir::ExprKind::List(_)
            | hir::ExprKind::Tuple(_)
            | hir::ExprKind::Record(_)
            | hir::ExprKind::Func(_, _)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::Field(_, _)
            | hir::ExprKind::Unary(_, _)
            | hir::ExprKind::Binary(_, _, _)
            | hir::ExprKind::Call(_, _)
            | hir::ExprKind::Ref(_)
            | hir::ExprKind::Match(_, _)
            | hir::ExprKind::Loop(_) => {
                let place = unpack!(block = self.build_place(block, expr)?);
                Ok(BlockAnd::new(block, mir::Operand::Copy(place)))
            }
        }
    }

    fn build_value(&mut self, mut block: mir::Block, expr: hir::Expr) -> BuildResult<mir::Value> {
        match expr.kind {
            hir::ExprKind::Tuple(exprs) => {
                let mut values = Vec::new();

                for expr in exprs {
                    values.push(unpack!(block = self.build_operand(block, expr)?));
                }

                Ok(BlockAnd::new(block, mir::Value::Tuple(values)))
            }

            hir::ExprKind::Record(fields) => {
                let mut values = Vec::new();

                for field in fields {
                    let operand = unpack!(block = self.build_operand(block, field.value)?);
                    values.push((field.name, operand));
                }

                Ok(BlockAnd::new(block, mir::Value::Record(values)))
            }

            hir::ExprKind::Func(body, generics) => {
                let body = self.bodies[&body];

                let generics = generics.into_iter().map(|ty| self.build_ty(ty)).collect();

                let value = mir::Value::Closure {
                    body,
                    captures: Vec::new(),
                    generics,
                };
                Ok(BlockAnd::new(block, value))
            }

            hir::ExprKind::Unary(op, expr) => {
                let hir_ty = expr.ty.clone();
                let mir_ty = self.build_ty(hir_ty.clone());

                let op = match (op, mir_ty, hir_ty) {
                    (hir::UnaryOp::Neg, mir::Ty::Int, _) => mir::UnaryOp::Neg,
                    (hir::UnaryOp::Neg, mir::Ty::Float, _) => mir::UnaryOp::FNeg,
                    (hir::UnaryOp::BitNot, mir::Ty::Int, _) => mir::UnaryOp::BNot,
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
                    (hir::BinaryOp::Add, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Add,
                    (hir::BinaryOp::Sub, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Sub,
                    (hir::BinaryOp::Mul, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Mul,
                    (hir::BinaryOp::Div, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Div,
                    (hir::BinaryOp::Mod, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Mod,

                    (hir::BinaryOp::Eq, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Eq,
                    (hir::BinaryOp::Ne, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Ne,
                    (hir::BinaryOp::Lt, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Lt,
                    (hir::BinaryOp::Le, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Le,
                    (hir::BinaryOp::Gt, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Gt,
                    (hir::BinaryOp::Ge, mir::Ty::Int, mir::Ty::Int) => mir::BinaryOp::Ge,

                    (hir::BinaryOp::Add, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FAdd,
                    (hir::BinaryOp::Sub, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FSub,
                    (hir::BinaryOp::Mul, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FMul,
                    (hir::BinaryOp::Div, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FDiv,
                    (hir::BinaryOp::Mod, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FMod,

                    (hir::BinaryOp::Eq, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FEq,
                    (hir::BinaryOp::Ne, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FNe,
                    (hir::BinaryOp::Lt, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FLt,
                    (hir::BinaryOp::Le, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FLe,
                    (hir::BinaryOp::Gt, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FGt,
                    (hir::BinaryOp::Ge, mir::Ty::Float, mir::Ty::Float) => mir::BinaryOp::FGe,

                    (hir::BinaryOp::And, _, _) => todo!(),
                    (hir::BinaryOp::Or, _, _) => todo!(),

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

                let f = unpack!(block = self.build_place(block, func.as_ref().clone())?);
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
            | hir::ExprKind::Block(_)
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
        value: mir::Place,
    ) -> BuildResult<()> {
        match binding {
            hir::Binding::Wild { .. } => Ok(BlockAnd::new(block, ())),

            hir::Binding::Bind { local, .. } => {
                let place = mir::Place {
                    local: self.build_local(local),
                    proj: Vec::new(),
                    is_mutable: false,
                };

                self.scope.push(place.local);

                let value = mir::Value::Use(mir::Operand::Copy(value));
                block.stmts.push(mir::Stmt::Assign(place, value));

                Ok(BlockAnd::new(block, ()))
            }

            hir::Binding::Tuple { bindings, .. } => {
                for (i, binding) in bindings.iter().enumerate() {
                    let mut value = value.clone();

                    let ty = match value.ty(&self.locals) {
                        mir::Ty::Tuple(tys) => tys[i].clone(),
                        _ => todo!(),
                    };

                    value.proj.push((mir::Proj::Tuple(i), ty));

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

        let tid = self.build_ty(to.clone());

        let operand = mir::Operand::Copy(place.clone());
        let value = mir::Value::Use(operand);
        let value = unpack!(block = self.coerce_value(block, value, from, to)?);

        let local = self.locals.push(tid);
        let place = mir::Place {
            local,
            proj: Vec::new(),
            is_mutable: false,
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

        let tid = self.build_ty(to.clone());

        let value = mir::Value::Use(operand);
        let value = unpack!(block = self.coerce_value(block, value, from, to)?);

        let local = self.locals.push(tid);
        let place = mir::Place {
            local,
            proj: Vec::new(),
            is_mutable: false,
        };

        block.stmts.push(mir::Stmt::Assign(place.clone(), value));

        Ok(BlockAnd::new(block, mir::Operand::Copy(place)))
    }

    fn coerce_value(
        &mut self,
        mut block: mir::Block,
        value: mir::Value,
        from: hir::Ty,
        to: hir::Ty,
    ) -> BuildResult<mir::Value> {
        let from = self.build_ty(from);
        let to = self.build_ty(to);

        if self.unwrap_type(from.clone()) == self.unwrap_type(to.clone()) {
            return Ok(BlockAnd::new(block, value));
        }

        let to = self.unwrap_type(to);

        if matches!(from, mir::Ty::Never) {
            return Ok(BlockAnd::new(block, mir::Value::NONE));
        }

        match to {
            mir::Ty::Int
            | mir::Ty::Float
            | mir::Ty::Str
            | mir::Ty::True
            | mir::Ty::False
            | mir::Ty::None
            | mir::Ty::Never
            | mir::Ty::Generic(_)
            | mir::Ty::Ref(_)
            | mir::Ty::List(_)
            | mir::Ty::Func(_, _)
            | mir::Ty::Tuple(_)
            | mir::Ty::Record(_) => todo!(),

            mir::Ty::Named(_, _) => unreachable!(),

            mir::Ty::Union(tys) => {
                if let mir::Ty::Union(from_tys) = from.clone() {
                    let local = self.locals.push(from);
                    let place = mir::Place {
                        local,
                        proj: Vec::new(),
                        is_mutable: false,
                    };

                    block.stmts.push(mir::Stmt::Assign(place.clone(), value));

                    let operand = mir::Operand::Copy(place);
                    let value = mir::Value::Coerce {
                        inputs: from_tys,
                        variants: tys,
                        operand,
                    };

                    return Ok(BlockAnd::new(block, value));
                }

                if !tys.contains(&from) {
                    panic!();
                }

                let local = self.locals.push(from.clone());
                let place = mir::Place {
                    local,
                    proj: Vec::new(),
                    is_mutable: false,
                };

                block.stmts.push(mir::Stmt::Assign(place.clone(), value));

                let operand = mir::Operand::Copy(place);
                let value = mir::Value::Promote {
                    variant: from,
                    variants: tys,
                    operand,
                };

                Ok(BlockAnd::new(block, value))
            }
        }
    }

    fn build_local(&mut self, hir: hir::LocalId) -> mir::Local {
        if let Some(&local) = self.local_map.get(&hir) {
            return local;
        }

        let ty = self.build_ty(self.body.locals[hir].ty.clone());
        let local = self.locals.push(ty);
        self.local_map.insert(hir, local);
        local
    }

    fn unwrap_type(&self, ty: mir::Ty) -> mir::Ty {
        match ty {
            mir::Ty::Named(id, ref generics) => self.mir.types[id].ty.clone().specialize(generics),
            ty => ty,
        }
    }

    fn build_ty(&mut self, hir: hir::Ty) -> mir::Ty {
        match hir {
            hir::Ty::Int => mir::Ty::Int,
            hir::Ty::Float => mir::Ty::Float,
            hir::Ty::Str => mir::Ty::Str,
            hir::Ty::True => mir::Ty::True,
            hir::Ty::False => mir::Ty::False,
            hir::Ty::None => mir::Ty::None,
            hir::Ty::Never => mir::Ty::Never,

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

            hir::Ty::Named(id, tys) => {
                let tys = tys.iter().map(|ty| self.build_ty(ty.clone())).collect();

                if let Some(&id) = self.types.get(&id) {
                    return mir::Ty::Named(id, tys);
                }

                let named = &self.hir[id];

                let named = mir::Named {
                    name: named.name.clone(),
                    generics: named.generics.params.len() as u16,
                    ty: match named.ty {
                        Some(ref ty) => self.build_ty(ty.clone()),
                        None => mir::Ty::None,
                    },
                };

                let mir_id = self.mir.types.push(named);
                self.types.insert(id, mir_id);

                mir::Ty::Named(mir_id, tys)
            }

            hir::Ty::Ref(ty) => mir::Ty::Ref(Box::new(self.build_ty(*ty))),

            hir::Ty::List(ty) => mir::Ty::List(Box::new(self.build_ty(*ty))),

            hir::Ty::Func(i, o) => {
                let i = self.build_ty(*i);
                let o = self.build_ty(*o);

                mir::Ty::Func(Box::new(i), Box::new(o))
            }

            hir::Ty::Tuple(tys) => {
                let tys = tys.into_iter().map(|ty| self.build_ty(ty)).collect();

                mir::Ty::Tuple(tys)
            }

            hir::Ty::Union(tys) => {
                let tys = tys.into_iter().map(|ty| self.build_ty(ty)).collect();

                mir::Ty::Union(tys)
            }

            hir::Ty::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|f| (f.name, self.build_ty(f.ty)))
                    .collect();

                mir::Ty::Record(fields)
            }
        }
    }
}
