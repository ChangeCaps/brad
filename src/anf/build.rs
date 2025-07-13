use crate::anf::{Bid, Body, Expr, ExprKind, Local, Program, Tid, Type, Value};
use crate::hir2 as hir;
use crate::hir2::Binding;
use solve::Tag;
use std::collections::HashMap;

pub struct BuildContext<'a> {
    ir: Program,
    hir: &'a hir::SpecializedProgram,
}

impl<'a> BuildContext<'a> {
    pub fn build(program: &hir::SpecializedProgram) -> Program {
        let ir = Program::default();

        let mut ctx = BuildContext { ir, hir: program };

        for (_, body) in program.bodies.iter() {
            println!("Building body: {}", body.name);
            ctx.build_body(body);
        }

        ctx.ir
    }

    pub fn build_body(&mut self, hir_body: &hir::SpecializedBody) -> Bid {
        let locals = if hir_body.is_extern {
            Vec::new()
        } else {
            Vec::with_capacity(hir_body.input.len() + hir_body.locals.len())
        };

        let mut body = Body {
            name: Some(hir_body.name.clone()),
            attrs: hir_body.attrs.clone(),
            is_extern: hir_body.is_extern,
            locals,
            arguments: hir_body.input.len(),
            output: self.ir.types.insert(hir_body.output.clone()),
            exprs: Vec::new(),
            span: hir_body.span,
        };

        let mut body_ctx = BodyBuildContext {
            ctx: self,
            body: &mut body,
            hir_body,
            local_map: HashMap::new(),
        };

        body_ctx.build();

        self.ir.bodies.insert(body)
    }
}

struct BodyBuildContext<'a, 'b> {
    ctx: &'a mut BuildContext<'b>,
    body: &'a mut Body,
    hir_body: &'a hir::SpecializedBody,
    /// Map between HIR locals and ANF locals.
    local_map: HashMap<hir::LocalId, Local>,
}

impl<'a, 'b> BodyBuildContext<'a, 'b> {
    pub fn build(&mut self) {
        // arguments are the first N locals.
        for arg in &self.hir_body.input {
            let tid = self.ctx.ir.types.insert(arg.ty.clone());
            self.body.locals.push(tid);
        }

        // build exprs after allocating arguments.
        if let Some(hir_expr) = &self.hir_body.expr {
            let expr_value = self.build_expr(hir_expr);
            self.body.exprs.push(Expr {
                kind: ExprKind::Return { val: expr_value },
                ty: self.body.output,
                span: hir_expr.span,
            })
        } else {
            let none_value = self.next_local(self.body.output);

            // If there is no expression, we still need to return a unit value.
            self.body.exprs.push(Expr {
                kind: ExprKind::TagInit {
                    dst: none_value,
                    tag: Tag::NONE,
                },
                ty: self.body.output,
                span: self.hir_body.span,
            });

            self.body.exprs.push(Expr {
                kind: ExprKind::Return {
                    val: Value::Local(none_value),
                },
                ty: self.body.output,
                span: self.hir_body.span,
            });
        }
    }

    pub fn prev_local(&self) -> Local {
        Local(self.body.locals.len() - 1)
    }

    pub fn next_local(&mut self, tid: Tid) -> Local {
        let local = Local(self.body.locals.len());
        self.body.locals.push(tid);
        local
    }

    pub fn build_expr_local(&mut self, hir_expr: &hir::Expr<Type>, local: hir::LocalId) -> Local {
        if let Some(&local_id) = self.local_map.get(&local) {
            local_id
        } else {
            let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
            let local_id = self.next_local(tid);
            self.local_map.insert(local, local_id);
            local_id
        }
    }

    pub fn build_expr(&mut self, hir_expr: &hir::Expr<Type>) -> Value {
        match hir_expr.kind {
            hir::ExprKind::Int(v) => Value::Int(v),
            hir::ExprKind::Float(v) => Value::Float(v),
            hir::ExprKind::ZeroSize(v) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let local = self.next_local(tid);

                self.body.exprs.push(Expr {
                    kind: ExprKind::TagInit { dst: local, tag: v },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::String(v) => Value::String(v),
            hir::ExprKind::Local(local) => Value::Local(self.build_expr_local(hir_expr, local)),
            hir::ExprKind::Tag(_, _) => {
                todo!()
            }
            hir::ExprKind::Func(_) => {
                todo!()
            }
            hir::ExprKind::Array(_) => {
                todo!()
            }
            hir::ExprKind::Tuple(_) => {
                todo!()
            }
            hir::ExprKind::Record(_) => {
                todo!()
            }
            hir::ExprKind::Index(_, _) => {
                todo!()
            }
            hir::ExprKind::Field(_, _) => {
                todo!()
            }
            hir::ExprKind::Unary(op, ref arg) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let val = self.build_expr(arg);
                let local = self.next_local(tid);
                self.body.exprs.push(Expr {
                    kind: ExprKind::Unary {
                        op: op.into(),
                        dst: local,
                        val,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::Binary(op, ref lhs, ref rhs) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let lhs = self.build_expr(lhs);
                let rhs = self.build_expr(rhs);
                let local = self.next_local(tid);
                self.body.exprs.push(Expr {
                    kind: ExprKind::Binary {
                        op: op.into(),
                        dst: local,
                        lhs,
                        rhs,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::Call(_, _) => {
                todo!()
            }
            hir::ExprKind::Lambda { .. } => {
                todo!()
            }
            hir::ExprKind::Assign(ref lhs, ref rhs) => {
                let Value::Local(local) = self.build_expr(lhs) else {
                    panic!("Expected a local value for assignment, found: {:?}", lhs);
                };

                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let rhs = self.build_expr(rhs);

                self.body.exprs.push(Expr {
                    kind: ExprKind::Mov {
                        dst: local,
                        src: rhs,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                let none_value = self.next_local(tid);

                self.body.exprs.push(Expr {
                    kind: ExprKind::TagInit {
                        dst: none_value,
                        tag: Tag::NONE,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(none_value)
            }
            hir::ExprKind::Ref(_) => {
                todo!()
            }
            hir::ExprKind::Match(_, _) => {
                todo!()
            }
            hir::ExprKind::Loop(_) => {
                todo!()
            }
            hir::ExprKind::Break(_) => {
                todo!()
            }
            hir::ExprKind::Let(ref binding, ref expr) => {
                let local = match binding {
                    Binding::Wild { .. } => todo!(),
                    Binding::Tuple { .. } => todo!(),
                    Binding::Bind { local, .. } => self.build_expr_local(hir_expr, *local),
                };

                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let expr_value = self.build_expr(expr.as_ref());
                self.body.exprs.push(Expr {
                    kind: ExprKind::Mov {
                        dst: local,
                        src: expr_value,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                let none_value = self.next_local(tid);
                self.body.exprs.push(Expr {
                    kind: ExprKind::TagInit {
                        dst: none_value,
                        tag: Tag::NONE,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });
                Value::Local(none_value)
            }
            hir::ExprKind::Block(ref hir_exprs) => {
                let block_tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let mut last_value = None;

                for expr in hir_exprs {
                    last_value = Some(self.build_expr(expr));
                }

                if last_value.is_none() {
                    // If the block is empty, we need to return a unit value.
                    let local = self.next_local(block_tid);
                    self.body.exprs.push(Expr {
                        kind: ExprKind::TagInit {
                            dst: local,
                            tag: Tag::NONE,
                        },
                        ty: block_tid,
                        span: hir_expr.span,
                    });

                    last_value = Some(Value::Local(local));
                }

                last_value.unwrap()
            }
        }
    }
}
