use crate::anf::{Arm, Bid, Body, Expr, ExprKind, Local, Program, Tid, Type, Value};
use crate::hir2 as hir;
use crate::hir2::{Binding, Pattern, SpecializedBodyId};
use solve::{Tag, Tags};
use std::collections::HashMap;

pub struct BuildContext<'a> {
    ir: Program,
    hir: &'a hir::SpecializedProgram,
    bid_map: HashMap<hir::SpecializedBodyId, Bid>,
}

impl<'a> BuildContext<'a> {
    pub fn build(program: &hir::SpecializedProgram) -> Program {
        let ir = Program::default();

        let mut ctx = BuildContext {
            ir,
            hir: program,
            bid_map: Default::default(),
        };

        // Hoist every body in the program.
        for (hir_body_id, hir_body) in program.bodies.iter() {
            ctx.build_body(hir_body_id, hir_body);
        }

        // Build every body in the program.
        for (hir_body_id, hir_body) in program.bodies.iter() {
            let bid = *ctx
                .bid_map
                .get(&hir_body_id)
                .expect("Body ID not found in map");

            let mut body_ctx = BodyBuildContext {
                ctx: &mut ctx,
                hir_body,
                bid,
                local_map: HashMap::new(),
                alt_block: None,
            };

            body_ctx.build();
        }

        ctx.ir
    }

    pub fn build_body(
        &mut self,
        hir_body_id: hir::SpecializedBodyId,
        hir_body: &hir::SpecializedBody,
    ) -> Bid {
        let locals = if hir_body.is_extern {
            Vec::new()
        } else {
            Vec::with_capacity(hir_body.input.len() + hir_body.locals.len())
        };

        let body = Body {
            name: Some(hir_body.name.clone()),
            attrs: hir_body.attrs.clone(),
            is_extern: hir_body.is_extern,
            locals,
            arguments: hir_body.input.len(),
            output: self.ir.types.insert(hir_body.output.clone()),
            exprs: Vec::new(),
            span: hir_body.span,
        };

        let bid = self.ir.bodies.insert(body);
        self.bid_map.insert(hir_body_id, bid);
        bid
    }
}

struct BodyBuildContext<'a, 'b> {
    ctx: &'a mut BuildContext<'b>,
    hir_body: &'a hir::SpecializedBody,
    bid: Bid,
    /// Map between HIR locals and ANF locals.
    local_map: HashMap<hir::LocalId, Local>,
    alt_block: Option<Vec<Expr>>,
}

impl<'a, 'b> BodyBuildContext<'a, 'b> {
    pub fn body_mut(&mut self) -> &mut Body {
        &mut self.ctx.ir.bodies[self.bid]
    }

    pub fn body(&self) -> &Body {
        &self.ctx.ir.bodies[self.bid]
    }

    pub fn body_output(&self) -> Tid {
        self.body().output
    }

    pub fn add_expr(&mut self, expr: Expr) {
        if let Some(alt_block) = &mut self.alt_block {
            alt_block.push(expr);
            return;
        }

        self.body_mut().exprs.push(expr);
    }

    pub fn build(&mut self) {
        // arguments are the first N locals.
        for arg in &self.hir_body.input {
            let tid = self.ctx.ir.types.insert(arg.ty.clone());
            self.body_mut().locals.push(tid);
        }

        // build exprs after allocating arguments.
        if let Some(hir_expr) = &self.hir_body.expr {
            let expr_value = self.build_expr(hir_expr);
            self.add_expr(Expr {
                kind: ExprKind::Return { val: expr_value },
                ty: self.body_output(),
                span: hir_expr.span,
            })
        } else {
            let none_value = self.next_local(self.body_output());

            // If there is no expression, we still need to return a unit value.
            self.add_expr(Expr {
                kind: ExprKind::TagInit {
                    dst: none_value,
                    tag: Tag::NONE,
                },
                ty: self.body_output(),
                span: self.hir_body.span,
            });

            self.add_expr(Expr {
                kind: ExprKind::Return {
                    val: Value::Local(none_value),
                },
                ty: self.body_output(),
                span: self.hir_body.span,
            });
        }
    }

    pub fn prev_local(&mut self) -> Local {
        Local(self.body_mut().locals.len() - 1)
    }

    pub fn next_local(&mut self, tid: Tid) -> Local {
        let local = Local(self.body_mut().locals.len());
        self.body_mut().locals.push(tid);
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

                self.add_expr(Expr {
                    kind: ExprKind::TagInit { dst: local, tag: v },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::String(v) => Value::String(v),
            hir::ExprKind::Local(local) => Value::Local(self.build_expr_local(hir_expr, local)),
            hir::ExprKind::Tag(ref tag, ref expr) => {
                // Construct union with one case, if expr is a union append the case.
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let local = self.next_local(tid);
                let expr_value = self.build_expr(expr);
                let ty = &self.ctx.ir.types[tid];

                let mut tags = Tags::new();

                for term in &ty.terms {
                    tags.union(&term.tags);
                }

                tags.insert(*tag);

                self.add_expr(Expr {
                    kind: ExprKind::UnionInit {
                        dst: local,
                        val: expr_value,
                        union: tags,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::Func(bid) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let local = self.next_local(tid);

                // bid is of type BodyId and needs to be casted to SpecializedBodyId
                let bid = SpecializedBodyId(bid.0);

                // Smack lamda into local
                self.add_expr(Expr {
                    kind: ExprKind::Closure {
                        dst: local,
                        func: *self
                            .ctx
                            .bid_map
                            .get(&bid)
                            .expect("Body ID not found in map"),
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
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
                self.add_expr(Expr {
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
                self.add_expr(Expr {
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
            hir::ExprKind::Call(ref lhs, ref rhs) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());

                let Value::Local(lhs) = self.build_expr(lhs) else {
                    panic!("Expected a local value for function call, found: {:?}", lhs);
                };

                let rhs = self.build_expr(rhs);

                let local = self.next_local(tid);

                self.add_expr(Expr {
                    kind: ExprKind::Call {
                        dst: local,
                        src: lhs,
                        arg: rhs,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
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

                self.add_expr(Expr {
                    kind: ExprKind::Mov {
                        dst: local,
                        src: rhs,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                let none_value = self.next_local(tid);

                self.add_expr(Expr {
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
            hir::ExprKind::Match(ref expr, ref body) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let local = self.next_local(tid); // return value
                let target = self.build_expr(expr);

                let mut anf_arms: Vec<(Tag, Arm)> = Vec::with_capacity(body.arms.len());

                for arm in &body.arms {
                    let arm_exprs = Vec::new();
                    self.alt_block = Some(arm_exprs);

                    let Pattern::Tag { tag, binding, .. } = &arm.pattern;
                    let arm_local = match binding {
                        Binding::Wild { .. } => Local(0),
                        Binding::Tuple { .. } => todo!(),
                        Binding::Bind { local, .. } => self.build_expr_local(expr, *local),
                    };

                    let arm_value = self.build_expr(&arm.body);

                    let arm_exprs = self.alt_block.take().unwrap();
                    self.add_expr(Expr {
                        kind: ExprKind::Mov {
                            dst: local,
                            src: arm_value,
                        },
                        ty: tid,
                        span: arm.body.span,
                    });

                    anf_arms.push((
                        *tag,
                        Arm {
                            local: arm_local,
                            body: arm_exprs,
                        },
                    ));
                }

                let mut default_arm: Option<Box<Arm>> = None;

                if let Some((binding, body_expr)) = &body.default.as_deref() {
                    let arm_local = match binding {
                        Binding::Wild { .. } => Local(0),
                        Binding::Tuple { .. } => todo!(),
                        Binding::Bind { local, .. } => self.build_expr_local(expr, *local),
                    };

                    let arm_exprs = Vec::new();
                    self.alt_block = Some(arm_exprs);
                    let arm_value = self.build_expr(body_expr);
                    let arm_exprs = self.alt_block.take().unwrap();
                    self.add_expr(Expr {
                        kind: ExprKind::Mov {
                            dst: local,
                            src: arm_value,
                        },
                        ty: tid,
                        span: body_expr.span,
                    });

                    default_arm = Some(Box::new(Arm {
                        local: arm_local,
                        body: arm_exprs,
                    }));
                }

                self.add_expr(Expr {
                    kind: ExprKind::Match {
                        dst: local,
                        target,
                        arms: anf_arms,
                        default: default_arm,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::Loop(ref expr) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let local = self.next_local(tid);
                let exprs = Vec::new();
                self.alt_block = Some(exprs);

                self.build_expr(expr);

                let exprs = self.alt_block.take().unwrap();

                self.add_expr(Expr {
                    kind: ExprKind::Loop {
                        dst: local,
                        body: exprs,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            hir::ExprKind::Break(ref expr) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());

                let value = if let Some(expr) = expr {
                    self.build_expr(expr)
                } else {
                    let local = self.next_local(tid);
                    self.add_expr(Expr {
                        kind: ExprKind::TagInit {
                            dst: local,
                            tag: Tag::NONE,
                        },
                        ty: tid,
                        span: hir_expr.span,
                    });
                    Value::Local(local)
                };

                self.add_expr(Expr {
                    kind: ExprKind::Break {
                        value: Some(value.clone()),
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                value
            }
            hir::ExprKind::Let(ref binding, ref expr) => {
                let local = match binding {
                    Binding::Wild { .. } => todo!(),
                    Binding::Tuple { .. } => todo!(),
                    Binding::Bind { local, .. } => self.build_expr_local(expr, *local),
                };

                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let expr_value = self.build_expr(expr.as_ref());
                self.add_expr(Expr {
                    kind: ExprKind::Mov {
                        dst: local,
                        src: expr_value,
                    },
                    ty: tid,
                    span: hir_expr.span,
                });

                let none_value = self.next_local(tid);
                self.add_expr(Expr {
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
                    self.add_expr(Expr {
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
