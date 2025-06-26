use crate::anf::{Tid, Type, Value};
use crate::hir2::ExprKind;
use crate::{anf, hir2 as hir};
use solve::Tag;
use std::collections::HashMap;

struct BuildContext<'a> {
    ir: anf::Program,
    hir: &'a hir::SpecializedProgram,
}

impl<'a> BuildContext<'a> {
    pub fn build(program: &hir::SpecializedProgram) -> anf::Program {
        let ir = anf::Program::default();

        let mut ctx = BuildContext { ir, hir: program };

        for (_, body) in program.bodies.iter() {
            println!("Building body: {}", body.name);
            ctx.build_body(body);
        }

        ctx.ir
    }

    pub fn build_body(&mut self, hir_body: &hir::SpecializedBody) -> anf::Bid {
        let locals = if hir_body.is_extern {
            Vec::new()
        } else {
            Vec::with_capacity(hir_body.input.len() + hir_body.locals.len())
        };

        let mut body = anf::Body {
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
    body: &'a mut anf::Body,
    hir_body: &'a hir::SpecializedBody,
    local_map: HashMap<hir::LocalId, anf::Local>,
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
            self.body.exprs.push(anf::Expr {
                kind: anf::ExprKind::Return { val: expr_value },
                ty: self.body.output,
                span: hir_expr.span,
            })
        } else {
            let none_value = self.next_local(self.body.output);

            // If there is no expression, we still need to return a unit value.
            self.body.exprs.push(anf::Expr {
                kind: anf::ExprKind::TagInit {
                    dst: none_value,
                    tag: Tag::NONE,
                },
                ty: self.body.output,
                span: self.hir_body.span,
            });

            self.body.exprs.push(anf::Expr {
                kind: anf::ExprKind::Return {
                    val: Value::Local(none_value),
                },
                ty: self.body.output,
                span: self.hir_body.span,
            });
        }
    }

    pub fn prev_local(&self) -> anf::Local {
        anf::Local(self.body.locals.len() - 1)
    }

    pub fn next_local(&mut self, tid: Tid) -> anf::Local {
        let local = anf::Local(self.body.locals.len());
        self.body.locals.push(tid);
        local
    }

    pub fn build_expr(&mut self, hir_expr: &hir::Expr<Type>) -> Value {
        match hir_expr.kind {
            ExprKind::Int(v) => Value::Int(v),
            ExprKind::Float(v) => Value::Float(v),
            ExprKind::ZeroSize(v) => {
                let tid = self.ctx.ir.types.insert(hir_expr.ty.clone());
                let local = self.next_local(tid);

                self.body.exprs.push(anf::Expr {
                    kind: anf::ExprKind::TagInit { dst: local, tag: v },
                    ty: tid,
                    span: hir_expr.span,
                });

                Value::Local(local)
            }
            ExprKind::String(v) => Value::String(v),
            ExprKind::Local(_) => {
                todo!()
            }
            ExprKind::Tag(_, _) => {
                todo!()
            }
            ExprKind::Func(_) => {
                todo!()
            }
            ExprKind::Array(_) => {
                todo!()
            }
            ExprKind::Tuple(_) => {
                todo!()
            }
            ExprKind::Record(_) => {
                todo!()
            }
            ExprKind::Index(_, _) => {
                todo!()
            }
            ExprKind::Field(_, _) => {
                todo!()
            }
            ExprKind::Unary(_, _) => {
                todo!()
            }
            ExprKind::Binary(_, _, _) => {
                todo!()
            }
            ExprKind::Call(_, _) => {
                todo!()
            }
            ExprKind::Lambda { .. } => {
                todo!()
            }
            ExprKind::Assign(_, _) => {
                todo!()
            }
            ExprKind::Ref(_) => {
                todo!()
            }
            ExprKind::Match(_, _) => {
                todo!()
            }
            ExprKind::Loop(_) => {
                todo!()
            }
            ExprKind::Break(_) => {
                todo!()
            }
            ExprKind::Let(_, _) => {
                todo!()
            }
            ExprKind::Block(_) => {
                todo!()
            }
        }
    }
}
