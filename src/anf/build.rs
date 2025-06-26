use crate::anf::Type;
use crate::hir2::ExprKind;
use crate::{anf, hir2 as hir};

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
            expr: None,
            span: hir_body.span,
        };

        let mut body_ctx = BodyBuildContext {
            ctx: self,
            body: &mut body,
            hir_body,
        };

        body_ctx.build();

        self.ir.bodies.insert(body)
    }
}

struct BodyBuildContext<'a, 'b> {
    ctx: &'a mut BuildContext<'b>,
    body: &'a mut anf::Body,
    hir_body: &'a hir::SpecializedBody,
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
            self.body.expr = Some(self.build_expr(hir_expr));
        }
    }

    pub fn build_expr(&mut self, hir_expr: &hir::Expr<Type>) -> anf::Expr {
        match hir_expr.kind {
            ExprKind::Int(_) => {
                todo!()
            }
            ExprKind::Float(_) => {
                todo!()
            }
            ExprKind::ZeroSize(_) => {
                todo!()
            }
            ExprKind::String(_) => {
                todo!()
            }
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
