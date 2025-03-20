use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::{Deref, DerefMut},
};

use crate::{
    ast,
    diagnostic::{Diagnostic, Span},
    hir2 as hir, solve,
};

use super::{FuncInfo, Generics, Lowerer};

impl Lowerer<'_> {
    pub(super) fn lower_function(
        &mut self,
        calls: &mut HashMap<hir::BodyId, Vec<hir::BodyId>>,
        body_id: hir::BodyId,
        info: FuncInfo,
    ) -> Result<(), ()> {
        let generics = info
            .ast
            .generics()
            .map(|p| p.name)
            .zip(self.program[body_id].generics.iter().cloned())
            .collect::<Vec<_>>();

        let mut lowerer = ExprLowerer {
            lowerer: self,

            calls,

            module: info.module,
            body: body_id,

            generics: &generics,

            scope: Vec::new(),
        };

        for (i, arg) in info.ast.args.iter().enumerate() {
            // fetch the type of the argument
            let ty = lowerer.program[body_id].input[i].ty.clone();

            // lower the binding
            let binding = lowerer.binding(&ty, &arg.binding)?;

            // replace the binding in the input
            lowerer.program[body_id].input[i].binding = binding;
        }

        self.program[body_id].expr = match info.ast.body {
            Some(ref body) => {
                let body = lowerer.expr(body)?;

                self.solver.subty(
                    &body.ty, // the body's type must be a subtype of the function output
                    &self.program[body_id].output,
                    body.span,
                );

                Some(body)
            }
            None => None,
        };

        Ok(())
    }
}

pub struct ExprLowerer<'a, 'r> {
    pub lowerer: &'a mut Lowerer<'r>,

    pub calls: &'a mut HashMap<hir::BodyId, Vec<hir::BodyId>>,

    pub module: hir::ModuleId,
    pub body: hir::BodyId,

    pub generics: &'a [(&'static str, solve::Ty)],

    pub scope: Vec<hir::LocalId>,
}

impl<'r> Deref for ExprLowerer<'_, 'r> {
    type Target = Lowerer<'r>;

    fn deref(&self) -> &Self::Target {
        self.lowerer
    }
}

impl DerefMut for ExprLowerer<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.lowerer
    }
}

impl ExprLowerer<'_, '_> {
    pub fn body(&self) -> &hir::Body {
        &self.program[self.body]
    }

    pub fn body_mut(&mut self) -> &mut hir::Body {
        let body = self.body;
        &mut self.program[body]
    }

    pub fn ty(&mut self, ast: &ast::Ty) -> Result<solve::Ty, ()> {
        self.lowerer.lower_ty(
            self.module,
            &mut Generics::Explicit(self.generics),
            true,
            ast,
        )
    }

    pub fn subty(&mut self, lhs: &solve::Ty, rhs: &solve::Ty, span: Span) {
        self.solver.subty(lhs, rhs, span);
    }

    fn recurses(&self, body_id: hir::BodyId) -> bool {
        let mut visited = HashSet::new();
        let mut queue = self.calls.get(&body_id).cloned().unwrap_or_default();

        while let Some(body_id) = queue.pop() {
            if body_id == self.body {
                return true;
            }

            if !visited.insert(body_id) {
                continue;
            }

            queue.extend(self.calls.get(&body_id).into_iter().flatten().copied());
        }

        false
    }

    fn binding(&mut self, ty: &solve::Ty, ast: &ast::Binding) -> Result<hir::Binding, ()> {
        Ok(match *ast {
            ast::Binding::Wild { span } => hir::Binding::Wild { span },

            ast::Binding::Bind {
                mutable,
                name,
                span,
            } => {
                let local = hir::Local {
                    is_mutable: mutable,
                    name,
                    ty: ty.clone(),
                    span,
                };

                let local = self.body_mut().locals.insert(local);

                self.scope.push(local);

                hir::Binding::Bind { local, span }
            }

            ast::Binding::Tuple { ref bindings, span } => {
                let mut hir_bindings = Vec::new();
                let mut tys = Vec::new();

                for binding in bindings {
                    let ty = solve::Ty::Var(self.solver.fresh_var());

                    let binding = self.binding(&ty, binding)?;
                    hir_bindings.push(binding);

                    tys.push(ty);
                }

                let tuple = solve::Ty::Tuple(tys);

                self.subty(ty, &tuple, span);

                hir::Binding::Tuple {
                    bindings: hir_bindings,
                    span,
                }
            }
        })
    }

    pub fn expr(&mut self, ast: &ast::Expr) -> Result<hir::Expr, ()> {
        match ast {
            ast::Expr::Literal(ast) => self.literal_expr(ast),
            ast::Expr::List(ast) => self.list_expr(ast),
            ast::Expr::Record(ast) => self.record_expr(ast),
            ast::Expr::Tuple(ast) => self.tuple_expr(ast),
            ast::Expr::Path(ast) => self.path_expr(ast),
            ast::Expr::Index(ast) => self.index_expr(ast),
            ast::Expr::Field(ast) => self.field_expr(ast),
            ast::Expr::Unary(ast) => self.unary_expr(ast),
            ast::Expr::Binary(ast) => self.binary_expr(ast),
            ast::Expr::Call(ast) => self.call_expr(ast),
            ast::Expr::Assign(ast) => self.assign_expr(ast),
            ast::Expr::Ref(ast) => self.ref_expr(ast),
            ast::Expr::Match(ast) => self.match_expr(ast),
            ast::Expr::Loop(ast) => self.loop_expr(ast),
            ast::Expr::Break(ast) => self.break_expr(ast),
            ast::Expr::Let(ast) => self.let_expr(ast),
            ast::Expr::Block(ast) => self.block_expr(ast),
        }
    }

    fn literal_expr(&mut self, ast: &ast::Literal) -> Result<hir::Expr, ()> {
        Ok(match ast {
            ast::Literal::Int { value, span } => hir::Expr {
                kind: hir::ExprKind::Int(*value),
                ty: solve::Ty::INT,
                span: *span,
            },

            ast::Literal::Float { value, span } => hir::Expr {
                kind: hir::ExprKind::Float(*value),
                ty: solve::Ty::FLOAT,
                span: *span,
            },

            ast::Literal::String { value, span } => hir::Expr {
                kind: hir::ExprKind::String(value),
                ty: solve::Ty::STR,
                span: *span,
            },

            ast::Literal::True { span } => hir::Expr {
                kind: hir::ExprKind::ZeroSize(solve::Tag::TRUE),
                ty: solve::Ty::TRUE,
                span: *span,
            },

            ast::Literal::False { span } => hir::Expr {
                kind: hir::ExprKind::ZeroSize(solve::Tag::FALSE),
                ty: solve::Ty::FALSE,
                span: *span,
            },

            ast::Literal::None { span } => hir::Expr {
                kind: hir::ExprKind::ZeroSize(solve::Tag::NONE),
                ty: solve::Ty::NONE,
                span: *span,
            },
        })
    }

    fn list_expr(&mut self, ast: &ast::ListExpr) -> Result<hir::Expr, ()> {
        let ty = solve::Ty::Var(self.solver.fresh_var());
        let mut items = Vec::new();

        for item in &ast.items {
            let item = self.expr(item)?;

            self.subty(&item.ty, &ty, item.span);

            items.push(item);
        }

        Ok(hir::Expr {
            kind: hir::ExprKind::List(items),
            ty: solve::Ty::list(ty),
            span: ast.span,
        })
    }

    fn record_expr(&mut self, ast: &ast::RecordExpr) -> Result<hir::Expr, ()> {
        let mut fields = Vec::new();
        let mut tys = BTreeMap::new();

        for field in &ast.fields {
            let value = self.expr(&field.value)?;

            tys.insert(field.name, value.ty.clone());

            fields.push(hir::Init {
                name: field.name,
                value,
                span: field.span,
            });
        }

        Ok(hir::Expr {
            kind: hir::ExprKind::Record(fields),
            ty: solve::Ty::Record(tys),
            span: ast.span,
        })
    }

    fn tuple_expr(&mut self, ast: &ast::TupleExpr) -> Result<hir::Expr, ()> {
        let mut items = Vec::new();
        let mut tys = Vec::new();

        for item in &ast.items {
            let item = self.expr(item)?;

            tys.push(item.ty.clone());
            items.push(item);
        }

        Ok(hir::Expr {
            kind: hir::ExprKind::Tuple(items),
            ty: solve::Ty::Tuple(tys),
            span: ast.span,
        })
    }

    fn path_expr(&mut self, path: &ast::Path) -> Result<hir::Expr, ()> {
        if path.segments.len() == 1 && path.spec.is_none() {
            let name = path.segments[0].name;

            for &local_id in self.scope.iter().rev() {
                let local = &self.body().locals[local_id];

                if local.name == name {
                    return Ok(hir::Expr {
                        kind: hir::ExprKind::Local(local_id),
                        ty: local.ty.clone(),
                        span: path.span,
                    });
                }
            }
        }

        if let Some(body_id) = self.program.modules.get_body(self.module, &path.segments) {
            self.calls.entry(self.body).or_default().push(body_id);

            if self.funcs.contains_key(&body_id) {
                let info = self.funcs.remove(&body_id).unwrap();

                self.lowerer.lower_function(self.calls, body_id, info)?;
            }

            // get the actual body and type
            let body = &self.program.bodies[body_id];
            let ty = body.ty();

            // compute the generic parameters
            let mut ty_map = HashMap::new();
            let mut generics = Vec::new();

            for generic in body.generics.clone() {
                let ty = solve::Ty::Var(self.solver.fresh_var());
                generics.push(ty.clone());
                ty_map.insert(generic, ty);
            }

            // substitute the generic parameters into the type
            let ty = ty.subst(&ty_map);

            // if the target isn't recursive with the current body, then
            // we can instantiate the type with fresh variables for the
            // subsumption check
            //
            // importantly, if the body however is recursive, then we CANNOT
            // instantiate, as this would break the soundness of type inference
            let ty = match self.recurses(body_id) {
                false => self.solver.instance(&ty),
                true => ty,
            };

            return Ok(hir::Expr {
                kind: hir::ExprKind::Func(body_id, generics),
                ty,
                span: path.span,
            });
        }

        let message = format!("path `{}`, does not resolve to a local or function", path);

        let diagnostic = Diagnostic::error("invalid::path")
            .message(message)
            .span(path.span);

        self.reporter.emit(diagnostic);

        Err(())
    }

    fn index_expr(&mut self, ast: &ast::IndexExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;
        let index = self.expr(&ast.index)?;

        let ty = solve::Ty::Var(self.solver.fresh_var());
        let list_ty = solve::Ty::list(ty.clone());

        self.subty(&target.ty, &list_ty, target.span);
        self.subty(&index.ty, &solve::Ty::INT, index.span);

        Ok(hir::Expr {
            kind: hir::ExprKind::Index(Box::new(target), Box::new(index)),
            ty,
            span: ast.span,
        })
    }

    fn field_expr(&mut self, ast: &ast::FieldExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;

        let ty = solve::Ty::Var(self.solver.fresh_var());
        let record_ty = solve::Ty::Record(BTreeMap::from([(ast.name, ty.clone())]));

        self.subty(&target.ty, &record_ty, target.span);

        Ok(hir::Expr {
            kind: hir::ExprKind::Field(Box::new(target), ast.name),
            ty,
            span: ast.span,
        })
    }

    fn unary_expr(&mut self, ast: &ast::UnaryExpr) -> Result<hir::Expr, ()> {
        todo!()
    }

    fn binary_expr(&mut self, ast: &ast::BinaryExpr) -> Result<hir::Expr, ()> {
        todo!()
    }

    fn call_expr(&mut self, ast: &ast::CallExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;
        let input = self.expr(&ast.input)?;

        let ty = solve::Ty::Var(self.solver.fresh_var());
        let func_ty = solve::Ty::func(input.ty.clone(), ty.clone());

        self.subty(&target.ty, &func_ty, target.span);

        Ok(hir::Expr {
            kind: hir::ExprKind::Call(Box::new(target), Box::new(input)),
            ty,
            span: ast.span,
        })
    }

    fn assign_expr(&mut self, ast: &ast::AssignExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;
        let value = self.expr(&ast.value)?;

        self.subty(&value.ty, &target.ty, value.span);

        Ok(hir::Expr {
            kind: hir::ExprKind::Assign(Box::new(target), Box::new(value)),
            ty: solve::Ty::NONE,
            span: ast.span,
        })
    }

    fn ref_expr(&mut self, ast: &ast::RefExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;

        let ty = solve::Ty::Var(self.solver.fresh_var());
        let kind = hir::ExprKind::Ref(Box::new(target));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn match_expr(&mut self, ast: &ast::MatchExpr) -> Result<hir::Expr, ()> {
        todo!()
    }

    fn loop_expr(&mut self, ast: &ast::LoopExpr) -> Result<hir::Expr, ()> {
        todo!()
    }

    fn break_expr(&mut self, ast: &ast::BreakExpr) -> Result<hir::Expr, ()> {
        todo!()
    }

    fn let_expr(&mut self, ast: &ast::LetExpr) -> Result<hir::Expr, ()> {
        let value = self.expr(&ast.value)?;

        if let Some(ref ty) = ast.ty {
            let ty = self.ty(ty)?;
            self.subty(&value.ty, &ty, value.span);
        }

        let binding = self.binding(&value.ty, &ast.binding)?;

        Ok(hir::Expr {
            kind: hir::ExprKind::Let(binding, Box::new(value)),
            ty: solve::Ty::NONE,
            span: ast.span,
        })
    }

    fn block_expr(&mut self, ast: &ast::BlockExpr) -> Result<hir::Expr, ()> {
        let mut exprs = Vec::new();
        let mut ty = solve::Ty::NONE;

        let scope_len = self.scope.len();

        for expr in &ast.exprs {
            let expr = self.expr(expr)?;
            ty = expr.ty.clone();
            exprs.push(expr);
        }

        self.scope.truncate(scope_len);

        Ok(hir::Expr {
            kind: hir::ExprKind::Block(exprs),
            ty,
            span: ast.span,
        })
    }
}
