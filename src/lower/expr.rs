use crate::{
    ast,
    diagnostic::{Diagnostic, Span},
    hir,
};

use super::{resolve_item, Resolved};

pub struct BodyLowerer<'a> {
    program: &'a mut hir::Program,
    module: hir::ModuleId,
    body: hir::BodyId,
    scope: Vec<hir::LocalId>,
}

impl<'a> BodyLowerer<'a> {
    pub fn new(program: &'a mut hir::Program, module: hir::ModuleId, body: hir::BodyId) -> Self {
        Self {
            program,
            module,
            body,
            scope: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, local: hir::LocalId) {
        self.scope.push(local);
    }

    pub fn find_scope(&self, name: &str) -> Option<hir::LocalId> {
        self.scope
            .iter()
            .rev()
            .copied()
            .find(|&local| self.body()[local].name == name)
    }

    pub fn body(&self) -> &hir::Body {
        &self.program[self.body]
    }

    pub fn body_mut(&mut self) -> &mut hir::Body {
        &mut self.program[self.body]
    }

    pub fn lower_binding(
        &mut self,
        binding: ast::Binding,
        ty: hir::Ty,
    ) -> Result<hir::Binding, Diagnostic> {
        match binding {
            ast::Binding::Wild { span } => Ok(hir::Binding::Wild { span }),

            ast::Binding::Bind {
                mutable,
                name,
                span,
            } => {
                let local = hir::Local {
                    mutable,
                    ty,
                    name,
                    span,
                };

                let local_id = self.body_mut().locals.insert(local);
                self.push_scope(local_id);

                Ok(hir::Binding::Bind {
                    local: local_id,
                    span,
                })
            }

            ast::Binding::Tuple { bindings, span } => {
                if let hir::Ty::Tuple(tys) = ty {
                    if bindings.len() != tys.len() {
                        let diagnostic = Diagnostic::error("unresolved::type")
                            .message("expected tuple type in binding")
                            .label(span, "found here");

                        return Err(diagnostic);
                    }

                    let mut result = Vec::new();

                    for (binding, ty) in bindings.into_iter().zip(tys) {
                        let binding = self.lower_binding(binding, ty)?;
                        result.push(binding);
                    }

                    Ok(hir::Binding::Tuple {
                        bindings: result,
                        span,
                    })
                } else {
                    let diagnostic = Diagnostic::error("unresolved::type")
                        .message("expected tuple type in binding")
                        .label(span, "found here");

                    Err(diagnostic)
                }
            }
        }
    }

    /// Asserts that a type is a subtype of another type.
    pub fn subty(&mut self, ty: hir::Ty, sub: hir::Ty, span: Span) -> Result<(), Diagnostic> {
        todo!()
    }

    pub fn lower_expr(&mut self, expr: ast::Expr) -> Result<hir::Expr, Diagnostic> {
        match expr {
            ast::Expr::Literal(e) => self.lower_literal(e),
            ast::Expr::List(e) => self.lower_list(e),
            ast::Expr::Record(e) => self.lower_record(e),
            ast::Expr::Tuple(e) => self.lower_tuple(e),
            ast::Expr::Path(e) => self.lower_path(e),
            ast::Expr::Index(e) => self.lower_index(e),
            ast::Expr::Field(e) => self.lower_field(e),
            ast::Expr::Unary(e) => self.lower_unary(e),
            ast::Expr::Binary(e) => self.lower_binary(e),
            ast::Expr::Call(e) => self.lower_call(e),
            ast::Expr::Assign(e) => self.lower_assign(e),
            ast::Expr::Ref(e) => self.lower_ref(e),
            ast::Expr::Match(e) => self.lower_match(e),
            ast::Expr::Loop(e) => self.lower_loop(e),
            ast::Expr::Break(e) => self.lower_break(e),
            ast::Expr::Let(e) => self.lower_let(e),
            ast::Expr::Block(e) => self.lower_block(e),
        }
    }

    fn lower_literal(&mut self, ast: ast::Literal) -> Result<hir::Expr, Diagnostic> {
        match ast {
            ast::Literal::Int { value, span } => Ok(hir::Expr {
                kind: hir::ExprKind::Int(value),
                ty: hir::Ty::Int,
                span,
            }),
            ast::Literal::Float { value, span } => Ok(hir::Expr {
                kind: hir::ExprKind::Float(value),
                ty: hir::Ty::Float,
                span,
            }),
            ast::Literal::String { value, span } => Ok(hir::Expr {
                kind: hir::ExprKind::String(value),
                ty: hir::Ty::Str,
                span,
            }),
            ast::Literal::True { .. } => todo!(),
            ast::Literal::False { .. } => todo!(),
            ast::Literal::None { .. } => todo!(),
        }
    }

    fn lower_list(&mut self, ast: ast::ListExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_record(&mut self, ast: ast::RecordExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_tuple(&mut self, ast: ast::TupleExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_path(&mut self, ast: ast::Path) -> Result<hir::Expr, Diagnostic> {
        if ast.segments.len() == 1 {
            let segment = &ast.segments[0];

            if let Some(local) = self.find_scope(segment.name) {}
        }

        let item = resolve_item(self.program, &mut &self.body().generics, self.module, ast)?;

        match item {
            Resolved::Type(_) => todo!(),
            Resolved::Func(body_id, vec) => {
                todo!()
            }
        }
    }

    fn lower_index(&mut self, ast: ast::IndexExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_field(&mut self, ast: ast::FieldExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_unary(&mut self, ast: ast::UnaryExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_call(&mut self, ast: ast::CallExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_assign(&mut self, ast: ast::AssignExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_ref(&mut self, ast: ast::RefExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_match(&mut self, ast: ast::MatchExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_loop(&mut self, ast: ast::LoopExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_break(&mut self, ast: ast::BreakExpr) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_let(&mut self, ast: ast::LetExpr) -> Result<hir::Expr, Diagnostic> {
        let value = self.lower_expr(*ast.value)?;

        let binding = self.lower_binding(ast.binding, value.ty.clone())?;

        Ok(hir::Expr {
            kind: hir::ExprKind::Let(binding, Box::new(value)),
            ty: hir::Ty::None,
            span: ast.span,
        })
    }

    fn lower_block(&mut self, ast: ast::BlockExpr) -> Result<hir::Expr, Diagnostic> {
        let mut ty = hir::Ty::None;
        let mut exprs = Vec::new();

        let scope = self.scope.len();

        for expr in ast.exprs {
            let expr = self.lower_expr(expr)?;
            ty = expr.ty.clone();
            exprs.push(expr);
        }

        self.scope.truncate(scope);

        Ok(hir::Expr {
            kind: hir::ExprKind::Block(exprs),
            ty,
            span: ast.span,
        })
    }
}
