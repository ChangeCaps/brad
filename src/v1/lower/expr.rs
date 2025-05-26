use std::collections::BTreeSet;

use diagnostic::Diagnostic;

use crate::{ast, v1::hir};

use super::{lower_ty, resolve_item, Resolved};

pub struct BodyLowerer<'a> {
    program: &'a mut hir::Program,
    module: hir::ModuleId,
    body: hir::BodyId,
    scope: Vec<hir::LocalId>,
    looping: bool,
    breaks: BTreeSet<hir::Ty>,
}

impl<'a> BodyLowerer<'a> {
    pub fn new(program: &'a mut hir::Program, module: hir::ModuleId, body: hir::BodyId) -> Self {
        Self {
            program,
            module,
            body,
            scope: Vec::new(),
            looping: false,
            breaks: BTreeSet::new(),
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
                    is_mutable: mutable,
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

    pub fn lower_ty(&self, ty: &ast::Ty) -> Result<hir::Ty, Diagnostic> {
        lower_ty(self.program, &self.body().generics, self.module, ty)
    }

    /// Asserts that a type is a subtype of another type.
    pub fn is_subty(&self, ty: &hir::Ty, sub: &hir::Ty) -> bool {
        match (ty, sub) {
            (hir::Ty::Int, hir::Ty::Int)
            | (hir::Ty::Float, hir::Ty::Float)
            | (hir::Ty::Str, hir::Ty::Str)
            | (hir::Ty::True, hir::Ty::True)
            | (hir::Ty::False, hir::Ty::False)
            | (hir::Ty::None, hir::Ty::None)
            | (_, hir::Ty::Never) => true,

            (hir::Ty::Generic(g1), hir::Ty::Generic(g2)) => g1 == g2,

            (hir::Ty::Named(id, tys), hir::Ty::Named(sub_id, sub_tys)) if id == sub_id => {
                assert_eq!(tys.len(), sub_tys.len());

                for (ty, sub) in tys.iter().zip(sub_tys) {
                    if !self.is_subty(ty, sub) {
                        return false;
                    }
                }

                true
            }

            (hir::Ty::Named(id, tys), sub) => {
                let spec = hir::Spec::new(&self.program[*id].generics, tys).unwrap();

                if let Some(ty) = self.program[*id].ty.as_ref() {
                    let ty = spec.apply(ty.clone()).unwrap();
                    return self.is_subty(&ty, sub);
                }

                false
            }

            (hir::Ty::Ref(ty), hir::Ty::Ref(sub)) => self.is_subty(ty, sub),

            (hir::Ty::List(ty), hir::Ty::List(sub)) => self.is_subty(ty, sub),

            (hir::Ty::Func(i, o), hir::Ty::Func(sub_i, sub_o)) => {
                self.is_subty(i, sub_i) && self.is_subty(o, sub_o)
            }

            (hir::Ty::Tuple(tys), hir::Ty::Tuple(subs)) => {
                if tys.len() != subs.len() {
                    return false;
                }

                for (ty, sub) in tys.iter().zip(subs) {
                    if !self.is_subty(ty, sub) {
                        return false;
                    }
                }

                true
            }

            (hir::Ty::Union(tys), hir::Ty::Union(subs)) => {
                for sub in subs {
                    if !tys.contains(sub) {
                        return false;
                    }
                }

                true
            }

            (hir::Ty::Union(tys), sub) => tys.contains(sub),

            (hir::Ty::Record(fields), hir::Ty::Record(sub_fields)) => {
                if fields.len() != sub_fields.len() {
                    return false;
                }

                for (field, sub_field) in fields.iter().zip(sub_fields) {
                    if field.name != sub_field.name {
                        return false;
                    }
                }

                true
            }

            (_, _) => false,
        }
    }

    pub fn lower_expr(
        &mut self,
        expr: ast::Expr,
        ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        match expr {
            ast::Expr::Literal(e) => self.lower_literal(e, ty),
            ast::Expr::List(e) => self.lower_list(e, ty),
            ast::Expr::Record(e) => self.lower_record(e, ty),
            ast::Expr::Tuple(e) => self.lower_tuple(e, ty),
            ast::Expr::Path(e) => self.lower_path(e, ty),
            ast::Expr::Index(e) => self.lower_index(e, ty),
            ast::Expr::Field(e) => self.lower_field(e, ty),
            ast::Expr::Unary(e) => self.lower_unary(e, ty),
            ast::Expr::Binary(e) => self.lower_binary(e, ty),
            ast::Expr::Call(e) => self.lower_call(e, ty),
            ast::Expr::Lambda(e) => self.lower_lambda(e, ty),
            ast::Expr::Assign(e) => self.lower_assign(e, ty),
            ast::Expr::Ref(e) => self.lower_ref(e, ty),
            ast::Expr::Match(e) => self.lower_match(e, ty),
            ast::Expr::Loop(e) => self.lower_loop(e, ty),
            ast::Expr::Break(e) => self.lower_break(e, ty),
            ast::Expr::Let(e) => self.lower_let(e, ty),
            ast::Expr::Block(e) => self.lower_block(e, ty),
        }
    }

    pub fn format_ty(&self, ty: &hir::Ty) -> String {
        self.program.types.format(ty)
    }

    fn lower_literal(
        &mut self,
        ast: ast::Literal,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
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
            ast::Literal::True { span } => Ok(hir::Expr {
                kind: hir::ExprKind::True,
                ty: hir::Ty::True,
                span,
            }),
            ast::Literal::False { span } => Ok(hir::Expr {
                kind: hir::ExprKind::False,
                ty: hir::Ty::False,
                span,
            }),
            ast::Literal::None { span } => Ok(hir::Expr {
                kind: hir::ExprKind::None,
                ty: hir::Ty::None,
                span,
            }),
        }
    }

    fn lower_list(
        &mut self,
        ast: ast::ListExpr,
        ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let mut ty = match ty {
            Some(hir::Ty::List(ty)) => Some(*ty),
            Some(_) => {
                let diagnostic = Diagnostic::error("unresolved::type")
                    .message("expected list type in list")
                    .label(ast.span, "found here");

                return Err(diagnostic);
            }
            None => None,
        };

        let mut items = Vec::new();

        for expr in ast.items {
            let expr = self.lower_expr(expr, ty.clone())?;

            if let Some(ref mut ty) = ty {
                if !self.is_subty(ty, &expr.ty) {
                    let diagnostic = Diagnostic::error("unresolved::type")
                        .message("expected type in list")
                        .label(ast.span, "found here");

                    return Err(diagnostic);
                }
            }

            ty = Some(expr.ty.clone());

            items.push(expr);
        }

        let Some(ty) = ty else {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected type in list")
                .label(ast.span, "found here");

            return Err(diagnostic);
        };

        Ok(hir::Expr {
            kind: hir::ExprKind::List(items),
            ty: hir::Ty::List(Box::new(ty)),
            span: ast.span,
        })
    }

    fn lower_record(
        &mut self,
        ast: ast::RecordExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let mut fields = Vec::new();
        let mut inits = Vec::new();

        for init in ast.fields {
            let value = self.lower_expr(init.value, None)?;

            fields.push(hir::Field {
                name: init.name,
                ty: value.ty.clone(),
            });

            inits.push(hir::Init {
                name: init.name,
                value,
                span: init.span,
            });
        }

        let ty = hir::Ty::Record(fields);
        let kind = hir::ExprKind::Record(inits);
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_tuple(
        &mut self,
        ast: ast::TupleExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let mut tys = Vec::new();
        let mut exprs = Vec::new();

        for expr in ast.items {
            let expr = self.lower_expr(expr, None)?;
            tys.push(expr.ty.clone());
            exprs.push(expr);
        }

        let ty = hir::Ty::Tuple(tys);
        let kind = hir::ExprKind::Tuple(exprs);
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_path(
        &mut self,
        ast: ast::Path,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        if ast.segments.len() == 1 {
            let segment = &ast.segments[0];

            if let Some(local) = self.find_scope(segment.name) {
                let kind = hir::ExprKind::Local(local);
                let ty = self.body()[local].ty.clone();
                let span = segment.span;

                return Ok(hir::Expr { kind, ty, span });
            }
        }

        let item = resolve_item(
            self.program,
            &mut &self.body().generics,
            self.module,
            ast.clone(),
        )?;

        match item {
            Resolved::Type(ty) => match ty {
                hir::Ty::Named(id, tys) => {
                    if self.program[id].ty.is_some() {
                        let diagnostic = Diagnostic::error("invalid::instance")
                            .message("instancing is only allowed for zero-sized types")
                            .label(ast.span, "found here");

                        return Err(diagnostic);
                    }

                    Ok(hir::Expr {
                        kind: hir::ExprKind::Named(id, tys.clone()),
                        ty: hir::Ty::Named(id, tys),
                        span: ast.span,
                    })
                }

                _ => {
                    let diagnostic = Diagnostic::error("unresolved::type")
                        .message("expected named type in path")
                        .label(ast.span, "found here");

                    Err(diagnostic)
                }
            },

            Resolved::Func(body_id, generics) => {
                let body = &self.program[body_id];

                if body.input.is_empty() {
                    panic!("calling functions without arguments is not yet supported");
                }

                let spec = hir::Spec::new(&body.generics, &generics)?;

                let mut ty = spec.apply(body.output.clone())?;

                for input in body.input.iter().rev() {
                    let input = spec.apply(input.ty.clone())?;

                    ty = hir::Ty::Func(Box::new(input), Box::new(ty));
                }

                let kind = hir::ExprKind::Func(body_id, generics);
                let span = ast.span;

                Ok(hir::Expr { kind, ty, span })
            }
        }
    }

    fn lower_index(
        &mut self,
        ast: ast::IndexExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let target = self.lower_expr(*ast.target, None)?;
        let index = self.lower_expr(*ast.index, None)?;

        let hir::Ty::List(ref ty) = target.ty else {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected list type in index")
                .label(ast.span, "found here");

            return Err(diagnostic);
        };

        if !self.is_subty(&hir::Ty::Int, &index.ty) {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected integer type in index")
                .label(ast.span, "found here");

            return Err(diagnostic);
        }

        let ty = ty.as_ref().clone();
        let kind = hir::ExprKind::Index(Box::new(target), Box::new(index));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_field(
        &mut self,
        ast: ast::FieldExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let target = self.lower_expr(*ast.target, None)?;

        let hir::Ty::Record(ref fields) = target.ty else {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected record type in field")
                .label(ast.span, "found here");

            return Err(diagnostic);
        };

        let Some(field) = fields.iter().find(|f| f.name == ast.name) else {
            let diagnostic = Diagnostic::error("unresolved::field")
                .message("expected field in record")
                .label(ast.span, "found here");

            return Err(diagnostic);
        };

        let ty = field.ty.clone();
        let kind = hir::ExprKind::Field(Box::new(target), ast.name);
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_unary(
        &mut self,
        ast: ast::UnaryExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let expr = self.lower_expr(*ast.target, None)?;

        let op = match ast.op {
            ast::UnaryOp::Neg => hir::UnaryOp::Neg,
            ast::UnaryOp::Not => hir::UnaryOp::Not,
            ast::UnaryOp::BitNot => hir::UnaryOp::BitNot,
            ast::UnaryOp::Deref => hir::UnaryOp::Deref,
        };

        let ty = match op {
            hir::UnaryOp::Neg | hir::UnaryOp::Not | hir::UnaryOp::BitNot => expr.ty.clone(),
            hir::UnaryOp::Deref => match expr.ty {
                hir::Ty::Ref(ref ty) => ty.as_ref().clone(),
                _ => {
                    let diagnostic = Diagnostic::error("unresolved::type")
                        .message("expected reference type in dereference")
                        .label(ast.span, "found here");

                    return Err(diagnostic);
                }
            },
        };

        let kind = hir::ExprKind::Unary(op, Box::new(expr));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_binary(
        &mut self,
        ast: ast::BinaryExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let lhs = self.lower_expr(*ast.lhs, None)?;
        let rhs = self.lower_expr(*ast.rhs, None)?;

        if lhs.ty != rhs.ty {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected equal types in binary operation")
                .label(ast.span, "found here");

            return Err(diagnostic);
        }

        let op = match ast.op {
            ast::BinaryOp::Add => hir::BinaryOp::Add,
            ast::BinaryOp::Sub => hir::BinaryOp::Sub,
            ast::BinaryOp::Mul => hir::BinaryOp::Mul,
            ast::BinaryOp::Div => hir::BinaryOp::Div,
            ast::BinaryOp::Mod => hir::BinaryOp::Mod,
            ast::BinaryOp::And => hir::BinaryOp::And,
            ast::BinaryOp::Or => hir::BinaryOp::Or,
            ast::BinaryOp::BitAnd => hir::BinaryOp::BitAnd,
            ast::BinaryOp::BitOr => hir::BinaryOp::BitOr,
            ast::BinaryOp::BitXor => hir::BinaryOp::BitXor,
            ast::BinaryOp::Shl => hir::BinaryOp::Shl,
            ast::BinaryOp::Shr => hir::BinaryOp::Shr,
            ast::BinaryOp::Eq => hir::BinaryOp::Eq,
            ast::BinaryOp::Ne => hir::BinaryOp::Ne,
            ast::BinaryOp::Lt => hir::BinaryOp::Lt,
            ast::BinaryOp::Le => hir::BinaryOp::Le,
            ast::BinaryOp::Gt => hir::BinaryOp::Gt,
            ast::BinaryOp::Ge => hir::BinaryOp::Ge,
        };

        let ty = match op {
            hir::BinaryOp::Add
            | hir::BinaryOp::Sub
            | hir::BinaryOp::Mul
            | hir::BinaryOp::Div
            | hir::BinaryOp::Mod
            | hir::BinaryOp::BitAnd
            | hir::BinaryOp::BitOr
            | hir::BinaryOp::BitXor
            | hir::BinaryOp::Shl
            | hir::BinaryOp::Shr => lhs.ty.clone(),

            hir::BinaryOp::Eq
            | hir::BinaryOp::Ne
            | hir::BinaryOp::Lt
            | hir::BinaryOp::Le
            | hir::BinaryOp::Gt
            | hir::BinaryOp::Ge
            | hir::BinaryOp::And
            | hir::BinaryOp::Or => hir::Ty::Union([hir::Ty::True, hir::Ty::False].into()),
        };

        let kind = hir::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_call(
        &mut self,
        ast: ast::CallExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let target = self.lower_expr(*ast.target, None)?;
        let input = self.lower_expr(*ast.input, None)?;

        let hir::Ty::Func(ref i, ref o) = target.ty else {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected function type in call")
                .label(ast.span, "found here");

            return Err(diagnostic);
        };

        if !self.is_subty(i, &input.ty) {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message(format!(
                    "expected input type in call `{}`, found `{}`",
                    self.format_ty(i),
                    self.format_ty(&input.ty)
                ))
                .label(ast.span, "found here");

            return Err(diagnostic);
        }

        let ty = o.as_ref().clone();
        let kind = hir::ExprKind::Call(Box::new(target), Box::new(input));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_lambda(
        &mut self,
        _ast: ast::LambdaExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        todo!()
    }

    fn lower_assign(
        &mut self,
        ast: ast::AssignExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let target = self.lower_expr(*ast.target, None)?;
        let value = self.lower_expr(*ast.value, None)?;

        if !self.is_subty(&target.ty, &value.ty) {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected type in assignment")
                .label(ast.span, "found here");

            return Err(diagnostic);
        }

        let kind = hir::ExprKind::Assign(Box::new(target), Box::new(value));
        let ty = hir::Ty::None;
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_ref(
        &mut self,
        ast: ast::RefExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let expr = self.lower_expr(*ast.target, None)?;

        let ty = hir::Ty::Ref(Box::new(expr.ty.clone()));
        let kind = hir::ExprKind::Ref(Box::new(expr));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_match(
        &mut self,
        ast: ast::MatchExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let target = self.lower_expr(*ast.target, None)?;

        let mut arms = Vec::new();
        let mut tys = BTreeSet::new();

        for arm in ast.arms {
            let pattern = match arm.pattern {
                ast::Pattern::Ty { ty, binding, span } => {
                    let ty = self.lower_ty(&ty)?;

                    let binding = match binding {
                        Some(binding) => self.lower_binding(binding, ty.clone())?,
                        None => hir::Binding::Wild { span },
                    };

                    hir::Pattern::Ty { ty, binding, span }
                }
            };

            let expr = self.lower_expr(arm.body, None)?;

            if !tys.contains(&expr.ty) {
                tys.insert(expr.ty.clone());
            }

            arms.push(hir::Arm {
                pattern,
                expr,
                span: arm.span,
            });
        }

        let ty = hir::Ty::Union(tys).normalize();
        let kind = hir::ExprKind::Match(Box::new(target), arms);
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_loop(
        &mut self,
        ast: ast::LoopExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let old_looping = self.looping;
        let old_breaks = self.breaks.clone();

        self.looping = true;
        self.breaks.clear();

        let body = self.lower_expr(*ast.body, None)?;

        let ty = match self.breaks.len() {
            0 => hir::Ty::Never,
            _ => hir::Ty::Union(self.breaks.clone()).normalize(),
        };

        let kind = hir::ExprKind::Loop(Box::new(body));
        let span = ast.span;

        self.looping = old_looping;
        self.breaks = old_breaks;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lower_break(
        &mut self,
        ast: ast::BreakExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        if !self.looping {
            let diagnostic = Diagnostic::error("unresolved::break")
                .message("break outside of loop")
                .label(ast.span, "found here");

            return Err(diagnostic);
        }

        match ast.value {
            Some(value) => {
                let value = self.lower_expr(*value, None)?;
                self.breaks.insert(value.ty.clone());

                let kind = hir::ExprKind::Break(Some(Box::new(value)));
                let ty = hir::Ty::None;
                let span = ast.span;

                Ok(hir::Expr { kind, ty, span })
            }
            None => {
                self.breaks.insert(hir::Ty::None);

                let kind = hir::ExprKind::Break(None);
                let ty = hir::Ty::None;
                let span = ast.span;

                Ok(hir::Expr { kind, ty, span })
            }
        }
    }

    fn lower_let(
        &mut self,
        ast: ast::LetExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let value = self.lower_expr(*ast.value, None)?;

        let (binding, ty) = if let Some(ty) = ast.ty {
            let ty = self.lower_ty(&ty)?;

            if !self.is_subty(&ty, &value.ty) {
                let diagnostic = Diagnostic::error("unresolved::type")
                    .message("expected type in binding")
                    .label(ast.span, "found here");

                return Err(diagnostic);
            }

            let binding = self.lower_binding(ast.binding, ty.clone())?;

            (binding, ty)
        } else {
            let binding = self.lower_binding(ast.binding, value.ty.clone())?;

            (binding, value.ty.clone())
        };

        Ok(hir::Expr {
            kind: hir::ExprKind::Let(binding, ty, Box::new(value)),
            ty: hir::Ty::None,
            span: ast.span,
        })
    }

    fn lower_block(
        &mut self,
        ast: ast::BlockExpr,
        _ty: Option<hir::Ty>,
    ) -> Result<hir::Expr, Diagnostic> {
        let mut ty = hir::Ty::None;
        let mut exprs = Vec::new();

        let scope = self.scope.len();

        for expr in ast.exprs {
            let expr = self.lower_expr(expr, None)?;
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
