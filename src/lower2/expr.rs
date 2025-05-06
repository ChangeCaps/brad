use std::{
    collections::{BTreeMap, HashMap, HashSet},
    mem,
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

        let output_ty = self.program[body_id].output.clone();

        let mut lowerer = ExprLowerer {
            lowerer: self,

            calls,

            module: info.module,
            context: Context::Body(body_id),

            generics: &mut Generics::Explicit(&generics),

            scope: Vec::new(),
            loop_ty: None,
            output_ty: output_ty.clone(),
        };

        // lower the arguments
        for (i, arg) in info.ast.args.iter().enumerate() {
            // fetch the type of the argument
            let ty = lowerer.program[body_id].input[i].ty.clone();

            // check that the expected type is a subtype of the actual type
            //
            // if the function is extern, require it
            if let Some(ref expected_ty) = arg.ty {
                // lower the expected type
                let expected_ty = lowerer.ty(expected_ty)?;

                lowerer.subty(ty.clone(), expected_ty.clone(), arg.span);
                lowerer.subty(expected_ty, ty.clone(), arg.span);
            } else if info.ast.is_extern {
                let diagnostic = Diagnostic::error("invalid::extern::arg")
                    .message("extern functions must have explicit types for all arguments")
                    .span(arg.span);

                lowerer.reporter.emit(diagnostic);

                return Err(());
            }

            // lower the binding
            let binding = lowerer.binding(ty, &arg.binding)?;

            // replace the binding in the input
            lowerer.program[body_id].input[i].binding = binding;
        }

        // constrain the output type, if one is provided
        //
        // if the function is extern, require it
        if let Some(ref expected_ty) = info.ast.output {
            // lower the expected type
            let expected_ty = lowerer.ty(expected_ty)?;

            let output_ty = lowerer.program[body_id].output.clone();
            lowerer.subty(expected_ty.clone(), output_ty.clone(), info.ast.span);
            lowerer.subty(output_ty, expected_ty, info.ast.span);
        } else if info.ast.is_extern {
            let diagnostic = Diagnostic::error("invalid::extern::output")
                .message("extern functions must have an explicit return type")
                .span(info.ast.span);

            lowerer.reporter.emit(diagnostic);

            return Err(());
        }

        self.program[body_id].expr = match info.ast.body {
            Some(ref body) => {
                let body = lowerer.expr(body)?;

                (self.program.solver).subty(body.ty.clone(), output_ty, body.span);

                Some(body)
            }
            None => None,
        };

        Ok(())
    }
}

struct ExprLowerer<'a, 'r> {
    lowerer: &'a mut Lowerer<'r>,

    calls: &'a mut HashMap<hir::BodyId, Vec<hir::BodyId>>,

    module: hir::ModuleId,
    context: Context,

    generics: &'a mut Generics<'a>,

    scope: Vec<Variable>,
    loop_ty: Option<solve::Ty>,
    output_ty: solve::Ty,
}

struct Variable {
    local: hir::LocalId,
    name: &'static str,
    depth: usize,
}

enum Context {
    None,

    Body(hir::BodyId),

    Lambda {
        parent: Box<Self>,
        locals: hir::Locals,
        captures: Vec<hir::Capture>,
    },
}

impl Context {
    fn body(&self) -> hir::BodyId {
        match self {
            Context::None => unreachable!(),
            Context::Body(body) => *body,
            Context::Lambda { parent, .. } => parent.body(),
        }
    }
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
    pub fn ty(&mut self, ast: &ast::Ty) -> Result<solve::Ty, ()> {
        self.lowerer.lower_ty(self.module, self.generics, true, ast)
    }

    pub fn fresh_var(&mut self) -> solve::Ty {
        solve::Ty::var(self.program.solver.fresh_var())
    }

    pub fn subty(&mut self, lhs: solve::Ty, rhs: solve::Ty, span: Span) {
        self.program.solver.subty(lhs, rhs, span);
    }

    // check whether `body_id` is mutually recursive with the current body
    fn recurses(&self, body_id: hir::BodyId) -> bool {
        let mut visited = HashSet::new();
        let mut queue = self.calls.get(&body_id).cloned().unwrap_or_default();

        while let Some(body_id) = queue.pop() {
            if body_id == self.context.body() {
                return true;
            }

            if !visited.insert(body_id) {
                continue;
            }

            queue.extend(self.calls.get(&body_id).into_iter().flatten().copied());
        }

        false
    }

    fn binding(&mut self, ty: solve::Ty, ast: &ast::Binding) -> Result<hir::Binding, ()> {
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
                    ty,
                    span,
                };

                let local = match self.context {
                    Context::None => unreachable!(),
                    Context::Body(body_id) => self.program[body_id].locals.insert(local),
                    Context::Lambda { ref mut locals, .. } => locals.insert(local),
                };

                self.scope.push(Variable {
                    local,
                    name,
                    depth: 0,
                });

                hir::Binding::Bind { local, span }
            }

            ast::Binding::Tuple { ref bindings, span } => {
                let mut hir_bindings = Vec::new();
                let mut tys = Vec::new();

                for binding in bindings {
                    let ty = self.fresh_var();

                    let binding = self.binding(ty.clone(), binding)?;
                    hir_bindings.push(binding);

                    tys.push(ty);
                }

                let tuple = solve::Ty::tuple(tys);

                self.subty(ty, tuple, span);

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
            ast::Expr::Lambda(ast) => self.lambda_expr(ast),
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
                ty: solve::Ty::int(),
                span: *span,
            },

            ast::Literal::Float { value, span } => hir::Expr {
                kind: hir::ExprKind::Float(*value),
                ty: solve::Ty::float(),
                span: *span,
            },

            ast::Literal::String { value, span } => hir::Expr {
                kind: hir::ExprKind::String(value),
                ty: solve::Ty::str(),
                span: *span,
            },

            ast::Literal::True { span } => hir::Expr {
                kind: hir::ExprKind::ZeroSize(solve::Tag::TRUE),
                ty: solve::Ty::true_(),
                span: *span,
            },

            ast::Literal::False { span } => hir::Expr {
                kind: hir::ExprKind::ZeroSize(solve::Tag::FALSE),
                ty: solve::Ty::false_(),
                span: *span,
            },

            ast::Literal::None { span } => hir::Expr {
                kind: hir::ExprKind::ZeroSize(solve::Tag::NONE),
                ty: solve::Ty::none(),
                span: *span,
            },
        })
    }

    fn list_expr(&mut self, ast: &ast::ListExpr) -> Result<hir::Expr, ()> {
        let ty = self.fresh_var();
        let mut items = Vec::new();

        for item in &ast.items {
            let item = self.expr(item)?;

            self.subty(item.ty.clone(), ty.clone(), item.span);

            items.push(item);
        }

        Ok(hir::Expr {
            kind: hir::ExprKind::Array(items),
            ty: solve::Ty::array(ty),
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
            ty: solve::Ty::record(tys),
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
            ty: solve::Ty::tuple(tys),
            span: ast.span,
        })
    }

    fn get_local<'a>(
        program: &'a hir::Program,
        context: &'a mut Context,
        id: hir::LocalId,
        depth: usize,
    ) -> (hir::LocalId, &'a hir::Local) {
        if depth == 0 {
            match context {
                Context::None => unreachable!(),

                Context::Body(body) => {
                    let local = &program[*body].locals[id];
                    return (id, local);
                }

                Context::Lambda { locals, .. } => {
                    let local = &locals[id];
                    return (id, local);
                }
            }
        }

        let Context::Lambda {
            parent,
            locals,
            captures,
            ..
        } = context
        else {
            unreachable!();
        };

        let (outer, local) = Self::get_local(program, parent, id, depth - 1);

        if let Some(c) = captures.iter().find(|c| c.outer == outer) {
            return (c.inner, local);
        }

        let inner = locals.insert(hir::Local {
            is_mutable: local.is_mutable,
            name: local.name,
            ty: local.ty.clone(),
            span: local.span,
        });

        captures.push(hir::Capture { inner, outer });

        (inner, &locals[inner])
    }

    fn path_expr(&mut self, path: &ast::Path) -> Result<hir::Expr, ()> {
        if path.segments.len() == 1 && path.spec.is_none() {
            let name = path.segments[0].name;

            for var in self.scope.iter().rev() {
                if var.name != name {
                    continue;
                }

                let (id, local) = Self::get_local(
                    &self.lowerer.program,
                    &mut self.context,
                    var.local,
                    var.depth,
                );

                let kind = hir::ExprKind::Local(id);
                let ty = match local.is_mutable {
                    false => local.ty.clone(),
                    true => solve::Ty::ref_(local.ty.clone()),
                };
                let span = path.span;

                return Ok(hir::Expr { kind, ty, span });
            }
        }

        if let Some(body_id) = self.program.modules.get_body(self.module, &path.segments) {
            self.calls
                .entry(self.context.body())
                .or_default()
                .push(body_id);

            if self.funcs.contains_key(&body_id) {
                let info = self.funcs.remove(&body_id).unwrap();

                self.lowerer.lower_function(self.calls, body_id, info)?;

                if !self.recurses(body_id) {
                    let mut ty = self.program[body_id].ty();
                    (self.program.solver).simplify_deep(&mut ty);
                }
            }

            // get the actual body and type
            let body = &self.program.bodies[body_id];
            let ty = body.ty();

            //let mut map = HashMap::new();

            match path.spec {
                Some(ref spec) => {
                    if spec.tys.len() != body.generics.len() {
                        let diagnostic = Diagnostic::error("invalid::spec")
                            .message("incorrect number of type arguments")
                            .span(spec.span);

                        self.reporter.emit(diagnostic);

                        return Err(());
                    }

                    //for (param, ty) in body.generics.clone().into_iter().zip(spec.tys.iter()) {
                    //    let ty = self.ty(ty)?;
                    //    map.insert(param, ty);
                    //}
                }

                None => {
                    //for param in body.generics.clone() {
                    //    let ty = self.fresh_var();
                    //    map.insert(param, ty);
                    //}
                }
            }

            // if the target isn't recursive with the current body, then
            // we can instantiate the type with fresh variables for the
            // subsumption check
            //
            // importantly, if the body however is recursive, then we CANNOT
            // instantiate, as this would break the soundness of type inference
            let ty = match self.recurses(body_id) {
                false => self.program.solver.instantiate(ty),
                true => ty,
            };

            return Ok(hir::Expr {
                kind: hir::ExprKind::Func(body_id),
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

        let ty = self.fresh_var();
        let list_ty = solve::Ty::array(ty.clone());

        self.subty(target.ty.clone(), list_ty, target.span);
        self.subty(index.ty.clone(), solve::Ty::int(), index.span);

        let kind = hir::ExprKind::Index(Box::new(target), Box::new(index));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn field_expr(&mut self, ast: &ast::FieldExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;

        let ty = self.fresh_var();
        let record_ty = solve::Ty::record(BTreeMap::from([(ast.name, ty.clone())]));

        self.subty(target.ty.clone(), record_ty, target.span);

        let kind = hir::ExprKind::Field(Box::new(target), ast.name);
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn unary_expr(&mut self, ast: &ast::UnaryExpr) -> Result<hir::Expr, ()> {
        let op = match ast.op {
            ast::UnaryOp::Neg => hir::UnaryOp::Neg,
            ast::UnaryOp::Not => hir::UnaryOp::Not,
            ast::UnaryOp::BitNot => hir::UnaryOp::BitNot,
            ast::UnaryOp::Deref => hir::UnaryOp::Deref,
        };

        let target = self.expr(&ast.target)?;

        let ty = match op {
            hir::UnaryOp::Neg | hir::UnaryOp::BitNot => {
                self.subty(target.ty.clone(), solve::Ty::int(), target.span);
                solve::Ty::int()
            }

            hir::UnaryOp::Not => {
                let boolean = solve::Ty::union_with(solve::Ty::true_(), solve::Ty::false_());

                self.subty(target.ty.clone(), boolean.clone(), target.span);

                boolean
            }

            hir::UnaryOp::Deref => {
                let ty = self.fresh_var();
                let ref_ty = solve::Ty::ref_(ty.clone());

                self.subty(target.ty.clone(), ref_ty, target.span);

                ty
            }
        };

        let kind = hir::ExprKind::Unary(op, Box::new(target));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn binary_expr(&mut self, ast: &ast::BinaryExpr) -> Result<hir::Expr, ()> {
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

        let lhs = self.expr(&ast.lhs)?;
        let rhs = self.expr(&ast.rhs)?;

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
            | hir::BinaryOp::Shr => {
                self.subty(lhs.ty.clone(), solve::Ty::int(), lhs.span);
                self.subty(rhs.ty.clone(), solve::Ty::int(), rhs.span);

                solve::Ty::int()
            }

            hir::BinaryOp::Lt | hir::BinaryOp::Le | hir::BinaryOp::Gt | hir::BinaryOp::Ge => {
                self.subty(lhs.ty.clone(), solve::Ty::int(), lhs.span);
                self.subty(rhs.ty.clone(), solve::Ty::int(), rhs.span);

                solve::Ty::union_with(solve::Ty::true_(), solve::Ty::false_())
            }

            hir::BinaryOp::And | hir::BinaryOp::Or => {
                let boolean = solve::Ty::union_with(solve::Ty::true_(), solve::Ty::false_());

                self.subty(lhs.ty.clone(), boolean.clone(), lhs.span);
                self.subty(rhs.ty.clone(), boolean.clone(), rhs.span);

                boolean
            }

            hir::BinaryOp::Eq | hir::BinaryOp::Ne => {
                self.subty(lhs.ty.clone(), rhs.ty.clone(), lhs.span);
                self.subty(rhs.ty.clone(), lhs.ty.clone(), rhs.span);

                solve::Ty::union_with(solve::Ty::true_(), solve::Ty::false_())
            }
        };

        let kind = hir::ExprKind::Binary(op, Box::new(lhs), Box::new(rhs));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn call_expr(&mut self, ast: &ast::CallExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;
        let input = self.expr(&ast.input)?;

        let ty = self.fresh_var();
        let func_ty = solve::Ty::func(input.ty.clone(), ty.clone());

        self.subty(target.ty.clone(), func_ty, target.span);

        let kind = hir::ExprKind::Call(Box::new(target), Box::new(input));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn lambda_expr(&mut self, ast: &ast::LambdaExpr) -> Result<hir::Expr, ()> {
        self.context = Context::Lambda {
            parent: Box::new(mem::replace(&mut self.context, Context::None)),
            locals: hir::Locals::new(),
            captures: Vec::new(),
        };

        // store information about the current context
        let old_loop_ty = self.loop_ty.take();
        let old_output_ty = self.output_ty.clone();
        let scope_len = self.scope.len();

        // set up the lowerer for the lambda context
        for var in &mut self.scope {
            var.depth += 1;
        }

        // lower the arguments
        let mut args = Vec::new();
        let mut tys = Vec::new();

        for binding in &ast.args {
            let ty = self.fresh_var();
            let binding = self.binding(ty.clone(), binding)?;

            args.push(binding);
            tys.push(ty);
        }

        // set up the output type
        let output_ty = self.fresh_var();
        self.output_ty = output_ty.clone();

        // lower the body of the lambda
        let body = Box::new(self.expr(&ast.body)?);

        // constrain the type of the body to be equal to the output type
        self.subty(body.ty.clone(), output_ty.clone(), body.span);
        self.subty(output_ty, body.ty.clone(), body.span);

        // restore the context
        let Context::Lambda {
            parent,
            locals,
            captures,
        } = mem::replace(&mut self.context, Context::None)
        else {
            unreachable!();
        };

        // restore the scope
        self.scope.truncate(scope_len);

        for var in &mut self.scope {
            var.depth -= 1;
        }

        // restore the lowerer to the previous context
        self.context = *parent;
        self.loop_ty = old_loop_ty;
        self.output_ty = old_output_ty;

        let ty = tys
            .into_iter()
            .rev()
            .fold(body.ty.clone(), |acc, ty| solve::Ty::func(ty, acc));

        let kind = hir::ExprKind::Lambda {
            captures,
            args,
            locals,
            body,
        };
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn assign_expr(&mut self, ast: &ast::AssignExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;
        let value = self.expr(&ast.value)?;

        let ref_value = solve::Ty::ref_(value.ty.clone());
        let ref_ty = solve::Ty::ref_(self.fresh_var());

        self.subty(ref_value, target.ty.clone(), value.span);
        self.subty(target.ty.clone(), ref_ty, target.span);

        let kind = hir::ExprKind::Assign(Box::new(target), Box::new(value));
        let ty = solve::Ty::none();
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn ref_expr(&mut self, ast: &ast::RefExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;

        let ty = self.fresh_var();
        let kind = hir::ExprKind::Ref(Box::new(target));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn match_expr(&mut self, ast: &ast::MatchExpr) -> Result<hir::Expr, ()> {
        let target = self.expr(&ast.target)?;

        let mut arms = Vec::new();
        let mut default = None;

        let mut input_ty = solve::Ty::never();
        let mut output_ty = solve::Ty::never();

        for arm in &ast.arms {
            match arm.pattern {
                ast::Pattern::Ty {
                    ref ty,
                    ref binding,
                    span,
                } => {
                    if let ast::Ty::Wild(..) = ty {
                        let ty = self.fresh_var();
                        let ty = solve::Ty::inter_with(ty, solve::Ty::neg(input_ty.clone()));
                        input_ty.union(ty.clone());

                        let scope_len = self.scope.len();

                        let binding = match binding {
                            Some(ref binding) => self.binding(ty, binding)?,
                            None => hir::Binding::Wild { span },
                        };

                        let body = self.expr(&arm.body)?;
                        output_ty.union(body.ty.clone());

                        self.scope.truncate(scope_len);

                        default = Some(Box::new((binding, body)));

                        break;
                    }

                    let tag = match ty {
                        ast::Ty::Int(..) => solve::Tag::INT,
                        ast::Ty::Float(..) => solve::Tag::FLOAT,
                        ast::Ty::Str(..) => solve::Tag::STR,
                        ast::Ty::True(..) => solve::Tag::TRUE,
                        ast::Ty::False(..) => solve::Tag::FALSE,
                        ast::Ty::None(..) => solve::Tag::NONE,
                        ast::Ty::Path(path) => {
                            match self.program.modules.get_type(self.module, &path.segments) {
                                Some(tag) => tag,
                                None => {
                                    let diagnostic = Diagnostic::error("invalid::path")
                                        .message(format!(
                                            "path `{}` does not resolve to a type",
                                            path,
                                        ))
                                        .span(span);

                                    self.reporter.emit(diagnostic);

                                    return Err(());
                                }
                            }
                        }

                        ast::Ty::Wild(..) => unreachable!(),

                        ast::Ty::Never(..)
                        | ast::Ty::Generic(..)
                        | ast::Ty::Ref { .. }
                        | ast::Ty::Func { .. }
                        | ast::Ty::List { .. }
                        | ast::Ty::Tuple { .. }
                        | ast::Ty::Union { .. }
                        | ast::Ty::Inter { .. }
                        | ast::Ty::Neg { .. }
                        | ast::Ty::Record { .. } => {
                            let ty = self.ty(ty)?;

                            let diagnostic = Diagnostic::error("invalid::match::ty")
                                .message(format!(
                                    "invalid type `{}` in match pattern",
                                    self.program.solver.format_ty(ty),
                                ))
                                .span(span);

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }
                    };

                    let mut ty = self.ty(ty)?;
                    ty.inter(input_ty.clone().neg());
                    input_ty.union(ty.clone());

                    let scope_len = self.scope.len();

                    let binding = match binding {
                        Some(ref binding) => self.binding(ty, binding)?,
                        None => hir::Binding::Wild { span },
                    };

                    let body = self.expr(&arm.body)?;
                    output_ty.union(body.ty.clone());

                    self.scope.truncate(scope_len);

                    let pattern = hir::Pattern::Tag { tag, binding, span };

                    arms.push(hir::Arm {
                        pattern,
                        body,
                        span,
                    });
                }
            }
        }

        self.subty(target.ty.clone(), input_ty, target.span);

        let body = hir::MatchBody { arms, default };

        Ok(hir::Expr {
            kind: hir::ExprKind::Match(Box::new(target), body),
            ty: output_ty,
            span: ast.span,
        })
    }

    fn loop_expr(&mut self, ast: &ast::LoopExpr) -> Result<hir::Expr, ()> {
        let ty = self.fresh_var();

        let scope_len = self.scope.len();
        let old_loop_ty = self.loop_ty.replace(ty.clone());

        let body = self.expr(&ast.body)?;

        self.scope.truncate(scope_len);
        self.loop_ty = old_loop_ty;

        let kind = hir::ExprKind::Loop(Box::new(body));
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn break_expr(&mut self, ast: &ast::BreakExpr) -> Result<hir::Expr, ()> {
        let Some(ty) = self.loop_ty.clone() else {
            let diagnostic = Diagnostic::error("invalid::break")
                .message("break outside of loop")
                .span(ast.span);

            self.reporter.emit(diagnostic);

            return Err(());
        };

        let value = match ast.value {
            Some(ref value) => {
                let value = self.expr(value)?;
                self.subty(value.ty.clone(), ty, value.span);
                Some(Box::new(value))
            }
            None => None,
        };

        let kind = hir::ExprKind::Break(value);
        let ty = solve::Ty::never();
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn let_expr(&mut self, ast: &ast::LetExpr) -> Result<hir::Expr, ()> {
        let value = self.expr(&ast.value)?;

        if let Some(ref ty) = ast.ty {
            let ty = self.ty(ty)?;
            self.subty(value.ty.clone(), ty, value.span);
        }

        let binding = self.binding(value.ty.clone(), &ast.binding)?;

        let kind = hir::ExprKind::Let(binding, Box::new(value));
        let ty = solve::Ty::none();
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }

    fn block_expr(&mut self, ast: &ast::BlockExpr) -> Result<hir::Expr, ()> {
        let mut exprs = Vec::new();
        let mut ty = solve::Ty::none();

        let scope_len = self.scope.len();

        for expr in &ast.exprs {
            let expr = self.expr(expr)?;
            ty = expr.ty.clone();
            exprs.push(expr);
        }

        self.scope.truncate(scope_len);

        let kind = hir::ExprKind::Block(exprs);
        let span = ast.span;

        Ok(hir::Expr { kind, ty, span })
    }
}
