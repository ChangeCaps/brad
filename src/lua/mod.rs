use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::{Deref, DerefMut},
};

use crate::{
    ast,
    diagnostic::{Diagnostic, Report},
    parse::Interner,
    solve::{Options, Solver, Tag, Ty},
};

const LUA_PRELUDE: &str = include_str!("prelude.lua");

pub struct Codegen {
    interner: Interner,
    solver: Solver,
    types: HashSet<&'static str>,
    funcs: HashMap<&'static str, (Ty, bool)>,
    code: String,
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            interner: Interner::new(),
            solver: Solver::new(Options::default()),
            types: HashSet::new(),
            funcs: HashMap::new(),
            code: String::new(),
        }
    }

    pub fn finish(mut self, ast: ast::Module) -> Result<String, Report> {
        for decl in &ast.decls {
            match decl {
                ast::Decl::Type(ast) => {
                    self.types
                        .insert(self.interner.intern(&ast.name.to_string()));
                }

                ast::Decl::Alias(ast) => {
                    self.types
                        .insert(self.interner.intern(&ast.name.to_string()));
                }

                _ => {}
            }
        }

        self.code.push_str(LUA_PRELUDE);
        self.code.push('\n');

        for decl in &ast.decls {
            match decl {
                ast::Decl::Type(ast) => {
                    self.codegen_ty(ast)?;
                }

                ast::Decl::Alias(ast) => {
                    self.codegen_alias(ast)?;
                }

                _ => {}
            }
        }

        let mut report = Report::new();

        for decl in &ast.decls {
            if let ast::Decl::Func(ast) = decl {
                if let Err(err) = self.codegen_function(ast) {
                    report.push(err);
                }
            }
        }

        let _ = self.solver.finish(&mut report);

        if !report.is_empty() {
            return Err(report);
        }

        self.code.push_str("print(M.main())");

        Ok(self.code)
    }

    fn codegen_ty(&mut self, ast: &ast::Type) -> Result<(), Diagnostic> {
        let name = self.interner.intern(&ast.name.to_string());
        let tag = Ty::tag(name, 0);

        let mut ctx = Vec::new();

        if let Some(generics) = &ast.generics {
            for generic in &generics.params {
                let ty = Ty::Var(self.solver.fresh_var());
                ctx.push((generic.name, ty.clone()));
            }
        }

        let ty = match ast.ty {
            Some(ref ty) => {
                let ty = self.lower_ty(&mut ctx, ty)?;

                let constructor = format!(
                    "M['{name}'] = function()
  return function(super)
    return add_type_tag(super, '{name}')
  end
end\n\n",
                    name = ast.name
                );
                self.code.push_str(&constructor);

                let output = Ty::inter(tag.clone(), ty.clone());

                let func = Ty::func(ty, output.clone());
                self.funcs.insert(name, (func, true));

                output
            }

            None => {
                let constructor = format!(
                    "M['{name}'] = function()
  local value = {{}}
  setmetatable(value, {{ __type_tags = {{ ['{name}'] = true }} }})
  return value
end\n\n",
                    name = ast.name
                );
                self.code.push_str(&constructor);

                self.funcs.insert(name, (tag.clone(), true));

                tag
            }
        };

        let args = ctx.iter().map(|(_, ty)| ty.clone()).collect();
        self.solver.add_applicable(Tag::new(name, 0), ty, args);

        Ok(())
    }

    fn codegen_alias(&mut self, ast: &ast::Alias) -> Result<(), Diagnostic> {
        let mut ctx = Vec::new();

        if let Some(generics) = &ast.generics {
            for generic in &generics.params {
                let ty = Ty::Var(self.solver.fresh_var());
                ctx.push((generic.name, ty.clone()));
            }
        }

        let ty = self.lower_ty(&mut ctx, &ast.ty)?;

        let args = ctx.iter().map(|(_, ty)| ty.clone()).collect();
        let name = self.interner.intern(&ast.name.to_string());
        (self.solver).add_applicable(Tag::new(name, 0), ty.clone(), args);

        Ok(())
    }

    fn codegen_function(&mut self, ast: &ast::Func) -> Result<(), Diagnostic> {
        let name = self.interner.intern(&ast.name.to_string());

        let mut codegen = ExprCodegen {
            codegen: self,
            generics: vec![],
            stmts: vec![],
            scope: vec![],
            loop_var: None,
            next_ident: 0,
        };

        // resolve explicit generics
        if let Some(generics) = &ast.generics {
            for generic in &generics.params {
                let ty = Ty::Var(codegen.solver.fresh_var());
                codegen.generics.push((generic.name, ty.clone()));
            }
        }

        // lower the output type
        let output = match ast.output {
            Some(ref ty) => codegen.codegen.lower_ty(&mut codegen.generics, ty)?,
            None => Ty::Var(codegen.solver.fresh_var()),
        };

        // generate the function body
        let mut body = String::new();

        // generate the function header
        for (i, _) in ast.args.iter().enumerate() {
            body.push_str(&format!("return function(arg{})\n", i));
        }

        // lower the function type and register arguments
        let mut func_ty = output.clone();

        for (i, arg) in ast.args.iter().enumerate().rev() {
            let ty = match arg.ty {
                Some(ref ty) => codegen.codegen.lower_ty(&mut codegen.generics, ty)?,
                None => Ty::Var(codegen.solver.fresh_var()),
            };

            func_ty = Ty::func(ty.clone(), func_ty);

            // generate the argument binding
            codegen.binding(&ty, &arg.binding, &format!("arg{}", i))?;
        }

        // register the function as incomplete, disabling subsumption checking
        codegen.funcs.insert(name, (func_ty.clone(), false));

        // generate the function body
        let (value, ty) = codegen.expr(ast.body.as_ref().unwrap())?;

        // constrain the output type, to the type of the body
        let span = ast.output.as_ref().map_or(ast.span, |ty| ty.span());
        codegen.solver.subty(&ty, &output, span);

        // append the statemetns in the body
        for stmt in codegen.stmts {
            body.push_str(&format!("  {}", stmt));
            body.push('\n');
        }

        // generate the return statement
        body.push_str(&format!("  return {}\n", value));

        // close the function body
        for _ in &ast.args {
            body.push_str("end\n");
        }

        // push the function to the generated code
        let func = format!("M['{}'] = function()\n{}end\n\n", ast.name, body);
        self.code.push_str(&func);

        // register the function as complete
        self.funcs.insert(name, (func_ty, true));

        Ok(())
    }

    fn lower_ty(&mut self, ctx: &mut dyn GenericContext, ty: &ast::Ty) -> Result<Ty, Diagnostic> {
        Ok(match ty {
            ast::Ty::Wild(_) => Ty::Var(self.solver.fresh_var()),
            ast::Ty::Int(_) => Ty::INT,
            ast::Ty::Float(_) => Ty::FLOAT,
            ast::Ty::Str(_) => Ty::STR,
            ast::Ty::True(_) => Ty::TRUE,
            ast::Ty::False(_) => Ty::FALSE,
            ast::Ty::None(_) => Ty::NONE,
            ast::Ty::Never(_) => Ty::Bot,
            ast::Ty::Generic(generic) => ctx.generic(&mut self.solver, generic.name)?,
            ast::Ty::Path(path) => {
                let name = path
                    .segments
                    .iter()
                    .map(|s| s.name)
                    .collect::<Vec<_>>()
                    .join("::");

                let name = self.interner.intern(&name);

                if !self.types.contains(name) {
                    let diagnostic = Diagnostic::error("invalid::path")
                        .message(format!("unknown type `{}`", name))
                        .span(path.span);

                    return Err(diagnostic);
                }

                let tag = Tag::new(name, 0);

                match path.spec {
                    Some(ref spec) => {
                        let mut tys = Vec::new();

                        for ty in &spec.tys {
                            let ty = self.lower_ty(ctx, ty)?;
                            tys.push(ty);
                        }

                        Ty::app(tag, tys)
                    }

                    None => {
                        let args = self.solver.applicable_args(tag).unwrap();

                        let args = (0..args.len())
                            .map(|_| Ty::Var(self.solver.fresh_var()))
                            .collect();

                        Ty::app(tag, args)
                    }
                }
            }

            ast::Ty::Func { input, output, .. } => {
                let input = self.lower_ty(ctx, input)?;
                let output = self.lower_ty(ctx, output)?;
                Ty::func(input, output)
            }

            ast::Ty::List { ty, .. } => Ty::list(self.lower_ty(ctx, ty)?),

            ast::Ty::Ref { ty, .. } => Ty::ref_(self.lower_ty(ctx, ty)?),

            ast::Ty::Tuple { tys, .. } => {
                let tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(ctx, ty))
                    .collect::<Result<_, _>>()?;

                Ty::Tuple(tys)
            }

            ast::Ty::Union { tys, .. } => {
                let mut tys = tys.iter().map(|ty| self.lower_ty(ctx, ty));
                let first = tys.next().unwrap()?;

                tys.try_fold(first, |acc, ty| Ok(Ty::union(acc, ty?)))?
            }

            ast::Ty::Record { fields, .. } => {
                let mut lowered = BTreeMap::new();

                for field in fields {
                    let ty = self.lower_ty(ctx, &field.ty)?;
                    lowered.insert(field.name, ty);
                }

                Ty::Record(lowered)
            }
        })
    }
}

trait GenericContext {
    fn generic(&mut self, solver: &mut Solver, name: &'static str) -> Result<Ty, Diagnostic>;
}

impl GenericContext for Vec<(&'static str, Ty)> {
    fn generic(&mut self, solver: &mut Solver, name: &'static str) -> Result<Ty, Diagnostic> {
        for (n, ty) in self.iter().rev() {
            if *n == name {
                return Ok(ty.clone());
            }
        }

        let ty = Ty::Var(solver.fresh_var());

        self.push((name, ty.clone()));

        Ok(ty)
    }
}

impl GenericContext for &Vec<(&'static str, Ty)> {
    fn generic(&mut self, _solver: &mut Solver, name: &'static str) -> Result<Ty, Diagnostic> {
        for (n, ty) in self.iter().rev() {
            if *n == name {
                return Ok(ty.clone());
            }
        }

        let diagnostic =
            Diagnostic::error("invalid::generic").message(format!("unknown generic `{}`", name));

        Err(diagnostic)
    }
}

struct ExprCodegen<'a> {
    codegen: &'a mut Codegen,
    generics: Vec<(&'static str, Ty)>,
    scope: Vec<(&'static str, bool, Ty)>,
    stmts: Vec<String>,
    loop_var: Option<(String, Ty)>,
    next_ident: usize,
}

impl Deref for ExprCodegen<'_> {
    type Target = Codegen;

    fn deref(&self) -> &Self::Target {
        self.codegen
    }
}

impl DerefMut for ExprCodegen<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.codegen
    }
}

impl ExprCodegen<'_> {
    fn fresh_ident(&mut self) -> String {
        let ident = self.next_ident;
        self.next_ident += 1;
        format!("tmp{}", ident)
    }

    fn lower_ty(&mut self, ty: &ast::Ty) -> Result<Ty, Diagnostic> {
        self.codegen.lower_ty(&mut &self.generics, ty)
    }

    fn push_scope(&mut self, name: &'static str, mutable: bool, ty: Ty) -> String {
        let var = format!("var{}", self.scope.len());
        self.scope.push((name, mutable, ty));
        var
    }

    fn find_scope(&mut self, name: &str) -> Option<usize> {
        for (i, (n, _, _)) in self.scope.iter().enumerate().rev() {
            if *n == name {
                return Some(i);
            }
        }

        None
    }

    fn binding(&mut self, ty: &Ty, ast: &ast::Binding, expr: &str) -> Result<(), Diagnostic> {
        match ast {
            ast::Binding::Wild { .. } => Ok(()),
            ast::Binding::Bind { name, mutable, .. } => {
                let var = self.push_scope(name, *mutable, ty.clone());

                self.stmts.push(format!("local {} = {}", var, expr));

                Ok(())
            }
            ast::Binding::Tuple { bindings, span } => {
                let temp = self.fresh_ident();
                self.stmts.push(format!("local {temp} = {expr}"));

                let mut tys = Vec::new();

                for (i, binding) in bindings.iter().enumerate() {
                    let ty = Ty::Var(self.solver.fresh_var());
                    self.binding(&ty, binding, &format!("{temp}[{i} + 1]"))?;

                    tys.push(ty.clone());
                }

                self.solver.subty(ty, &Ty::Tuple(tys), *span);

                Ok(())
            }
        }
    }

    fn expr(&mut self, ast: &ast::Expr) -> Result<(String, Ty), Diagnostic> {
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

    fn literal_expr(&mut self, ast: &ast::Literal) -> Result<(String, Ty), Diagnostic> {
        Ok(match ast {
            ast::Literal::Int { value, .. } => (format!("make_int({})", value), Ty::INT),

            ast::Literal::Float { value, .. } => (format!("make_float({})", value), Ty::FLOAT),

            ast::Literal::String { value, .. } => (format!("make_str('{}')", value), Ty::STR),

            ast::Literal::True { .. } => (String::from("make_true()"), Ty::TRUE),
            ast::Literal::False { .. } => (String::from("make_false()"), Ty::FALSE),
            ast::Literal::None { .. } => (String::from("make_none()"), Ty::NONE),
        })
    }

    fn list_expr(&mut self, ast: &ast::ListExpr) -> Result<(String, Ty), Diagnostic> {
        let ty = Ty::Var(self.solver.fresh_var());
        let mut items = Vec::new();

        for item in &ast.items {
            let (value, item_ty) = self.expr(item)?;
            self.solver.subty(&item_ty, &ty, item.span());
            items.push(value);
        }

        let value = format!("make_list({})", items.join(", "));

        Ok((value, Ty::list(ty)))
    }

    fn record_expr(&mut self, ast: &ast::RecordExpr) -> Result<(String, Ty), Diagnostic> {
        let mut types = BTreeMap::new();
        let mut fields = Vec::new();

        for field in &ast.fields {
            let (value, ty) = self.expr(&field.value)?;
            types.insert(field.name, ty);
            fields.push(format!("{} = {}", field.name, value));
        }

        let value = format!("make_record({{{}}})", fields.join(", "));

        Ok((value, Ty::Record(types)))
    }

    fn tuple_expr(&mut self, ast: &ast::TupleExpr) -> Result<(String, Ty), Diagnostic> {
        let mut types = Vec::new();
        let mut items = Vec::new();

        for item in &ast.items {
            let (value, ty) = self.expr(item)?;

            types.push(ty);
            items.push(value);
        }

        let tuple = format!("make_tuple({})", items.join(", "));

        Ok((tuple, Ty::Tuple(types)))
    }

    fn path_expr(&mut self, ast: &ast::Path) -> Result<(String, Ty), Diagnostic> {
        if ast.segments.len() == 1 {
            let name = ast.segments[0].name;

            if let Some(idx) = self.find_scope(name) {
                let (_, _, ty) = self.scope[idx].clone();

                return Ok((format!("var{}", idx), ty));
            }
        }

        let name = ast
            .segments
            .iter()
            .map(|s| s.name)
            .collect::<Vec<_>>()
            .join("::");

        if let Some((func, complete)) = self.funcs.get(name.as_str()).cloned() {
            let ty = match complete {
                true => self.solver.instance(&func),
                false => func,
            };

            return Ok((format!("M['{}']()", name), ty));
        }

        let diagnostic = Diagnostic::error("invalid::path")
            .message(format!("unknown path `{}`", name))
            .span(ast.span);

        Err(diagnostic)
    }

    fn index_expr(&mut self, ast: &ast::IndexExpr) -> Result<(String, Ty), Diagnostic> {
        let (target, target_ty) = self.expr(&ast.target)?;
        let (index, index_ty) = self.expr(&ast.index)?;

        let ty = Ty::Var(self.solver.fresh_var());
        let list_ty = Ty::list(ty.clone());

        self.solver.subty(&target_ty, &list_ty, ast.span);
        self.solver.subty(&index_ty, &Ty::INT, ast.span);

        let value = format!("{target}[{index}.value + 1]");

        Ok((value, ty))
    }

    fn field_expr(&mut self, ast: &ast::FieldExpr) -> Result<(String, Ty), Diagnostic> {
        let (target, target_ty) = self.expr(&ast.target)?;

        let ty = Ty::Var(self.solver.fresh_var());

        let record_ty = Ty::record([(ast.name, ty.clone())]);

        self.solver.subty(&target_ty, &record_ty, ast.span);

        Ok((format!("{}.{}", target, ast.name), ty))
    }

    fn unary_expr(&mut self, ast: &ast::UnaryExpr) -> Result<(String, Ty), Diagnostic> {
        let (value, value_ty) = self.expr(&ast.target)?;

        match ast.op {
            ast::UnaryOp::Neg => {
                self.solver.subty(&value_ty, &Ty::INT, ast.span);
                let value = format!("make_int(-{}.value)", value);

                Ok((value, Ty::INT))
            }

            ast::UnaryOp::Not => {
                self.solver.subty(&value_ty, &Ty::TRUE, ast.span);
                self.solver.subty(&value_ty, &Ty::FALSE, ast.span);

                let value = format!("make_bool(not has_type_tag({}, 'true'))", value);

                Ok((value, Ty::union(Ty::TRUE, Ty::FALSE)))
            }

            ast::UnaryOp::BitNot => {
                self.solver.subty(&value_ty, &Ty::INT, ast.span);
                let value = format!("make_int(~{}.value)", value);

                Ok((value, Ty::INT))
            }

            ast::UnaryOp::Deref => {
                let ty = Ty::Var(self.solver.fresh_var());
                (self.solver).subty(&value_ty, &Ty::ref_(ty.clone()), ast.span);

                Ok((value, ty))
            }
        }
    }

    fn binary_expr(&mut self, ast: &ast::BinaryExpr) -> Result<(String, Ty), Diagnostic> {
        let (lhs, lhs_ty) = self.expr(&ast.lhs)?;
        let (rhs, rhs_ty) = self.expr(&ast.rhs)?;

        match ast.op {
            ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Mod
            | ast::BinaryOp::BitAnd
            | ast::BinaryOp::BitOr
            | ast::BinaryOp::BitXor
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr => {
                self.solver.subty(&lhs_ty, &Ty::INT, ast.span);
                self.solver.subty(&rhs_ty, &Ty::INT, ast.span);

                let op = match ast.op {
                    ast::BinaryOp::Add => "+",
                    ast::BinaryOp::Sub => "-",
                    ast::BinaryOp::Mul => "*",
                    ast::BinaryOp::Div => "/",
                    ast::BinaryOp::Mod => "%",
                    ast::BinaryOp::BitAnd => "&",
                    ast::BinaryOp::BitOr => "|",
                    ast::BinaryOp::BitXor => "~",
                    ast::BinaryOp::Shl => "<<",
                    ast::BinaryOp::Shr => ">>",
                    _ => unreachable!(),
                };

                let value = format!("make_int({lhs}.value {op} {rhs}.value)");

                Ok((value, Ty::INT))
            }

            ast::BinaryOp::Eq | ast::BinaryOp::Ne => {
                self.solver.subty(&lhs_ty, &rhs_ty, ast.span);
                self.solver.subty(&rhs_ty, &lhs_ty, ast.span);

                let op = match ast.op {
                    ast::BinaryOp::Eq => "==",
                    ast::BinaryOp::Ne => "~=",
                    _ => unreachable!(),
                };

                let ty = Ty::union(Ty::TRUE, Ty::FALSE);
                let value = format!("make_bool({lhs} {op} {rhs})");

                Ok((value, ty))
            }

            ast::BinaryOp::And | ast::BinaryOp::Or => {
                let ty = Ty::union(Ty::TRUE, Ty::FALSE);

                self.solver.subty(&lhs_ty, &ty, ast.span);
                self.solver.subty(&rhs_ty, &ty, ast.span);

                let op = match ast.op {
                    ast::BinaryOp::And => "and",
                    ast::BinaryOp::Or => "or",
                    _ => unreachable!(),
                };

                let lhs = format!("has_type_tag({lhs}, 'true')");
                let rhs = format!("has_type_tag({rhs}, 'true')");

                let value = format!("make_bool({lhs} {op} {rhs})");

                Ok((value, ty))
            }

            ast::BinaryOp::Lt | ast::BinaryOp::Le | ast::BinaryOp::Gt | ast::BinaryOp::Ge => {
                self.solver.subty(&lhs_ty, &Ty::INT, ast.span);
                self.solver.subty(&rhs_ty, &Ty::INT, ast.span);

                let op = match ast.op {
                    ast::BinaryOp::Lt => "<",
                    ast::BinaryOp::Le => "<=",
                    ast::BinaryOp::Gt => ">",
                    ast::BinaryOp::Ge => ">=",
                    _ => unreachable!(),
                };

                let ty = Ty::union(Ty::TRUE, Ty::FALSE);
                let value = format!("make_bool({lhs}.value {op} {rhs}.value)");

                Ok((value, ty))
            }
        }
    }

    fn call_expr(&mut self, ast: &ast::CallExpr) -> Result<(String, Ty), Diagnostic> {
        let (target, target_ty) = self.expr(&ast.target)?;
        let (input, input_ty) = self.expr(&ast.input)?;

        let output = Ty::Var(self.solver.fresh_var());

        let func_ty = Ty::func(input_ty, output.clone());

        self.solver.subty(&target_ty, &func_ty, ast.span);

        Ok((format!("{}({})", target, input), output))
    }

    fn assign_expr(&mut self, ast: &ast::AssignExpr) -> Result<(String, Ty), Diagnostic> {
        let (value, value_ty) = self.expr(&ast.value)?;
        let (target, target_ty) = self.expr(&ast.target)?;

        self.solver.subty(&value_ty, &target_ty, ast.span);
        self.stmts.push(format!("{} = {}", target, value));

        Ok((String::from("make_none()"), Ty::NONE))
    }

    fn ref_expr(&mut self, _ast: &ast::RefExpr) -> Result<(String, Ty), Diagnostic> {
        todo!()
    }

    fn match_expr(&mut self, ast: &ast::MatchExpr) -> Result<(String, Ty), Diagnostic> {
        let (target, target_ty) = self.expr(&ast.target)?;

        let trg = self.fresh_ident();
        let res = self.fresh_ident();

        self.stmts.push(format!("local {trg} = {}", target));
        self.stmts.push(format!("local {res}"));

        let mut output = Ty::Bot;
        let mut input = Ty::Bot;

        for (i, arm) in ast.arms.iter().enumerate() {
            let ast::Pattern::Ty {
                ref ty,
                ref binding,
                ..
            } = arm.pattern;

            let check = match ty {
                ast::Ty::Float(_) => todo!(),

                ast::Ty::Int(_) => format!("has_type_tag({trg}, 'int')"),
                ast::Ty::Str(_) => format!("has_type_tag({trg}, 'str')"),
                ast::Ty::True(_) => format!("has_type_tag({trg}, 'true')"),
                ast::Ty::False(_) => format!("has_type_tag({trg}, 'false')"),
                ast::Ty::None(_) => format!("has_type_tag({trg}, 'none')"),

                ast::Ty::Never(_) => continue,

                ast::Ty::Path(path) => {
                    assert_eq!(path.segments.len(), 1);
                    let name = path.segments[0].name;

                    format!("has_type_tag({trg}, '{}')", name)
                }

                ast::Ty::Wild(_) => {
                    let ty = Ty::Var(self.solver.fresh_var());
                    let ty = Ty::inter(ty.clone(), Ty::neg(input.clone()));
                    input = Ty::union(input, ty.clone());

                    self.stmts.push(match i > 0 {
                        false => String::from("if true then"),
                        true => String::from("else"),
                    });

                    if let Some(binding) = binding {
                        self.binding(&ty, binding, &trg)?;
                    }

                    let (value, value_ty) = self.expr(&arm.body)?;
                    output = Ty::union(output, value_ty);

                    self.stmts.push(format!("{res} = {}", value));

                    break;
                }

                ast::Ty::Generic(_)
                | ast::Ty::Ref { .. }
                | ast::Ty::Func { .. }
                | ast::Ty::List { .. }
                | ast::Ty::Tuple { .. }
                | ast::Ty::Union { .. }
                | ast::Ty::Record { .. } => todo!(),
            };

            let ty = self.lower_ty(ty)?;
            let ty = Ty::inter(ty.clone(), Ty::neg(input.clone()));

            input = Ty::union(input, ty.clone());

            self.stmts.push(match i > 0 {
                false => format!("if {} then", check),
                true => format!("elseif {} then", check),
            });

            if let Some(binding) = binding {
                self.binding(&ty, binding, &trg)?;
            }

            let (value, value_ty) = self.expr(&arm.body)?;
            output = Ty::union(output, value_ty);

            self.stmts.push(format!("{res} = {value}"));
        }

        self.stmts.push("end".to_string());

        self.solver.subty(&target_ty, &input, ast.span);

        Ok((res, output))
    }

    fn loop_expr(&mut self, ast: &ast::LoopExpr) -> Result<(String, Ty), Diagnostic> {
        let ty = Ty::Var(self.solver.fresh_var());

        let res = self.fresh_ident();

        self.stmts.push(format!("local {res}"));
        self.stmts.push(String::from("while true do"));

        let old_ty = self.loop_var.replace((res.clone(), ty.clone()));

        let (_, _) = self.expr(&ast.body)?;

        self.loop_var = old_ty;

        self.stmts.push(String::from("end"));

        Ok((res, ty))
    }

    fn break_expr(&mut self, ast: &ast::BreakExpr) -> Result<(String, Ty), Diagnostic> {
        let Some((res, ty)) = self.loop_var.clone() else {
            let diagnostic = Diagnostic::error("invalid::break")
                .message("break outside of loop")
                .span(ast.span);

            return Err(diagnostic);
        };

        if let Some(value) = ast.value.clone() {
            let (value, value_ty) = self.expr(&value)?;
            self.solver.subty(&value_ty, &ty, ast.span);

            self.stmts.push(format!("{} = {}", res, value));
        }

        self.stmts.push("break".to_string());

        Ok((String::from("nil"), Ty::NONE))
    }

    fn let_expr(&mut self, ast: &ast::LetExpr) -> Result<(String, Ty), Diagnostic> {
        let (expr, ty) = self.expr(&ast.value)?;

        if let Some(ref expected) = ast.ty {
            let expected = self.lower_ty(expected)?;
            self.solver.subty(&ty, &expected, ast.span);
        }

        self.binding(&ty, &ast.binding, &expr)?;

        Ok((String::from("make_none()"), Ty::NONE))
    }

    fn block_expr(&mut self, ast: &ast::BlockExpr) -> Result<(String, Ty), Diagnostic> {
        let scope = self.scope.len();

        let mut code = String::from("make_none()");
        let mut ty = Ty::NONE;

        for expr in &ast.exprs {
            self.stmts.push(format!("local _ = {}", code));
            (code, ty) = self.expr(expr)?;
        }

        self.scope.truncate(scope);

        Ok((code, ty))
    }
}
