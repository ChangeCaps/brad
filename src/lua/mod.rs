use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::{Deref, DerefMut},
};

use crate::{
    ast,
    diagnostic::Diagnostic,
    solve::{Options, Solver, Ty},
};

const LUA_PRELUDE: &str = r#"
local function dump(value)
  if type(value) == 'table' then
    local result = '{'
    for k, v in pairs(value) do
      result = result .. '[' .. dump(k) .. '] = ' .. dump(v) .. ', '
    end
    return result .. '}'
  elseif type(value) == 'string' then
    return '"' .. value .. '"'
  else
    return tostring(value)
  end
end

local function make_true()
  local value = {}
  setmetatable(value, { __type_tags = { ['true'] = true } })
  return value
end

local function make_false()
  local value = {}
  setmetatable(value, { __type_tags = { ['false'] = true } })
  return value
end

local function make_none()
  local value = {}
  setmetatable(value, { __type_tags = { ['none'] = true } })
  return value
end

local function make_bool(value)
  if value then
    return make_true()
  else
    return make_false()
  end
end

local function make_int(value)
  local value = { value = value }
  setmetatable(value, { __type_tags = { ['int'] = true } })
  return value
end

local function make_float(value)
  local value = { value = value }
  setmetatable(value, { __type_tags = { ['float'] = true } })
  return value
end

local function make_str(value)
  local value = { value = value }
  setmetatable(value, { __type_tags = { ['str'] = true } })
  return value
end

local function add_type_tag(value, tag)
  getmetatable(value).__type_tags[tag] = true
  return value
end

local function has_type_tag(value, tag)
  return type(value) == 'table' and getmetatable(value).__type_tags[tag] ~= nil
end

local M = {}
"#;

pub struct Codegen {
    solver: Solver,
    types: HashSet<&'static str>,
    funcs: HashMap<&'static str, (Ty, bool)>,
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            solver: Solver::new(Options::default()),
            types: HashSet::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn finish(&mut self, ast: ast::Module) -> Result<String, Diagnostic> {
        for decl in &ast.decls {
            match decl {
                ast::Decl::Type(ast) => {
                    self.types.insert(ast.name);
                }

                ast::Decl::Alias(ast) => {
                    self.types.insert(ast.name);
                }

                _ => {}
            }
        }

        let mut code = String::new();

        code.push_str(LUA_PRELUDE);
        code.push('\n');

        for decl in &ast.decls {
            match decl {
                ast::Decl::Type(ast) => {
                    let name = Ty::Name(ast.name);

                    let mut ctx = Vec::new();

                    if let Some(generics) = &ast.generics {
                        for generic in &generics.generics {
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
                            code.push_str(&constructor);

                            let output = Ty::inter(name.clone(), ty.clone());

                            let func = Ty::func(ty, output.clone());
                            self.funcs.insert(ast.name, (func, true));

                            output
                        }

                        None => name,
                    };

                    let args = ctx.iter().map(|(_, ty)| ty.clone()).collect();
                    self.solver.add_applicable(ast.name, ty, args);
                }

                ast::Decl::Alias(ast) => {
                    let mut ctx = Vec::new();

                    if let Some(generics) = &ast.generics {
                        for generic in &generics.generics {
                            let ty = Ty::Var(self.solver.fresh_var());
                            ctx.push((generic.name, ty.clone()));
                        }
                    }

                    let ty = self.lower_ty(&mut ctx, &ast.ty)?;

                    let args = ctx.iter().map(|(_, ty)| ty.clone()).collect();
                    self.solver.add_applicable(ast.name, ty.clone(), args);
                }

                _ => {}
            }
        }

        for decl in &ast.decls {
            if let ast::Decl::Func(ast) = decl {
                let mut codegen = ExprCodegen {
                    codegen: self,
                    generics: vec![],
                    stmts: vec![],
                    scope: vec![],
                    next_ident: 0,
                };

                // resolve explicit generics
                if let Some(generics) = &ast.generics {
                    for generic in &generics.generics {
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
                codegen.funcs.insert(ast.name, (func_ty.clone(), false));

                // generate the function body
                let (value, ty) = codegen.expr(ast.body.as_ref().unwrap())?;

                // constrain the output type, to the type of the body
                let span = ast.output.as_ref().map_or(ast.span, |ty| ty.span());
                codegen.solver.subty(&ty, &output, span)?;

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
                code.push_str(&format!("M['{}'] = function()\n{}end\n\n", ast.name, body));

                // register the function as complete
                println!("{}: {}", ast.name, self.solver.format_ty(&func_ty));
                self.funcs.insert(ast.name, (func_ty, true));
            }
        }

        code.push_str("print(dump(M.main()))");

        Ok(code)
    }

    fn lower_ty(&mut self, ctx: &mut dyn GenericContext, ty: &ast::Ty) -> Result<Ty, Diagnostic> {
        Ok(match ty {
            ast::Ty::Wild(_) => Ty::Var(self.solver.fresh_var()),
            ast::Ty::Int(_) => Ty::Name("int"),
            ast::Ty::Float(_) => Ty::Name("float"),
            ast::Ty::Str(_) => Ty::Name("str"),
            ast::Ty::True(_) => Ty::Name("true"),
            ast::Ty::False(_) => Ty::Name("false"),
            ast::Ty::None(_) => Ty::Name("none"),
            ast::Ty::Never(_) => Ty::Bot,
            ast::Ty::Generic(generic) => ctx.generic(&mut self.solver, generic.name)?,
            ast::Ty::Path(path) => {
                assert_eq!(path.segments.len(), 1);
                let name = path.segments[0].name;

                if !self.types.contains(name) {
                    let diagnostic = Diagnostic::error("invalid::path")
                        .message(format!("unknown type `{}`", name))
                        .span(path.span);

                    return Err(diagnostic);
                }

                match path.spec {
                    Some(ref spec) => {
                        let mut tys = Vec::new();

                        for ty in &spec.tys {
                            let ty = self.lower_ty(ctx, ty)?;
                            tys.push(ty);
                        }

                        Ty::app(name, tys)
                    }

                    None => {
                        let args = self.solver.applicable_args(name).unwrap();

                        let args = (0..args.len())
                            .map(|_| Ty::Var(self.solver.fresh_var()))
                            .collect();

                        Ty::app(name, args)
                    }
                }
            }

            ast::Ty::Ref { ty, .. } => Ty::union(Ty::Name("ref"), self.lower_ty(ctx, ty)?),

            ast::Ty::Func { input, output, .. } => {
                let input = self.lower_ty(ctx, input)?;
                let output = self.lower_ty(ctx, output)?;
                Ty::Func(Box::new(input), Box::new(output))
            }

            ast::Ty::List { .. } => todo!(),
            ast::Ty::Tuple { .. } => todo!(),

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
    scope: Vec<(&'static str, Ty)>,
    stmts: Vec<String>,
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

    fn push_scope(&mut self, name: &'static str, ty: Ty) -> String {
        let var = format!("var{}", self.scope.len());
        self.scope.push((name, ty));
        var
    }

    fn find_scope(&mut self, name: &str) -> Option<usize> {
        for (i, (n, _)) in self.scope.iter().enumerate().rev() {
            if *n == name {
                return Some(i);
            }
        }

        None
    }

    fn binding(&mut self, ty: &Ty, ast: &ast::Binding, expr: &str) -> Result<(), Diagnostic> {
        match ast {
            ast::Binding::Wild { .. } => Ok(()),
            ast::Binding::Bind { name, .. } => {
                let var = self.push_scope(name, ty.clone());

                self.stmts.push(format!("local {} = {}", var, expr));

                Ok(())
            }
            ast::Binding::Tuple { .. } => unimplemented!("tuple bindings"),
        }
    }

    fn expr(&mut self, ast: &ast::Expr) -> Result<(String, Ty), Diagnostic> {
        match ast {
            ast::Expr::Literal(ast) => self.literal_expr(ast),
            ast::Expr::List(list_expr) => todo!(),
            ast::Expr::Record(ast) => self.record_expr(ast),
            ast::Expr::Tuple(tuple_expr) => todo!(),
            ast::Expr::Path(ast) => self.path_expr(ast),
            ast::Expr::Index(index_expr) => todo!(),
            ast::Expr::Field(ast) => self.field_expr(ast),
            ast::Expr::Unary(unary_expr) => todo!(),
            ast::Expr::Binary(ast) => self.binary_expr(ast),
            ast::Expr::Call(ast) => self.call_expr(ast),
            ast::Expr::Assign(ast) => self.assign_expr(ast),
            ast::Expr::Ref(ref_expr) => todo!(),
            ast::Expr::Match(ast) => self.match_expr(ast),
            ast::Expr::Loop(loop_expr) => todo!(),
            ast::Expr::Break(break_expr) => todo!(),
            ast::Expr::Let(ast) => self.let_expr(ast),
            ast::Expr::Block(ast) => self.block_expr(ast),
        }
    }

    fn literal_expr(&mut self, ast: &ast::Literal) -> Result<(String, Ty), Diagnostic> {
        Ok(match ast {
            ast::Literal::Int { value, .. } => (format!("make_int({})", value), Ty::Name("int")),

            ast::Literal::Float { value, .. } => {
                (format!("make_float({})", value), Ty::Name("float"))
            }

            ast::Literal::String { value, .. } => {
                (format!("make_str('{}')", value), Ty::Name("str"))
            }

            ast::Literal::True { .. } => (String::from("make_true()"), Ty::Name("true")),
            ast::Literal::False { .. } => (String::from("make_false()"), Ty::Name("false")),
            ast::Literal::None { .. } => (String::from("make_none()"), Ty::Name("none")),
        })
    }

    fn record_expr(&mut self, ast: &ast::RecordExpr) -> Result<(String, Ty), Diagnostic> {
        let mut types = BTreeMap::new();
        let mut fields = Vec::new();

        for field in &ast.fields {
            let (value, ty) = self.expr(&field.value)?;
            types.insert(field.name, ty);
            fields.push(format!("{} = {}", field.name, value));
        }

        let temp = self.fresh_ident();
        (self.stmts).push(format!("local {temp} = {{{}}}", fields.join(", ")));
        (self.stmts).push(format!("setmetatable({temp}, {{ __type_tags = {{}} }})"));

        Ok((temp, Ty::Record(types)))
    }

    fn path_expr(&mut self, ast: &ast::Path) -> Result<(String, Ty), Diagnostic> {
        assert_eq!(ast.segments.len(), 1);
        let name = ast.segments[0].name;

        if let Some(idx) = self.find_scope(name) {
            let (_, ty) = self.scope[idx].clone();

            return Ok((format!("var{}", idx), ty));
        }

        if let Some((func, complete)) = self.funcs.get(name).cloned() {
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

    fn field_expr(&mut self, ast: &ast::FieldExpr) -> Result<(String, Ty), Diagnostic> {
        let (target, target_ty) = self.expr(&ast.target)?;

        let ty = Ty::Var(self.solver.fresh_var());

        let record_ty = Ty::record([(ast.name, ty.clone())]);

        self.solver.subty(&target_ty, &record_ty, ast.span)?;

        Ok((format!("{}.{}", target, ast.name), ty))
    }

    fn binary_expr(&mut self, ast: &ast::BinaryExpr) -> Result<(String, Ty), Diagnostic> {
        let (lhs, lhs_ty) = self.expr(&ast.lhs)?;
        let (rhs, rhs_ty) = self.expr(&ast.rhs)?;

        match ast.op {
            ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Mod => {
                self.solver.subty(&lhs_ty, &Ty::Name("int"), ast.span)?;
                self.solver.subty(&rhs_ty, &Ty::Name("int"), ast.span)?;

                let op = match ast.op {
                    ast::BinaryOp::Add => "+",
                    ast::BinaryOp::Sub => "-",
                    ast::BinaryOp::Mul => "*",
                    ast::BinaryOp::Div => "/",
                    ast::BinaryOp::Mod => "%",
                    _ => unreachable!(),
                };

                let value = format!("make_int({lhs}.value {op} {rhs}.value)");

                Ok((value, Ty::Name("int")))
            }

            ast::BinaryOp::BitAnd => todo!(),
            ast::BinaryOp::BitOr => todo!(),
            ast::BinaryOp::BitXor => todo!(),
            ast::BinaryOp::Shl => todo!(),
            ast::BinaryOp::Shr => todo!(),

            ast::BinaryOp::Eq | ast::BinaryOp::Ne => {
                self.solver.subty(&lhs_ty, &rhs_ty, ast.span)?;
                self.solver.subty(&rhs_ty, &lhs_ty, ast.span)?;

                let op = match ast.op {
                    ast::BinaryOp::Eq => "==",
                    ast::BinaryOp::Ne => "~=",
                    _ => unreachable!(),
                };

                let ty = Ty::union(Ty::Name("true"), Ty::Name("false"));
                let value = format!("make_bool({lhs} {op} {rhs})");

                Ok((value, ty))
            }

            ast::BinaryOp::And | ast::BinaryOp::Or => {
                let ty = Ty::union(Ty::Name("true"), Ty::Name("false"));

                self.solver.subty(&lhs_ty, &ty, ast.span)?;
                self.solver.subty(&rhs_ty, &ty, ast.span)?;

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
                self.solver.subty(&lhs_ty, &Ty::Name("int"), ast.span)?;
                self.solver.subty(&rhs_ty, &Ty::Name("int"), ast.span)?;

                let op = match ast.op {
                    ast::BinaryOp::Lt => "<",
                    ast::BinaryOp::Le => "<=",
                    ast::BinaryOp::Gt => ">",
                    ast::BinaryOp::Ge => ">=",
                    _ => unreachable!(),
                };

                let ty = Ty::union(Ty::Name("true"), Ty::Name("false"));
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

        self.solver.subty(&target_ty, &func_ty, ast.span)?;

        Ok((format!("{}({})", target, input), output))
    }

    fn assign_expr(&mut self, ast: &ast::AssignExpr) -> Result<(String, Ty), Diagnostic> {
        let (value, value_ty) = self.expr(&ast.value)?;
        let (target, target_ty) = self.expr(&ast.target)?;

        self.solver.subty(&value_ty, &target_ty, ast.span)?;
        self.stmts.push(format!("{} = {}", target, value));

        Ok((String::from("nil"), Ty::Name("none")))
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

                    let (value, value_ty) = self.expr(&arm.expr)?;
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

            let (value, value_ty) = self.expr(&arm.expr)?;
            output = Ty::union(output, value_ty);

            self.stmts.push(format!("{res} = {value}"));
        }

        self.stmts.push("end".to_string());

        self.solver.subty(&target_ty, &input, ast.span)?;

        Ok((res, output))
    }

    fn let_expr(&mut self, ast: &ast::LetExpr) -> Result<(String, Ty), Diagnostic> {
        let (expr, ty) = self.expr(&ast.value)?;

        if let Some(ref expected) = ast.ty {
            let expected = self.lower_ty(expected)?;
            self.solver.subty(&ty, &expected, ast.span)?;
            self.solver.subty(&expected, &ty, ast.span)?;
        }

        self.binding(&ty, &ast.binding, &expr)?;

        Ok((String::from("nil"), Ty::Name("none")))
    }

    fn block_expr(&mut self, ast: &ast::BlockExpr) -> Result<(String, Ty), Diagnostic> {
        let scope = self.scope.len();

        let mut code = String::from("nil");
        let mut ty = Ty::Name("none");

        for expr in &ast.exprs {
            self.stmts.push(format!("local _ = {}", code));
            (code, ty) = self.expr(expr)?;
        }

        self.scope.truncate(scope);

        Ok((code, ty))
    }
}
