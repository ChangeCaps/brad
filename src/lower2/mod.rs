use std::{
    collections::{HashMap, HashSet},
    mem,
};

use crate::{ast, attribute::Attributes, diagnostic::Reporter, hir2 as hir, solve};

mod expr;
mod import;
mod ty;

struct FuncInfo {
    /// The module that the function is defined in.
    module: hir::ModuleId,

    /// The number of generics that the function has.
    generics: usize,

    /// The AST of the function.
    ast: ast::Func,
}

struct AliasInfo {
    /// The module that the alias is defined in.
    module: hir::ModuleId,

    /// The number of generics that the alias has.
    generics: usize,

    /// The AST of the alias.
    ast: ast::Alias,
}

struct TypeInfo {
    /// The module that the type is defined in.
    module: hir::ModuleId,

    /// The number of generics that the type has.
    generics: usize,

    /// The body of the type constructor.
    body: hir::BodyId,

    /// The AST of the type.
    ast: ast::Type,
}

pub struct Lowerer<'r> {
    reporter: &'r mut dyn Reporter,

    modules: Vec<(hir::ModuleId, ast::Module)>,

    program: hir::Program,
    root: hir::ModuleId,

    solver: solve::Solver,

    funcs: HashMap<hir::BodyId, FuncInfo>,
    aliases: HashMap<solve::Tag, AliasInfo>,
    types: HashMap<solve::Tag, TypeInfo>,

    next_tag: u64,
}

impl<'a> Lowerer<'a> {
    pub fn new(reporter: &'a mut dyn Reporter) -> Self {
        let mut program = hir::Program::new();
        let root = program.modules.insert(hir::Module::new());

        Self {
            reporter,

            modules: Vec::new(),

            program,
            root,

            solver: solve::Solver::new(Default::default()),

            funcs: HashMap::new(),
            aliases: HashMap::new(),
            types: HashMap::new(),

            next_tag: 0,
        }
    }

    pub fn add_module(&mut self, path: &[&'static str], ast: ast::Module) {
        let module = self
            .program
            .modules
            .insert_module(self.root, path, hir::Vis::Pub);

        self.modules.push((module, ast));
    }

    pub fn finish(mut self) -> Result<hir::Program, ()> {
        self.resolve_imports()?;
        self.lower_types()?;
        self.lower_aliases()?;
        self.lower_functions()?;

        Ok(self.program)
    }

    fn lower_types(&mut self) -> Result<(), ()> {
        for (tag, info) in mem::take(&mut self.types) {
            let mut generics = Vec::new();

            if let Some(ref ast_generics) = info.ast.generics {
                for generic in &ast_generics.params {
                    let ty = solve::Ty::Var(self.solver.fresh_var());
                    generics.push((generic.name, ty));
                }
            }

            let params: Vec<_> = generics.iter().map(|(_, ty)| ty.clone()).collect();

            // we have to generate the constructor function, this is not pretty...
            let body = match &info.ast.ty {
                Some(ty) => {
                    let mut generics = Generics::Explicit(&generics);

                    let body = self.lower_ty(info.module, &mut generics, false, ty)?;

                    let ty = solve::Ty::inter(solve::Ty::Tag(tag), body.clone());

                    let mut locals = hir::Locals::new();

                    let local = locals.insert(hir::Local {
                        is_mutable: false,
                        name: "input",
                        ty: body.clone(),
                        span: info.ast.span,
                    });

                    let body = hir::Body {
                        attrs: Attributes::new(),
                        is_extern: false,
                        name: info.ast.name.to_string(),
                        generics: params.clone(),
                        locals,
                        input: vec![hir::Argument {
                            binding: hir::Binding::Bind {
                                local,
                                span: info.ast.span,
                            },
                            ty: body.clone(),
                        }],
                        output: ty.clone(),
                        expr: Some(hir::Expr {
                            kind: hir::ExprKind::Tag(
                                tag,
                                Box::new(hir::Expr {
                                    kind: hir::ExprKind::Local(local),
                                    ty: body.clone(),
                                    span: info.ast.span,
                                }),
                            ),
                            ty: ty.clone(),
                            span: info.ast.span,
                        }),
                        span: info.ast.span,
                    };

                    self.program[info.body] = body;

                    ty
                }

                None => {
                    let ty = solve::Ty::Tag(tag);

                    let body = hir::Body {
                        attrs: Attributes::new(),
                        is_extern: false,
                        name: info.ast.name.to_string(),
                        generics: params.clone(),
                        locals: hir::Locals::new(),
                        input: Vec::new(),
                        output: ty.clone(),
                        expr: Some(hir::Expr {
                            kind: hir::ExprKind::ZeroSize(tag),
                            ty: ty.clone(),
                            span: info.ast.span,
                        }),
                        span: info.ast.span,
                    };

                    self.program[info.body] = body;

                    solve::Ty::Tag(tag)
                }
            };

            self.program.solver.add_applicable(tag, body, params);
        }

        Ok(())
    }

    fn lower_aliases(&mut self) -> Result<(), ()> {
        for (tag, info) in mem::take(&mut self.aliases) {
            let mut generics = Vec::new();

            if let Some(ref ast_generics) = info.ast.generics {
                for generic in &ast_generics.params {
                    let ty = solve::Ty::Var(self.solver.fresh_var());
                    generics.push((generic.name, ty));
                }
            }

            let params: Vec<_> = generics.iter().map(|(_, ty)| ty.clone()).collect();

            let mut generics = Generics::Explicit(&generics);
            let body = self.lower_ty(info.module, &mut generics, false, &info.ast.ty)?;

            self.program.solver.add_applicable(tag, body, params);
        }

        Ok(())
    }

    fn lower_functions(&mut self) -> Result<(), ()> {
        let mut calls = HashMap::new();

        while let Some(&body_id) = self.funcs.keys().next() {
            let info = self.funcs.remove(&body_id).unwrap();

            self.lower_function(&mut calls, body_id, info)?;
        }

        for (_, body) in self.program.bodies.iter() {
            println!("{}: {}", body.name, self.solver.format_ty(&body.ty()));
        }

        Ok(())
    }
}

pub enum Generics<'a> {
    Explicit(&'a [(&'static str, solve::Ty)]),
    Implicit(&'a mut Vec<(&'static str, solve::Ty)>),
}
