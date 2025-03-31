use std::collections::HashMap;

use crate::{
    ast, attribute::Attributes, diagnostic::Reporter, hir2 as hir, parse::Interner, solve,
};

mod expr;
mod import;
mod ty;

#[derive(Clone)]
struct FuncInfo {
    /// The module that the function is defined in.
    module: hir::ModuleId,

    /// The number of generics that the function has.
    generics: usize,

    /// The AST of the function.
    ast: ast::Func,
}

#[derive(Clone)]
struct AliasInfo {
    /// The module that the alias is defined in.
    module: hir::ModuleId,

    /// The number of generics that the alias has.
    generics: usize,

    /// The AST of the alias.
    ast: ast::Alias,
}

#[derive(Clone)]
struct TypeInfo {
    /// The module that the type is defined in.
    module: hir::ModuleId,

    /// The number of generics that the type has.
    generics: usize,

    /// The body of the type constructor.
    ///
    /// This is `None` if the type is extern.
    body: Option<hir::BodyId>,

    /// The AST of the type.
    ast: ast::Type,
}

pub struct Lowerer<'a> {
    reporter: &'a mut dyn Reporter,
    interner: &'a mut Interner,

    modules: Vec<(hir::ModuleId, ast::Module)>,

    program: hir::Program,
    root: hir::ModuleId,

    funcs: HashMap<hir::BodyId, FuncInfo>,
    aliases: HashMap<solve::Tag, AliasInfo>,
    types: HashMap<solve::Tag, TypeInfo>,

    next_tag: u64,
}

impl<'a> Lowerer<'a> {
    pub fn new(reporter: &'a mut dyn Reporter, interner: &'a mut Interner) -> Self {
        let mut program = hir::Program::new();
        let root = program.modules.insert(hir::Module::new());

        Self {
            reporter,
            interner,

            modules: Vec::new(),

            program,
            root,

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

        self.program.solver.finish(self.reporter)?;

        Ok(self.program)
    }

    fn lower_types(&mut self) -> Result<(), ()> {
        for (tag, info) in self.types.clone() {
            let mut generics = Vec::new();
            let mut args = Vec::new();

            if let Some(ref ast_generics) = info.ast.generics {
                for generic in &ast_generics.params {
                    let var = self.program.solver.fresh_var();
                    generics.push((generic.name, var));
                    args.push(var);
                }
            }

            // we have to generate the constructor function, this is not pretty...
            let ty_body = match &info.ast.ty {
                Some(ty) => {
                    let mut generics = Generics::Explicit(&generics);

                    let body = self.lower_ty(info.module, &mut generics, false, ty)?;

                    let ty = body.clone().with_tag(tag);

                    if let Some(body_id) = info.body {
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
                            name: tag.name.to_string(),
                            generics: args.clone(),
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

                        self.program[body_id] = body;
                    }

                    ty
                }

                None => {
                    let ty = solve::Ty::tag(tag);

                    if let Some(body_id) = info.body {
                        let body = hir::Body {
                            attrs: Attributes::new(),
                            is_extern: false,
                            name: tag.name.to_string(),
                            generics: args.clone(),
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

                        self.program[body_id] = body;
                    }

                    solve::Ty::tag(tag)
                }
            };

            self.program.solver.add_applicable(tag, ty_body, args);
        }

        Ok(())
    }

    fn lower_aliases(&mut self) -> Result<(), ()> {
        for (tag, info) in self.aliases.clone() {
            let mut generics = Vec::new();
            let mut args = Vec::new();

            if let Some(ref ast_generics) = info.ast.generics {
                for generic in &ast_generics.params {
                    let ty = self.program.solver.fresh_var();
                    generics.push((generic.name, ty));
                    args.push(ty);
                }
            }

            let mut generics = Generics::Explicit(&generics);
            let body = self.lower_ty(info.module, &mut generics, false, &info.ast.ty)?;

            self.program.solver.add_applicable(tag, body, args);
        }

        Ok(())
    }

    fn lower_functions(&mut self) -> Result<(), ()> {
        let mut calls = HashMap::new();

        while let Some(&body_id) = self.funcs.keys().next() {
            let info = self.funcs.remove(&body_id).unwrap();

            self.lower_function(&mut calls, body_id, info)?;

            let mut ty = self.program[body_id].ty();
            self.program.solver.simplify_deep(&mut ty);
        }

        Ok(())
    }
}

pub enum Generics<'a> {
    Explicit(&'a [(&'static str, solve::Var)]),
    Implicit(&'a mut Vec<(&'static str, solve::Var)>),
}
