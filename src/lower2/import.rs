use std::{collections::HashMap, mem};

use diagnostic::{Diagnostic, Span};

use crate::{ast, attribute::Attributes, hir2 as hir};

use super::{AliasInfo, FuncInfo, Lowerer, TypeInfo};

type Imports = HashMap<(hir::ModuleId, &'static str), ast::Path>;

impl Lowerer<'_> {
    pub(super) fn resolve_imports(&mut self) -> Result<(), ()> {
        self.import_packages();

        let mut imports = self.register_items()?;

        while let Some(&(id, name)) = imports.keys().next() {
            self.resolve_import(&mut imports, id, name)?;
        }

        Ok(())
    }

    fn import_packages(&mut self) {
        for id in self.program.modules.ids() {
            let packages: Vec<_> = self.program[self.root]
                .modules
                .iter()
                .map(|(name, (id, _))| (*name, (*id, hir::Vis::Priv)))
                .collect();

            self.program[id].modules.extend(packages);
        }
    }

    fn register_items(&mut self) -> Result<Imports, ()> {
        let mut imports = HashMap::new();

        for (module, ast) in mem::take(&mut self.modules) {
            for decl in ast.decls {
                match decl {
                    ast::Decl::Func(ref ast) => {
                        let mut input = Vec::new();

                        for arg in &ast.args {
                            input.push(hir::Argument {
                                binding: hir::Binding::Wild { span: arg.span },
                                ty: solve::Type::fresh_var(),
                            });
                        }

                        let output = solve::Type::fresh_var();

                        // lower the generics
                        let mut generics = Vec::new();

                        for _ in ast.generics() {
                            generics.push(solve::Var::fresh());
                        }

                        let sub_module = self.program.modules.insert_module(
                            module,
                            &ast.name.segments[..ast.name.segments.len() - 1],
                            hir::Vis::Pub,
                        );

                        // create the full name of the function
                        let short_name = ast.name.segments.last().unwrap();
                        let module_name = self.program[sub_module].name.as_ref().unwrap();
                        let full_name = format!("{}::{}", module_name, short_name);

                        // create a body for the function
                        // with temporary data that will be filled in later
                        let body = hir::Body {
                            attrs: ast.attrs.clone(),
                            is_extern: ast.is_extern,
                            name: full_name,
                            generics,
                            locals: hir::Locals::new(),
                            input,
                            output,
                            expr: None,
                            span: ast.span,
                        };

                        // insert the body into the hir program
                        let body_id = self.program.bodies.insert(body);

                        let info = FuncInfo {
                            module,
                            generics: ast.generics().count(),
                            ast: ast.clone(),
                        };

                        self.funcs.insert(body_id, info);

                        // insert the body into the module
                        let result = self.program[sub_module].bodies.insert(
                            short_name, //
                            (body_id, hir::Vis::Pub),
                        );

                        // check if the function was already declared
                        if let Some((body_id, _)) = result {
                            let existing_span = self.program.bodies[body_id].span;

                            let diagnostic = Diagnostic::error("duplicate::function")
                                .message(format!("duplicate declaration function `{}`", ast.name))
                                .label(ast.span, "conflicting declaration")
                                .label(existing_span, "previously declared here");

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }
                    }

                    ast::Decl::Type(ast::Type {
                        ref name,
                        span,
                        ref generics,
                        ..
                    })
                    | ast::Decl::Alias(ast::Alias {
                        ref name,
                        span,
                        ref generics,
                        ..
                    }) => {
                        let sub_module = self.program.modules.insert_module(
                            module,
                            &name.segments[..name.segments.len() - 1],
                            hir::Vis::Pub,
                        );

                        let short_name = name.segments.last().unwrap();
                        let module_name = self.program[sub_module].name.as_ref().unwrap();
                        let full_name = format!("{}::{}", module_name, short_name);
                        let full_name = self.interner.intern(&full_name);

                        let tag = solve::Tag::generate(full_name);

                        // compute the number of generics
                        let generics = generics.as_ref().map_or(0, |g| g.params.len());

                        // insert the tag into the module
                        let result = self.program[sub_module].types.insert(
                            short_name, //
                            (tag, hir::Vis::Pub, span),
                        );

                        // check if the type was already declared
                        if let Some((_, _, existing_span)) = result {
                            let diagnostic = Diagnostic::error("duplicate::type")
                                .message(format!("duplicate declaration type `{}`", name))
                                .label(span, "conflicting declaration")
                                .label(existing_span, "previously declared here");

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }

                        match decl {
                            ast::Decl::Type(ast) if ast.is_extern => {
                                let info = TypeInfo {
                                    module,
                                    generics,
                                    body: None,
                                    ast,
                                };

                                self.types.insert(tag, info);

                                continue;
                            }

                            ast::Decl::Type(_) => {}

                            // if the declaration is an alias, we need to store the alias info and
                            // we are done
                            ast::Decl::Alias(ast) => {
                                let info = AliasInfo {
                                    module,
                                    generics,
                                    ast,
                                };

                                self.aliases.insert(tag, info);

                                continue;
                            }

                            _ => unreachable!(),
                        }

                        // create a body for the type constructor
                        // with temporary data that will be filled in later
                        let body = hir::Body {
                            attrs: Attributes::new(),
                            is_extern: false,
                            name: full_name.to_string(),
                            generics: Vec::new(),
                            locals: hir::Locals::new(),
                            input: Vec::new(),
                            output: solve::Type::bottom(),
                            expr: None,
                            span,
                        };

                        // insert the body into the hir program
                        let body_id = self.program.bodies.insert(body);

                        // insert the body into the module
                        let result = self.program[sub_module].bodies.insert(
                            short_name, //
                            (body_id, hir::Vis::Pub),
                        );

                        // check if the function was already declared
                        if let Some((body_id, _)) = result {
                            let existing_span = self.program.bodies[body_id].span;

                            let diagnostic = Diagnostic::error("duplicate::function")
                                .message(format!("duplicate declaration function `{}`", name))
                                .label(span, "conflicting declaration")
                                .label(existing_span, "previously declared here");

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }

                        // store the type info
                        if let ast::Decl::Type(ast) = decl {
                            let info = TypeInfo {
                                module,
                                generics,
                                body: Some(body_id),
                                ast,
                            };

                            self.types.insert(tag, info);
                        } else {
                            unreachable!();
                        }
                    }

                    ast::Decl::Import(ast) => {
                        if ast.path.spec.is_some() {
                            let diagnostic = Diagnostic::error("import::generics")
                                .message("import paths cannot have generics")
                                .label(ast.path.span, "generics not allowed here");

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }

                        let name = ast.path.segments.last().unwrap().name;
                        imports.insert((module, name), ast.path.clone());
                    }
                }
            }
        }

        Ok(imports)
    }

    fn resolve_import(
        &mut self,
        imports: &mut Imports,
        id: hir::ModuleId,
        name: &'static str,
    ) -> Result<(), ()> {
        let path = imports.remove(&(id, name)).unwrap();

        let first = path.segments.first().unwrap();
        let last = path.segments.last().unwrap();

        let segments = path
            .segments
            .iter()
            .take(path.segments.len() - 1)
            .map(|s| s.name)
            .collect::<Vec<_>>();

        let span = path
            .segments
            .iter()
            .take(path.segments.len() - 1)
            .map(|s| s.span)
            .fold(first.span, Span::join);

        let Some(module) = self.program.modules.get_module(id, &segments) else {
            let diagnostic = Diagnostic::error("import::module")
                .message("module not found")
                .label(span, "here");

            self.reporter.emit(diagnostic);

            return Err(());
        };

        if imports.contains_key(&(module, name)) {
            self.resolve_import(imports, module, name)?;
        }

        let mut has_any = false;

        if let Some((body, _)) = self.program[module].bodies.get(name).cloned() {
            has_any = true;

            if !self.program[id].bodies.contains_key(name) {
                let value = (body, hir::Vis::Priv);
                self.program[id].bodies.insert(name, value);
            }
        }

        if let Some((ty, _, _)) = self.program[module].types.get(name).cloned() {
            has_any = true;

            if !self.program[id].types.contains_key(name) {
                let value = (ty, hir::Vis::Priv, path.span);
                self.program[id].types.insert(name, value);
            }
        }

        if let Some((module, _)) = self.program[module].modules.get(name).cloned() {
            has_any = true;

            if !self.program[id].modules.contains_key(name) {
                let value = (module, hir::Vis::Priv);
                self.program[id].modules.insert(name, value);
            }
        }

        if !has_any {
            let diagnostic = Diagnostic::error("import::item")
                .message("item not found")
                .label(last.span, "here");

            self.reporter.emit(diagnostic);

            return Err(());
        }

        Ok(())
    }
}
