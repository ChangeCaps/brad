use std::{collections::HashMap, mem};

use crate::{
    ast,
    attribute::Attributes,
    diagnostic::{Diagnostic, Span},
    hir2 as hir, solve,
};

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
                                ty: solve::Ty::Var(self.program.solver.fresh_var()),
                            });
                        }

                        let output = solve::Ty::Var(self.program.solver.fresh_var());

                        let generics = ast
                            .generics()
                            .map(|_| solve::Ty::Var(self.program.solver.fresh_var()))
                            .collect();

                        // create a body for the function
                        // with temporary data that will be filled in later
                        let body = hir::Body {
                            attrs: ast.attrs.clone(),
                            is_extern: ast.is_extern,
                            name: ast.name.to_string(),
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
                        let result = self.program.modules.insert_body(
                            module,
                            &ast.name.segments,
                            body_id,
                            hir::Vis::Pub,
                        );

                        // check if the function was already declared
                        if let Err(body_id) = result {
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
                        let tag_name = self.interner.intern(&name.to_string());
                        let tag = self.make_tag(tag_name);

                        // compute the number of generics
                        let generics = generics.as_ref().map_or(0, |g| g.params.len());

                        // insert the tag into the module
                        let result = self.program.modules.insert_type(
                            module,
                            &name.segments,
                            tag,
                            hir::Vis::Pub,
                            span,
                        );

                        // check if the type was already declared
                        if let Err(existing_span) = result {
                            let diagnostic = Diagnostic::error("duplicate::type")
                                .message(format!("duplicate declaration type `{}`", name))
                                .label(span, "conflicting declaration")
                                .label(existing_span, "previously declared here");

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }

                        match decl {
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
                            name: name.to_string(),
                            generics: Vec::new(),
                            locals: hir::Locals::new(),
                            input: Vec::new(),
                            output: solve::Ty::NONE,
                            expr: None,
                            span,
                        };

                        // insert the body into the hir program
                        let body_id = self.program.bodies.insert(body);

                        // insert the body into the module
                        let result = self.program.modules.insert_body(
                            module,
                            &name.segments,
                            body_id,
                            hir::Vis::Pub,
                        );

                        // check if the function was already declared
                        if let Err(body_id) = result {
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
                                body: body_id,
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

            let value = (body, hir::Vis::Priv);
            self.program[id].bodies.insert(name, value);
        }

        if let Some((ty, _, _)) = self.program[module].types.get(name).cloned() {
            has_any = true;

            let value = (ty, hir::Vis::Priv, path.span);
            self.program[id].types.insert(name, value);
        }

        if let Some((module, _)) = self.program[module].modules.get(name).cloned() {
            has_any = true;

            let value = (module, hir::Vis::Priv);
            self.program[id].modules.insert(name, value);
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

    fn make_tag(&mut self, name: &'static str) -> solve::Tag {
        let tag = self.next_tag;
        self.next_tag += 1;

        solve::Tag::new(name, tag)
    }
}
