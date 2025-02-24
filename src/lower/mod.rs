use std::collections::{hash_map::Entry, HashMap};

use crate::{ast, diagnostic::Diagnostic, hir};

pub struct Lowerer {
    program: hir::Program,
    modules: Vec<(hir::ModuleId, ast::Module)>,
    imports: HashMap<hir::ModuleId, Vec<ast::Path>>,
    root: hir::ModuleId,
}

impl Lowerer {
    pub fn new() -> Self {
        let mut program = hir::Program::new();

        let root = program.modules.insert(hir::Module::new());

        Self {
            program,
            modules: Vec::new(),
            imports: HashMap::new(),
            root,
        }
    }

    pub fn add_module(&mut self, path: &[&'static str], ast: ast::Module) {
        let mut current = self.root;

        for module in path {
            match self.program.modules[current].modules.entry(module) {
                Entry::Occupied(entry) => current = *entry.get(),
                Entry::Vacant(_) => {
                    let new = self.program.modules.insert(hir::Module::new());
                    self.program.modules[current].modules.insert(module, new);
                    current = new;
                }
            }
        }

        self.modules.push((current, ast));
    }

    pub fn lower(mut self) -> Result<hir::Program, Diagnostic> {
        self.resolve_imports()?;
        self.lower_aliases()?;
        self.lower_types()?;
        self.prepare_functions()?;

        Ok(self.program)
    }

    /// Resolve all imports in the program.
    ///
    /// This is done by recursively resolving every import, one at a time.
    fn resolve_imports(&mut self) -> Result<(), Diagnostic> {
        let mut imports = HashMap::new();

        for &(m, ref ast) in &self.modules {
            for decl in &ast.decls {
                match decl {
                    ast::Decl::Func(decl) => {
                        let body = hir::Body {
                            generics: hir::Generics::new(),
                            locals: hir::Locals::new(),
                            input: Vec::new(),
                            output: hir::Ty::Never,
                            expr: hir::Expr::none(decl.span),
                            span: decl.span,
                        };

                        let body_id = self.program.bodies.insert(body);
                        let item = hir::Item::Func(body_id);
                        self.program.modules[m].items.insert(decl.name, item);
                    }

                    ast::Decl::Type(decl) => {
                        let named = hir::Named {
                            generics: hir::Generics::new(),
                            ty: None,
                        };

                        let named_id = self.program.types.insert(named);
                        let item = hir::Item::Type(named_id);
                        self.program.modules[m].items.insert(decl.name, item);
                    }

                    ast::Decl::Alias(alias) => {
                        let item = hir::Item::Alias(hir::Ty::None);
                        self.program.modules[m].items.insert(alias.name, item);
                    }

                    ast::Decl::Import(import) => {
                        let ast::PathSegment::Item(item) = import.path.segments.last().unwrap();
                        imports.insert((m, item.name), import.path.clone());
                    }
                }
            }
        }

        while let Some((module, name)) = imports.keys().next().copied() {
            let path = imports.remove(&(module, name)).unwrap();

            let (found_module, found_item) =
                resolve_import(self, &mut imports, module, path.clone())?;

            if found_module.is_none() && found_item.is_none() {
                let diagnostic = Diagnostic::error("unresolved::import")
                    .message(format!(
                        "path `{}` does not resolve to a module or item",
                        path,
                    ))
                    .label(path.span, "here");

                return Err(diagnostic);
            }

            if let Some(found) = found_module {
                self.program.modules[module].modules.insert(name, found);
            }

            if let Some(found) = found_item {
                self.program.modules[module].items.insert(name, found);
            }
        }

        Ok(())
    }

    /// Lower all aliases in the program.
    fn lower_aliases(&mut self) -> Result<(), Diagnostic> {
        for &(m, ref ast) in &self.modules {
            for decl in &ast.decls {
                let ast::Decl::Alias(decl) = decl else {
                    continue;
                };

                let mut generics = hir::Generics::new();

                let ty = if let Some(ref params) = decl.generics {
                    // if the alias has a generics list, we add each generic
                    // to the context and lower the type without allowing new
                    // generics to be defined.

                    for param in params.generics.iter() {
                        let _ = (&mut generics).resolve_generic(param);
                    }

                    lower_ty(&self.program, &generics, m, &decl.ty)?
                } else {
                    // if the alias does not have a generics list, we lower
                    // the type and allow new generics to be defined.

                    lower_ty(&self.program, &mut generics, m, &decl.ty)?
                };

                let item = hir::Item::Alias(ty);
                self.program.modules[m].items.insert(decl.name, item);
            }
        }

        Ok(())
    }

    /// Lower all types in the program.
    fn lower_types(&mut self) -> Result<(), Diagnostic> {
        for &(m, ref ast) in &self.modules {
            for decl in &ast.decls {
                let ast::Decl::Type(decl) = decl else {
                    continue;
                };

                let mut generics = hir::Generics::new();

                let Some(ref ty) = decl.ty else {
                    continue;
                };

                // same story as with aliases
                let ty = if let Some(ref params) = decl.generics {
                    for param in params.generics.iter() {
                        let _ = (&mut generics).resolve_generic(param);
                    }

                    lower_ty(&self.program, &generics, m, ty)?
                } else {
                    lower_ty(&self.program, &mut generics, m, ty)?
                };

                self.program.types.insert(hir::Named {
                    generics,
                    ty: Some(ty),
                });
            }
        }

        Ok(())
    }

    fn prepare_functions(&mut self) -> Result<(), Diagnostic> {
        for &(m, ref ast) in &self.modules {
            for decl in &ast.decls {
                let ast::Decl::Func(decl) = decl else {
                    continue;
                };

                let item = self.program.modules[m].items.get(decl.name);
                let body_id = match item {
                    Some(hir::Item::Func(body_id)) => *body_id,
                    _ => panic!("expected function body"),
                };

                let mut generics = hir::Generics::new();
                let generic_ctx: &mut dyn GenericContext = if let Some(ref params) = decl.generics {
                    for param in params.generics.iter() {
                        let _ = (&mut generics).resolve_generic(param);
                    }

                    &mut &generics
                } else {
                    &mut &mut generics
                };

                for input in &decl.args {
                    let Some(ref ty) = input.ty else {
                        let diagnostic = Diagnostic::error("unresolved::type")
                            .message("missing type in function argument")
                            .label(input.span, "found here");

                        return Err(diagnostic);
                    };

                    let ty = lower_ty(&self.program, &mut *generic_ctx, m, ty)?;
                    let body = &mut self.program.bodies[body_id];

                    let binding = lower_binding(body, input.binding.clone(), ty.clone())?;
                    body.input.push(hir::Argument { ty, binding });
                }

                let output = if let Some(ref ty) = decl.output {
                    lower_ty(&self.program, &mut *generic_ctx, m, ty)?
                } else {
                    let diagnostic = Diagnostic::error("unresolved::type")
                        .message("missing return type in function")
                        .label(decl.span, "found here");

                    return Err(diagnostic);
                };

                self.program.bodies[body_id].output = output;
                self.program.bodies[body_id].generics = generics;
            }
        }

        Ok(())
    }
}

fn lower_binding(
    body: &mut hir::Body,
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

            let local_id = body.locals.insert(local);
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
                    let binding = lower_binding(body, binding, ty)?;
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

trait GenericContext {
    fn resolve_generic(&mut self, generic: &ast::Generic) -> Result<hir::Generic, Diagnostic>;
}

impl<T: GenericContext + ?Sized> GenericContext for &mut T {
    fn resolve_generic(&mut self, generic: &ast::Generic) -> Result<hir::Generic, Diagnostic> {
        T::resolve_generic(*self, generic)
    }
}

impl GenericContext for &hir::Generics {
    fn resolve_generic(&mut self, generic: &ast::Generic) -> Result<hir::Generic, Diagnostic> {
        for param in &self.params {
            if param.name == generic.name {
                return Ok(param.generic);
            }
        }

        let diagnostic = Diagnostic::error("unresolved::generic")
            .message(format!("generic `{}` is not defined", generic.name))
            .label(generic.span, "found here");

        Err(diagnostic)
    }
}

impl GenericContext for &mut hir::Generics {
    fn resolve_generic(&mut self, generic: &ast::Generic) -> Result<hir::Generic, Diagnostic> {
        for param in &self.params {
            if param.name == generic.name {
                return Ok(param.generic);
            }
        }

        let param = hir::Param {
            name: generic.name,
            generic: hir::Generic::new(),
        };
        let generic = param.generic;
        self.params.push(param);

        Ok(generic)
    }
}

fn resolve_import(
    lowerer: &mut Lowerer,
    imports: &mut HashMap<(hir::ModuleId, &str), ast::Path>,
    mut module: hir::ModuleId,
    path: ast::Path,
) -> Result<(Option<hir::ModuleId>, Option<hir::Item>), Diagnostic> {
    for segment in &path.segments[..path.segments.len() - 1] {
        let ast::PathSegment::Item(item) = segment;

        if let Some(next) = lowerer.program.modules[module].modules.get(item.name) {
            module = *next;
            continue;
        }

        if let Some(path) = imports.get(&(module, item.name)).cloned() {
            let (found_module, _) = resolve_import(lowerer, imports, module, path.clone())?;

            if let Some(found_module) = found_module {
                module = found_module;
                continue;
            }

            let diagnostic = Diagnostic::error("unresolved::import")
                .message(format!("module does not contain item `{}`", item.name))
                .label(path.span, "imported here");

            return Err(diagnostic);
        }
    }

    let ast::PathSegment::Item(item) = path.segments.last().unwrap();

    let found_item = lowerer.program.modules[module].items.get(item.name);
    let found_module = lowerer.program.modules[module].modules.get(item.name);

    Ok((found_module.cloned(), found_item.cloned()))
}

fn lower_ty(
    hir: &hir::Program,
    mut ctx: impl GenericContext,
    module: hir::ModuleId,
    ty: &ast::Ty,
) -> Result<hir::Ty, Diagnostic> {
    lower_ty_inner(hir, &mut ctx, module, ty)
}

fn lower_ty_inner(
    hir: &hir::Program,
    ctx: &mut impl GenericContext,
    module: hir::ModuleId,
    ty: &ast::Ty,
) -> Result<hir::Ty, Diagnostic> {
    Ok(match ty {
        ast::Ty::Wild(span) => {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("wildcard type is not allowed in this context")
                .label(*span, "found here");

            return Err(diagnostic);
        }

        ast::Ty::Int(_) => hir::Ty::Int,
        ast::Ty::Float(_) => hir::Ty::Float,
        ast::Ty::Str(_) => hir::Ty::Str,
        ast::Ty::True(_) => hir::Ty::True,
        ast::Ty::False(_) => hir::Ty::False,
        ast::Ty::None(_) => hir::Ty::None,
        ast::Ty::Never(_) => hir::Ty::Never,

        ast::Ty::Generic(generic) => {
            let generic = ctx.resolve_generic(generic)?;
            hir::Ty::Generic(generic)
        }

        ast::Ty::Path(path) => resolve_ty(hir, ctx, module, path.clone())?,

        ast::Ty::Ref { ty, .. } => {
            let ty = lower_ty_inner(hir, ctx, module, ty)?;
            hir::Ty::Ref(Box::new(ty))
        }

        ast::Ty::Func { input, output, .. } => {
            let input = lower_ty_inner(hir, ctx, module, input)?;
            let output = lower_ty_inner(hir, ctx, module, output)?;

            hir::Ty::Func(Box::new(input), Box::new(output))
        }

        ast::Ty::List { ty, .. } => {
            let ty = lower_ty_inner(hir, ctx, module, ty)?;
            hir::Ty::List(Box::new(ty))
        }

        ast::Ty::Tuple { tys, .. } => {
            let tys = tys
                .iter()
                .map(|ty| lower_ty_inner(hir, ctx, module, ty))
                .collect::<Result<_, _>>()?;

            hir::Ty::Tuple(tys)
        }

        ast::Ty::Union { tys, .. } => {
            let tys = tys
                .iter()
                .map(|ty| lower_ty_inner(hir, ctx, module, ty))
                .collect::<Result<_, _>>()?;

            hir::Ty::Union(tys)
        }

        ast::Ty::Record { fields, .. } => {
            let fields = fields
                .iter()
                .map(|field| {
                    let ty = lower_ty_inner(hir, ctx, module, &field.ty)?;

                    Ok(hir::Field {
                        name: field.name,
                        ty,
                    })
                })
                .collect::<Result<_, _>>()?;

            hir::Ty::Record(fields)
        }
    })
}

fn resolve_ty(
    hir: &hir::Program,
    ctx: &mut impl GenericContext,
    mut module: hir::ModuleId,
    path: ast::Path,
) -> Result<hir::Ty, Diagnostic> {
    let mut spec = Vec::new();

    for ty in path.spec.iter().flatten() {
        spec.push(lower_ty_inner(hir, ctx, module, ty)?);
    }

    for segment in &path.segments[..path.segments.len() - 1] {
        let ast::PathSegment::Item(item) = segment;

        if let Some(next) = hir.modules[module].modules.get(item.name) {
            module = *next;
            continue;
        }

        let diagnostic = Diagnostic::error("unresolved::type")
            .message(format!("module does not contain item `{}`", item.name));

        return Err(diagnostic);
    }

    let ast::PathSegment::Item(item) = path.segments.last().unwrap();

    match hir.modules[module].items.get(item.name) {
        Some(hir::Item::Type(named_id)) => Ok(hir::Ty::Named(*named_id, spec)),

        Some(hir::Item::Alias(ty)) => Ok(ty.clone()),

        _ => {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message(format!("module does not contain type `{}`", item.name));

            Err(diagnostic)
        }
    }
}
