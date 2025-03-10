use std::collections::{hash_map::Entry, HashMap};

use expr::BodyLowerer;

use crate::{ast, diagnostic::Diagnostic, hir};

mod expr;

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

        for segment in path {
            match self.program[current].modules.entry(segment) {
                Entry::Occupied(entry) => current = *entry.get(),
                Entry::Vacant(_) => {
                    let module = hir::Module::new().with_name(segment).with_parent(current);

                    let new = self.program.modules.insert(module);
                    self.program[current].modules.insert(segment, new);
                    current = new;
                }
            }
        }

        self.modules.push((current, ast));
    }

    pub fn finish(mut self) -> Result<hir::Program, Diagnostic> {
        self.import_packages()?;
        self.resolve_imports()?;
        self.lower_aliases()?;
        self.lower_types()?;
        self.prepare_functions()?;
        self.lower_functions()?;

        Ok(self.program)
    }

    fn import_packages(&mut self) -> Result<(), Diagnostic> {
        for id in self.program.modules.ids() {
            let packages: Vec<_> = self.program[self.root]
                .modules
                .iter()
                .map(|(name, id)| (*name, *id))
                .collect();

            self.program[id].modules.extend(packages);
        }

        Ok(())
    }

    fn format_module_path(&self, module: hir::ModuleId) -> String {
        let module = &self.program[module];

        match (module.name, module.parent) {
            (Some(name), Some(parent)) => {
                let parent = self.format_module_path(parent);

                if parent.is_empty() {
                    name.to_string()
                } else {
                    format!("{}::{}", parent, name)
                }
            }

            _ => String::new(),
        }
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
                        if self.program[m].items.contains_key(decl.name) {
                            let diagnostic = Diagnostic::error("unresolved::func")
                                .message(format!("function `{}` is already defined", decl.name))
                                .label(decl.span, "previously defined here");

                            return Err(diagnostic);
                        }

                        let body = hir::Body {
                            attrs: decl.attrs.clone(),
                            name: format!("{}::{}", self.format_module_path(m), decl.name),
                            generics: hir::Generics::new(),
                            locals: hir::Locals::new(),
                            input: Vec::new(),
                            output: hir::Ty::Never,
                            expr: None,
                            is_extern: decl.is_extern,
                            span: decl.span,
                        };

                        let body_id = self.program.bodies.insert(body);
                        let item = hir::Item::Func(body_id);
                        self.program[m].items.insert(decl.name, item);
                    }

                    ast::Decl::Type(decl) => {
                        if self.program[m].items.contains_key(decl.name) {
                            let diagnostic = Diagnostic::error("unresolved::type")
                                .message(format!("type `{}` is already defined", decl.name))
                                .label(decl.span, "previously defined here");

                            return Err(diagnostic);
                        }

                        let named = hir::Named {
                            name: format!("{}::{}", self.format_module_path(m), decl.name),
                            generics: hir::Generics::new(),
                            ty: None,
                        };

                        let named_id = self.program.types.insert_named(named);
                        let item = hir::Item::Type(named_id);
                        self.program[m].items.insert(decl.name, item);
                    }

                    ast::Decl::Alias(decl) => {
                        if self.program[m].items.contains_key(decl.name) {
                            let diagnostic = Diagnostic::error("unresolved::alias")
                                .message(format!("alias `{}` is already defined", decl.name))
                                .label(decl.span, "previously defined here");

                            return Err(diagnostic);
                        }

                        let alias = hir::Alias {
                            generics: hir::Generics::new(),
                            ty: hir::Ty::None,
                        };

                        let alias_id = self.program.types.insert_alias(alias);
                        let item = hir::Item::Alias(alias_id);
                        self.program[m].items.insert(decl.name, item);
                    }

                    ast::Decl::Import(import) => {
                        let item = import.path.segments.last().unwrap();
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
                    .message(format!("path `{}` does not resolve to a item", path))
                    .label(path.span, "here");

                return Err(diagnostic);
            }

            if let Some(found) = found_module {
                self.program[module].modules.insert(name, found);
            }

            if let Some(found) = found_item {
                self.program[module].items.insert(name, found);
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

                let item = self.program[m].items.get(decl.name);
                let alias_id = match item {
                    Some(hir::Item::Alias(alias_id)) => *alias_id,
                    _ => panic!("expected alias body"),
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

                self.program[alias_id].generics = generics;
                self.program[alias_id].ty = ty;
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

                let item = self.program[m].items.get(decl.name);
                let named_id = match item {
                    Some(hir::Item::Type(named_id)) => *named_id,
                    _ => panic!("expected type body"),
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

                self.program[named_id].ty = Some(ty);
                self.program[named_id].generics = generics;
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

                Self::prepare_function(&mut self.program, m, decl)?;
            }
        }

        Ok(())
    }

    fn prepare_function(
        program: &mut hir::Program,
        m: hir::ModuleId,
        decl: &ast::Func,
    ) -> Result<(), Diagnostic> {
        let item = program[m].items.get(decl.name);
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

            let ty = lower_ty(program, &mut *generic_ctx, m, ty)?;

            let mut lowerer = BodyLowerer::new(program, m, body_id);
            let binding = lowerer.lower_binding(input.binding.clone(), ty.clone())?;
            lowerer.body_mut().input.push(hir::Argument { ty, binding });
        }

        let output = if let Some(ref ty) = decl.output {
            lower_ty(program, &mut *generic_ctx, m, ty)?
        } else {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("missing return type in function")
                .label(decl.span, "found here");

            return Err(diagnostic);
        };

        program[body_id].output = output;
        program[body_id].generics = generics;

        Ok(())
    }

    fn lower_functions(&mut self) -> Result<(), Diagnostic> {
        for &(m, ref ast) in &self.modules {
            for decl in &ast.decls {
                let ast::Decl::Func(decl) = decl else {
                    continue;
                };

                let item = self.program[m].items.get(decl.name);
                let body_id = match item {
                    Some(hir::Item::Func(body_id)) => *body_id,
                    _ => panic!("expected function body"),
                };

                let mut lowerer = BodyLowerer::new(&mut self.program, m, body_id);

                for local in lowerer.body_mut().locals.ids() {
                    lowerer.push_scope(local);
                }

                if let Some(body) = decl.body.clone() {
                    let expr = lowerer.lower_expr(body, Some(lowerer.body().output.clone()))?;

                    if !decl.is_extern && !lowerer.is_subty(&lowerer.body().output, &expr.ty) {
                        let diagnostic = Diagnostic::error("unresolved::type")
                            .message(format!(
                                "expected `{}`, found `{}`",
                                lowerer.format_ty(&lowerer.body().output),
                                lowerer.format_ty(&expr.ty),
                            ))
                            .label(decl.span, "found here");

                        return Err(diagnostic);
                    }

                    lowerer.body_mut().expr = Some(expr);
                }
            }
        }

        Ok(())
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
    for item in &path.segments[..path.segments.len() - 1] {
        if let Some(next) = lowerer.program[module].modules.get(item.name) {
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

    let item = path.segments.last().unwrap();

    let found_item = lowerer.program[module].items.get(item.name);
    let found_module = lowerer.program[module].modules.get(item.name);

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

enum Resolved {
    Type(hir::Ty),
    Func(hir::BodyId, Vec<hir::Ty>),
}

fn resolve_item(
    program: &hir::Program,
    mut ctx: impl GenericContext,
    mut module: hir::ModuleId,
    path: ast::Path,
) -> Result<Resolved, Diagnostic> {
    let mut generics = Vec::new();

    for ty in path.spec.iter().flatten() {
        generics.push(lower_ty_inner(program, &mut ctx, module, ty)?);
    }

    for item in &path.segments[..path.segments.len() - 1] {
        if let Some(next) = program[module].modules.get(item.name) {
            module = *next;
            continue;
        }

        let diagnostic = Diagnostic::error("unresolved::type")
            .message(format!("module does not contain item `{}`", item.name));

        return Err(diagnostic);
    }

    let item = path.segments.last().unwrap();

    match program[module].items.get(item.name) {
        Some(hir::Item::Type(named_id)) => Ok(Resolved::Type(hir::Ty::Named(*named_id, generics))),

        Some(hir::Item::Alias(alias_id)) => {
            if program[*alias_id].generics.params.len() != generics.len() {
                let diagnostic = Diagnostic::error("unresolved::type")
                    .message("expected generic arguments in alias")
                    .label(path.span, "found here");

                return Err(diagnostic);
            }

            let spec = hir::Spec::new(&program[*alias_id].generics, &generics)?;
            let ty = spec.apply(program[*alias_id].ty.clone())?;
            Ok(Resolved::Type(ty))
        }

        Some(hir::Item::Func(body_id)) => {
            if program[*body_id].generics.params.len() != generics.len() {
                let diagnostic = Diagnostic::error("unresolved::type")
                    .message("expected generic arguments in function")
                    .label(path.span, "found here");

                return Err(diagnostic);
            }

            Ok(Resolved::Func(*body_id, generics))
        }

        _ => {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message(format!("module does not contain type `{}`", item.name));

            Err(diagnostic)
        }
    }
}

fn resolve_ty(
    program: &hir::Program,
    ctx: &mut dyn GenericContext,
    module: hir::ModuleId,
    path: ast::Path,
) -> Result<hir::Ty, Diagnostic> {
    match resolve_item(program, ctx, module, path.clone())? {
        Resolved::Type(ty) => Ok(ty),
        Resolved::Func(_, _) => {
            let diagnostic = Diagnostic::error("unresolved::type")
                .message("expected type, found function")
                .label(path.span, "found here");

            Err(diagnostic)
        }
    }
}
