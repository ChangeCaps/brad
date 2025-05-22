use diagnostic::Diagnostic;

use crate::{ast, hir2 as hir};

use super::{Generics, Lowerer};

impl Lowerer<'_> {
    pub(super) fn lower_ty(
        &mut self,
        module: hir::ModuleId,
        generics: &mut Generics<'_>,
        allow_wild: bool,
        ty: &ast::Ty,
    ) -> Result<solve::Type, ()> {
        Ok(match ty {
            ast::Ty::Wild(span) => {
                if !allow_wild {
                    let diagnostic = Diagnostic::error("wild::type")
                        .message("wildcard type not allowed here")
                        .label(*span, "here");

                    self.reporter.emit(diagnostic);

                    return Err(());
                }

                solve::Type::fresh_var()
            }

            ast::Ty::Int(_) => solve::Type::int(),
            ast::Ty::Float(_) => solve::Type::float(),
            ast::Ty::Str(_) => solve::Type::str(),
            ast::Ty::True(_) => solve::Type::true_(),
            ast::Ty::False(_) => solve::Type::false_(),
            ast::Ty::None(_) => solve::Type::none(),
            ast::Ty::Never(_) => solve::Type::bottom(),

            ast::Ty::Generic(generic) => match generics {
                Generics::Explicit(generics) => {
                    match generics.iter().find(|(name, _)| name == &generic.name) {
                        Some((_, ty)) => solve::Type::var(*ty),
                        None => {
                            let diagnostic = Diagnostic::error("unbound::generic")
                                .message(format!("unbound generic `{}`", generic.name))
                                .label(generic.span, "here");

                            self.reporter.emit(diagnostic);

                            return Err(());
                        }
                    }
                }

                Generics::Implicit(generics) => {
                    match generics.iter().find(|(name, _)| name == &generic.name) {
                        Some((_, ty)) => solve::Type::var(*ty),
                        None => {
                            let var = solve::Var::fresh();
                            generics.push((generic.name, var));
                            solve::Type::var(var)
                        }
                    }
                }
            },

            ast::Ty::Path(path) => {
                let segments = path
                    .segments
                    .iter()
                    .map(|segment| segment.name)
                    .collect::<Vec<_>>();

                let Some(tag) = self.program.modules.get_type(module, &segments) else {
                    let diagnostic = Diagnostic::error("unbound::type")
                        .message(format!("unbound type `{}`", path))
                        .label(path.span, "here");

                    self.reporter.emit(diagnostic);

                    return Err(());
                };

                let arg_count = match self.types.get(&tag) {
                    Some(info) => info.generics,
                    None => self.aliases[&tag].generics,
                };

                let Some(ref spec) = path.spec else {
                    let args = (0..arg_count)
                        .map(|_| solve::Type::fresh_var())
                        .collect::<Vec<_>>();

                    return Ok(solve::Type::app(solve::App::new(tag, args)));
                };

                let args = spec
                    .tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                if args.len() != arg_count {
                    let diagnostic = Diagnostic::error("type::args")
                        .message(format!(
                            "expected {} type arguments, found {}",
                            arg_count,
                            args.len()
                        ))
                        .label(spec.span, "here");

                    self.reporter.emit(diagnostic);

                    return Err(());
                }

                solve::Type::app(solve::App::new(tag, args))
            }

            ast::Ty::Union { tys, .. } => {
                let mut tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                let first = tys.remove(0);
                tys.into_iter().fold(first, solve::Type::union)
            }

            ast::Ty::Inter { tys, .. } => {
                let mut tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                let first = tys.remove(0);
                tys.into_iter().fold(first, solve::Type::inter)
            }

            ast::Ty::Neg { ty, .. } => self.lower_ty(module, generics, allow_wild, ty)?.neg(),

            ast::Ty::Ref { ty, .. } => {
                todo!()
            }

            ast::Ty::List { ty, .. } => {
                solve::Type::array(self.lower_ty(module, generics, allow_wild, ty)?)
            }

            ast::Ty::Func { input, output, .. } => {
                let input = self.lower_ty(module, generics, allow_wild, input)?;
                let output = self.lower_ty(module, generics, allow_wild, output)?;

                solve::Type::function(input, output)
            }

            ast::Ty::Tuple { tys, .. } => {
                let tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                solve::Type::tuple(tys)
            }

            ast::Ty::Record { fields, .. } => {
                let mut lowered_fields = Vec::new();

                for field in fields {
                    let ty = self.lower_ty(module, generics, allow_wild, &field.ty)?;

                    if lowered_fields.iter().any(|(name, _)| *name == field.name) {
                        let diagnostic = Diagnostic::error("duplicate::field")
                            .message(format!("duplicate field `{}`", field.name))
                            .label(field.span, "here");

                        self.reporter.emit(diagnostic);

                        return Err(());
                    }

                    lowered_fields.push((field.name, ty));
                }

                solve::Type::record(lowered_fields)
            }
        })
    }
}
