use std::collections::BTreeMap;

use crate::{ast, diagnostic::Diagnostic, hir2 as hir, solve};

use super::{Generics, Lowerer};

impl Lowerer<'_> {
    pub(super) fn lower_ty(
        &mut self,
        module: hir::ModuleId,
        generics: &mut Generics<'_>,
        allow_wild: bool,
        ty: &ast::Ty,
    ) -> Result<solve::Ty, ()> {
        Ok(match ty {
            ast::Ty::Wild(span) => {
                if !allow_wild {
                    let diagnostic = Diagnostic::error("wild::type")
                        .message("wildcard type not allowed here")
                        .label(*span, "here");

                    self.reporter.emit(diagnostic);

                    return Err(());
                }

                solve::Ty::var(self.program.solver.fresh_var())
            }

            ast::Ty::Int(_) => solve::Ty::int(),
            ast::Ty::Float(_) => solve::Ty::float(),
            ast::Ty::Str(_) => solve::Ty::str(),
            ast::Ty::True(_) => solve::Ty::true_(),
            ast::Ty::False(_) => solve::Ty::false_(),
            ast::Ty::None(_) => solve::Ty::none(),
            ast::Ty::Never(_) => solve::Ty::never(),

            ast::Ty::Generic(generic) => match generics {
                Generics::Explicit(generics) => {
                    match generics.iter().find(|(name, _)| name == &generic.name) {
                        Some((_, ty)) => solve::Ty::var(*ty),
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
                        Some((_, ty)) => solve::Ty::var(*ty),
                        None => {
                            let var = self.program.solver.fresh_var();
                            generics.push((generic.name, var));
                            solve::Ty::var(var)
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
                        .map(|_| solve::Ty::var(self.program.solver.fresh_var()))
                        .collect::<Vec<_>>();

                    return Ok(solve::Ty::app(tag, args));
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

                solve::Ty::app(tag, args)
            }

            ast::Ty::Union { tys, .. } => {
                let mut tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                let first = tys.remove(0);
                tys.into_iter().fold(first, solve::Ty::union_with)
            }

            ast::Ty::Inter { tys, .. } => {
                let mut tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                let first = tys.remove(0);
                tys.into_iter().fold(first, solve::Ty::inter_with)
            }

            ast::Ty::Neg { ty, .. } => self.lower_ty(module, generics, allow_wild, ty)?.neg(),

            ast::Ty::Ref { ty, .. } => {
                solve::Ty::ref_(self.lower_ty(module, generics, allow_wild, ty)?)
            }

            ast::Ty::List { ty, .. } => {
                solve::Ty::array(self.lower_ty(module, generics, allow_wild, ty)?)
            }

            ast::Ty::Func { input, output, .. } => {
                let input = self.lower_ty(module, generics, allow_wild, input)?;
                let output = self.lower_ty(module, generics, allow_wild, output)?;

                solve::Ty::func(input, output)
            }

            ast::Ty::Tuple { tys, .. } => {
                let tys = tys
                    .iter()
                    .map(|ty| self.lower_ty(module, generics, allow_wild, ty))
                    .collect::<Result<Vec<_>, _>>()?;

                solve::Ty::tuple(tys)
            }

            ast::Ty::Record { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let ty = self.lower_ty(module, generics, allow_wild, &field.ty)?;

                        Ok((field.name, ty))
                    })
                    .collect::<Result<BTreeMap<_, _>, _>>()?;

                solve::Ty::record(fields)
            }
        })
    }
}
