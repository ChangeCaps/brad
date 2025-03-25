use std::sync::atomic::{AtomicUsize, Ordering};

use crate::diagnostic::Diagnostic;

use super::Ty;

#[derive(Clone, Debug, Default)]
pub struct Generics {
    pub params: Vec<Param>,
}

impl Generics {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: &'static str,
    pub generic: Generic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Generic {
    pub(crate) index: usize,
}

impl Generic {
    pub fn new() -> Self {
        static INDEX: AtomicUsize = AtomicUsize::new(0);
        let index = INDEX.fetch_add(1, Ordering::SeqCst);
        Self { index }
    }
}

#[derive(Clone, Debug)]
pub struct Spec {
    tys: Vec<(Generic, Ty)>,
}

impl Spec {
    pub fn new(generics: &Generics, tys: &[Ty]) -> Result<Self, Diagnostic> {
        if generics.params.len() != tys.len() {
            let diagnostic = Diagnostic::error("unresolved::generic");

            return Err(diagnostic);
        }

        let mut spec = Vec::new();

        for (param, ty) in generics.params.iter().zip(tys) {
            spec.push((param.generic, ty.clone()));
        }

        Ok(Self { tys: spec })
    }

    pub fn get(&self, generic: Generic) -> Option<&Ty> {
        for (g, ty) in &self.tys {
            if *g == generic {
                return Some(ty);
            }
        }

        None
    }

    pub fn apply(&self, ty: Ty) -> Result<Ty, Diagnostic> {
        Ok(match ty {
            Ty::Int | Ty::Float | Ty::Str | Ty::True | Ty::False | Ty::None | Ty::Never => ty,

            Ty::Generic(generic) => match self.get(generic) {
                Some(ty) => ty.clone(),
                None => {
                    let diagnostic = Diagnostic::error("unresolved::generic");

                    return Err(diagnostic);
                }
            },

            Ty::Named(named_id, mut tys) => {
                for ty in &mut tys {
                    *ty = self.apply(ty.clone())?;
                }

                Ty::Named(named_id, tys)
            }

            Ty::Ref(mut ty) => {
                *ty = self.apply(*ty)?;
                Ty::Ref(ty)
            }

            Ty::List(mut ty) => {
                *ty = self.apply(*ty)?;
                Ty::List(ty)
            }

            Ty::Func(mut input, mut output) => {
                *input = self.apply(*input)?;
                *output = self.apply(*output)?;
                Ty::Func(input, output)
            }

            Ty::Tuple(mut tys) => {
                for ty in &mut tys {
                    *ty = self.apply(ty.clone())?;
                }

                Ty::Tuple(tys)
            }

            Ty::Union(mut tys) => {
                tys = tys
                    .into_iter()
                    .map(|ty| self.apply(ty))
                    .collect::<Result<_, _>>()?;

                Ty::Union(tys)
            }

            Ty::Record(mut fields) => {
                for field in &mut fields {
                    field.ty = self.apply(field.ty.clone())?;
                }

                Ty::Record(fields)
            }
        })
    }
}
