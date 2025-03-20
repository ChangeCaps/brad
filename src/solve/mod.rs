use std::{collections::HashMap, fmt};

use crate::diagnostic::{Diagnostic, Reporter, Span};

pub use dnf::*;
pub use ty::*;

mod dnf;
mod format;
mod simplify;
mod ty;

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Union(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
            Ty::Inter(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            Ty::Neg(ty) => write!(f, "~{}", ty),
            Ty::Func(lhs, rhs) => write!(f, "({} -> {})", lhs, rhs),
            Ty::List(ty) => write!(f, "[{}]", ty),
            Ty::Ref(ty) => write!(f, "ref {}", ty),

            Ty::Top => write!(f, "⊤"),
            Ty::Bot => write!(f, "⊥"),
            Ty::Var(idx) => write!(f, "'{}", idx.index),

            Ty::Tag(tag) => write!(f, "{}", tag.name),

            Ty::Record(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|(ident, ty)| format!("{}: {}", ident, ty))
                    .collect();

                write!(f, "{{{}}}", fields.join(", "))
            }

            Ty::Tuple(tys) => {
                let tys: Vec<_> = tys.iter().map(|ty| ty.to_string()).collect();

                write!(f, "({})", tys.join(", "))
            }

            Ty::App(app) => {
                let args: Vec<_> = app.args.iter().map(|ty| ty.to_string()).collect();

                write!(f, "{}<{}>", app.tag.name, args.join(", "))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Options {
    pub simplify_normal_forms: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            simplify_normal_forms: true,
        }
    }
}

#[derive(Debug)]
pub struct Solver {
    options: Options,
    bounds: HashMap<Var, Bounds>,
    applicables: HashMap<Tag, (Ty, Vec<Ty>)>,
    cache: HashMap<(Ty, Ty), bool>,
    diagnostics: Vec<Diagnostic>,
}

impl Default for Solver {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    index: usize,
}

/// Constraints for a type variable.
#[derive(Clone, Debug)]
pub struct Bounds {
    /// Lower bounds.
    pub lbs: Vec<Ty>,

    /// Upper bounds.
    pub ubs: Vec<Ty>,
}

impl Solver {
    pub fn new(options: Options) -> Self {
        Self {
            options,
            bounds: HashMap::new(),
            applicables: HashMap::new(),
            cache: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn fresh_var(&mut self) -> Var {
        let index = self.bounds.len();
        let var = Var { index };

        self.bounds.insert(
            var,
            Bounds {
                lbs: Vec::new(),
                ubs: Vec::new(),
            },
        );

        var
    }

    /// Add a type body and arguments of an applicable type.
    pub fn add_applicable(&mut self, tag: Tag, body: Ty, args: Vec<Ty>) {
        self.applicables.insert(tag, (body, args));
    }

    /// Get the arguments of an applicable type.
    pub fn applicable_args(&self, tag: Tag) -> Option<&Vec<Ty>> {
        self.applicables.get(&tag).map(|(_, args)| args)
    }

    pub fn bounds(&mut self, var: Var) -> &mut Bounds {
        self.bounds.get_mut(&var).unwrap()
    }

    pub fn finish(&mut self, reporter: &mut dyn Reporter) -> Result<(), ()> {
        if !self.diagnostics.is_empty() {
            for diagnostic in self.diagnostics.drain(..) {
                reporter.emit(diagnostic);
            }

            return Err(());
        }

        Ok(())
    }

    pub fn instance(&mut self, ty: &Ty) -> Ty {
        self.inst(&mut HashMap::new(), ty)
    }

    fn inst(&mut self, map: &mut HashMap<Var, Var>, ty: &Ty) -> Ty {
        ty.clone().map(|ty| {
            if let Ty::Var(var) = ty {
                if let Some(&var) = map.get(&var) {
                    return Ty::Var(var);
                }

                let new_var = self.fresh_var();
                map.insert(var, new_var);

                let var = self.bounds(var).clone();

                let lbs = var.lbs.iter().map(|ty| self.inst(map, ty)).collect();
                let ubs = var.ubs.iter().map(|ty| self.inst(map, ty)).collect();

                let var = self.bounds(new_var);

                var.lbs = lbs;
                var.ubs = ubs;

                Ty::Var(new_var)
            } else {
                ty
            }
        })
    }

    pub fn subty(&mut self, lhs: &Ty, rhs: &Ty, span: Span) {
        let key = (lhs.clone(), rhs.clone());

        if self.cache.contains_key(&key) {
            return;
        }

        self.cache.insert(key.clone(), false);

        if let Err(err) = self.constrain(lhs, rhs, span) {
            self.diagnostics.push(err);
            self.cache.remove(&key);
        }

        self.cache.insert(key.clone(), true);
    }

    fn expand(&self, app: &App) -> Ty {
        let (body, args) = self.applicables.get(&app.tag).unwrap();

        if args.len() != app.args.len() {
            panic!(
                "expected {} arguments, found {}",
                args.len(),
                app.args.len()
            );
        }

        let mut map = HashMap::new();

        for (arg, ty) in args.iter().zip(app.args.iter()) {
            map.insert(arg.clone(), ty.clone());
        }

        body.subst(&map)
    }

    fn constrain(&mut self, lhs: &Ty, rhs: &Ty, span: Span) -> Result<(), Diagnostic> {
        let lhs = self.simplify_dnf(self.dnf(lhs));
        let rhs = self.simplify_cnf(self.cnf(rhs));

        // (a & ~b) | (c & ~d) | .. <: (~e | f) & (~g | h) & ..
        //
        // (a & ~b) <: (~e | f) & (~g | h) & ..
        // (c & ~d) <: (~e | f) & (~g | h) & ..
        // ..
        //
        // a & ~b <: ~e | f
        // a & ~b <: ~g | h
        // c & ~d <: ~e | f
        // c & ~d <: ~g | h
        // ..
        //
        // a | e <: b & f
        // a | g <: b & h
        // c | e <: d & f
        // c | g <: d & h
        // ..

        for l in lhs.0.clone() {
            'out: for r in rhs.0.clone() {
                let Conjunct {
                    mut lnf,
                    mut vars,
                    mut rnf,
                    mut nvars,
                } = self.inter_conjunct(l.clone(), r.clone().neg());

                // lnf & vars <: rnf & nvars

                if let Some(&var) = vars.iter().next() {
                    vars.remove(&var);

                    let dis = Conjunct {
                        lnf,
                        vars,
                        rnf,
                        nvars,
                    }
                    .neg()
                    .to_ty();

                    let bounds = self.bounds(var);
                    bounds.ubs.push(dis.clone());

                    let bounds = self.bounds(var);

                    for lb in bounds.lbs.clone() {
                        self.subty(&lb, &dis, span);
                    }

                    continue;
                }

                if let Some(&var) = nvars.iter().next() {
                    nvars.remove(&var);

                    let dis = Conjunct {
                        lnf,
                        vars,
                        rnf,
                        nvars,
                    }
                    .to_ty();

                    let bounds = self.bounds(var);
                    bounds.lbs.push(dis.clone());

                    let var = self.bounds(var);

                    for ub in var.ubs.clone() {
                        self.subty(&dis, &ub, span);
                    }

                    continue;
                }

                if lnf.is_bot() || rnf.is_top() {
                    continue;
                }

                if lnf.is_top() || rnf.is_bot() {
                    let diagnostic = Diagnostic::error("type::error")
                        .message(format!(
                            "type constraint `{} <: {}` is unsatisfiable",
                            self.format_ty(&lnf.to_ty()),
                            self.format_ty(&rnf.to_ty()),
                        ))
                        .note(format!(
                            "required for `{} <: {}` to hold",
                            self.format_ty(&l.to_ty()),
                            self.format_ty(&r.to_ty()),
                        ))
                        .note(format!(
                            "required for `{} <: {}` to hold",
                            self.format_ty(&lhs.to_ty()),
                            self.format_ty(&rhs.to_ty()),
                        ))
                        .label(span, "originated here");

                    return Err(diagnostic);
                }

                let Lnf::Base {
                    tags: ref lt,
                    apps: ref mut la,
                    base: ref lb,
                } = lnf
                else {
                    unreachable!("expected base, found {:?}", lnf);
                };

                let Rnf::Base {
                    tags: ref rt,
                    apps: ref mut ra,
                    base: ref rb,
                } = rnf
                else {
                    unreachable!("expected base, found {:?}", rnf);
                };

                if let Some(app) = la.first().cloned() {
                    la.remove(&app);

                    // this should not need to be here, but for whatever reason it does
                    // frankly, it makes me want to puke
                    //  - @hjalte 2025-03-16
                    if lnf.is_bot() {
                        lnf = Lnf::Top;
                    }

                    let rhs = Ty::union(Ty::neg(lnf.to_ty()), rnf.to_ty());
                    let lhs = self.expand(&app);

                    self.subty(&lhs, &rhs, span);

                    continue;
                }

                if let Some(app) = ra.first().cloned() {
                    ra.remove(&app);

                    // this should not need to be here, but for whatever reason it does
                    // frankly, it makes me want to puke
                    //  - @hjalte 2025-03-16
                    if rnf.is_top() {
                        rnf = Rnf::Bot;
                    }

                    let lhs = Ty::inter(lnf.to_ty(), Ty::neg(rnf.to_ty()));
                    let rhs = self.expand(&app);

                    self.subty(&lhs, &rhs, span);

                    continue;
                }

                for rn in rt {
                    if lt.contains(rn) {
                        continue 'out;
                    }
                }

                match (lb, rb) {
                    (LnfBase::Func(li, lo), RnfBase::Func(ri, ro)) => {
                        self.subty(ri, li, span);
                        self.subty(lo, ro, span);

                        continue;
                    }

                    (LnfBase::Record(lf), RnfBase::Field(rf, rt)) => match lf.get(rf) {
                        Some(lt) => {
                            self.subty(lt, rt, span);
                            continue;
                        }

                        None => {
                            let fields = lf.keys().map(|f| format!("`{}`", f)).collect::<Vec<_>>();

                            let diagnostic = Diagnostic::error("type::error")
                                .message(format!("expected field `{}` in record", rf))
                                .note(format!("available fields are: {}", fields.join(", ")))
                                .note(format!(
                                    "required for `{} <: {}` to hold",
                                    self.format_ty(&lnf.to_ty()),
                                    self.format_ty(&rnf.to_ty()),
                                ))
                                .note(format!(
                                    "required for `{} <: {}` to hold",
                                    self.format_ty(&lhs.to_ty()),
                                    self.format_ty(&rhs.to_ty()),
                                ))
                                .label(span, "originated here");

                            return Err(diagnostic);
                        }
                    },

                    (LnfBase::Tuple(lt), RnfBase::Tuple(rt)) if lt.len() == rt.len() => {
                        for (l, r) in lt.iter().zip(rt.iter()) {
                            self.subty(l, r, span);
                        }

                        continue;
                    }

                    (LnfBase::Tuple(lt), RnfBase::Tuple(rt)) => {
                        let diagnostic = Diagnostic::error("type::error")
                            .message(format!(
                                "expected tuple of length `{}`, found tuple of length `{}`",
                                lt.len(),
                                rt.len()
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lnf.to_ty()),
                                self.format_ty(&rnf.to_ty()),
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lhs.to_ty()),
                                self.format_ty(&rhs.to_ty()),
                            ))
                            .label(span, "originated here");

                        return Err(diagnostic);
                    }

                    (LnfBase::List(lt), RnfBase::List(rt)) => {
                        self.subty(lt, rt, span);
                        continue;
                    }

                    (LnfBase::Ref(lt), RnfBase::Ref(rt)) => {
                        self.subty(lt, rt, span);
                        continue;
                    }

                    (lb, rb) => {
                        let lkind = match lb {
                            LnfBase::None => self.format_ty(&lnf.to_ty()),
                            LnfBase::Record(_) => String::from("record"),
                            LnfBase::Tuple(_) => String::from("tuple"),
                            LnfBase::Func(_, _) => String::from("function"),
                            LnfBase::List(_) => String::from("list"),
                            LnfBase::Ref(_) => String::from("reference"),
                        };

                        let rkind = match rb {
                            RnfBase::None => self.format_ty(&rnf.to_ty()),
                            RnfBase::Field(_, _) => String::from("record"),
                            RnfBase::Func(_, _) => String::from("function"),
                            RnfBase::Tuple(_) => String::from("tuple"),
                            RnfBase::List(_) => String::from("list"),
                            RnfBase::Ref(_) => String::from("reference"),
                        };

                        let diagnostic = Diagnostic::error("type::error")
                            .message(format!("`{}` does not subtype `{}`", lkind, rkind))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lnf.to_ty()),
                                self.format_ty(&rnf.to_ty()),
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&l.to_ty()),
                                self.format_ty(&r.to_ty()),
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lhs.to_ty()),
                                self.format_ty(&rhs.to_ty()),
                            ))
                            .label(span, "originated here");

                        return Err(diagnostic);
                    }
                }
            }
        }

        Ok(())
    }
}
