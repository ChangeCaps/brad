mod format;
mod simplify;
mod ty;

use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::BuildHasherDefault,
};

pub use ty::*;

use crate::diagnostic::{Diagnostic, Reporter, Span};

type SeaHashSet<T> = HashSet<T, BuildHasherDefault<seahash::SeaHasher>>;
type SeaHashMap<K, V> = HashMap<K, V, BuildHasherDefault<seahash::SeaHasher>>;

#[derive(Clone, Debug, Default)]
pub struct Options {}

#[derive(Debug)]
pub struct Solver {
    options: Options,
    bounds: HashMap<Var, Bounds>,
    cache: SeaHashMap<(Ty, Ty), Option<bool>>,
    applicables: HashMap<Tag, (Ty, Vec<Var>)>,
    errors: Vec<Diagnostic>,
}

#[derive(Clone, Debug)]
pub struct Bounds {
    pub lower: Ty,
    pub upper: Ty,
}

impl Default for Bounds {
    fn default() -> Self {
        Self {
            lower: Ty::never(),
            upper: Ty::always(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    index: usize,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.index)
    }
}

impl Default for Solver {
    fn default() -> Self {
        Self::new(Options::default())
    }
}

impl Solver {
    pub fn new(options: Options) -> Self {
        Self {
            options,
            bounds: HashMap::new(),
            cache: HashMap::default(),
            applicables: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn fresh_var(&mut self) -> Var {
        let var = Var {
            index: self.bounds.len(),
        };

        self.bounds.insert(var, Bounds::default());

        var
    }

    pub fn add_applicable(&mut self, tag: Tag, body: Ty, args: Vec<Var>) {
        self.applicables.insert(tag, (body, args));
    }

    pub fn instantiate(&mut self, mut ty: Ty) -> Ty {
        let mut subst = HashMap::new();
        self.instantiate_impl(&mut ty, &mut subst);

        ty
    }

    fn instantiate_impl(&mut self, Ty(ty): &mut Ty, subst: &mut HashMap<Var, Var>) {
        for conj in ty.iter_mut() {
            conj.pos = self.instantiate_term(conj.pos.clone(), subst);
            conj.neg = self.instantiate_term(conj.neg.clone(), subst);
        }
    }

    fn instantiate_term(&mut self, term: Term, subst: &mut HashMap<Var, Var>) -> Term {
        let mut new_term = Term::extreme();

        for vars in term.vars {
            new_term.vars.insert(self.instantiate_var(vars, subst));
        }

        for mut app in term.apps {
            for arg in app.args.iter_mut() {
                self.instantiate_impl(arg, subst);
            }

            new_term.apps.insert(app);
        }

        new_term.tags = term.tags;
        new_term.base = term.base;

        if let Some(ref mut base) = new_term.base {
            match base {
                Base::Record(fields) => {
                    for ty in fields.values_mut() {
                        self.instantiate_impl(ty, subst);
                    }
                }

                Base::Tuple(items) => {
                    for item in items {
                        self.instantiate_impl(item, subst);
                    }
                }

                Base::Array(ty) => {
                    self.instantiate_impl(ty, subst);
                }

                Base::Func(i, o) => {
                    self.instantiate_impl(i, subst);
                    self.instantiate_impl(o, subst);
                }

                Base::Ref(ty) => {
                    self.instantiate_impl(ty, subst);
                }
            }
        }

        new_term
    }

    fn instantiate_var(&mut self, var: Var, subst: &mut HashMap<Var, Var>) -> Var {
        if let Some(&var) = subst.get(&var) {
            return var;
        }

        let fresh = self.fresh_var();
        subst.insert(var, fresh);

        let mut bounds = self.bounds[&var].clone();
        self.instantiate_impl(&mut bounds.lower, subst);
        self.instantiate_impl(&mut bounds.upper, subst);

        self.bounds.insert(fresh, bounds);

        fresh
    }

    fn expand(&self, app: App) -> Ty {
        let (mut body, args) = self.applicables[&app.tag].clone();

        assert_eq!(args.len(), app.args.len());

        let subst = args.into_iter().zip(app.args).collect();
        Self::expand_impl(&mut body, &subst);

        body
    }

    fn expand_impl(ty: &mut Ty, subst: &HashMap<Var, Ty>) {
        let mut added = Vec::new();

        for conj in ty.0.iter_mut() {
            for (var, ty) in subst {
                if conj.pos.vars.remove(var) {
                    let mut ty = ty.clone();

                    ty.inter(Ty(vec![Conj {
                        pos: conj.pos.clone(),
                        neg: conj.neg.clone(),
                    }]));

                    added.push(ty);
                }
            }

            for (var, ty) in subst {
                if conj.neg.vars.remove(var) {
                    let mut ty = ty.clone();

                    ty.inter(Ty(vec![Conj {
                        pos: conj.pos.clone(),
                        neg: conj.neg.clone(),
                    }]));

                    added.push(ty);
                }
            }
        }

        for added in added {
            ty.union(added);
        }
    }

    pub fn subty(&mut self, mut lhs: Ty, mut rhs: Ty, span: Span) {
        lhs.simplify();
        rhs.simplify();

        let key = (lhs, rhs);

        if self.cache.get(&key).is_some_and(|&r| r != Some(false)) {
            return;
        }

        self.cache.insert(key.clone(), None);

        let (lhs, rhs) = key.clone();

        if let Err(diagnostic) = self.constrain(lhs, rhs, span) {
            self.errors.push(diagnostic);
        }

        self.cache.insert(key, Some(true));
    }

    fn constrain(&mut self, lhs: Ty, rhs: Ty, span: Span) -> Result<(), Diagnostic> {
        for conj in lhs.inter_with(rhs.neg()) {
            self.constrain_term(conj.pos, conj.neg, span)?;
        }

        Ok(())
    }

    fn constrain_term(
        &mut self,
        mut lhs: Term,
        mut rhs: Term,
        span: Span,
    ) -> Result<(), Diagnostic> {
        if let Some(var) = lhs.vars.pop_first() {
            let mut bound = Ty::term_neg(rhs).neg();
            bound.union(Ty::term_pos(lhs).neg());

            let bounds = self.bounds.get_mut(&var).unwrap();
            bounds.upper.inter(bound.clone());

            let lower = bounds.lower.clone();
            self.subty(lower, bound, span);

            return Ok(());
        }

        if let Some(var) = rhs.vars.pop_first() {
            let mut bound = Ty::term_pos(lhs);
            bound.inter(Ty::term_neg(rhs));

            let bounds = self.bounds.get_mut(&var).unwrap();
            bounds.lower.union(bound.clone());

            let upper = bounds.upper.clone();
            self.subty(bound, upper, span);

            return Ok(());
        }

        if let Some(app) = lhs.apps.pop_first() {
            let mut rhs = Ty::term_neg(rhs).neg();
            rhs.union(Ty::term_pos(lhs).neg());

            let body = self.expand(app);
            self.subty(body, rhs, span);

            return Ok(());
        }

        if let Some(app) = rhs.apps.pop_first() {
            let mut lhs = Ty::term_pos(lhs);
            lhs.inter(Ty::term_neg(rhs));

            let body = self.expand(app);
            self.subty(lhs, body, span);

            return Ok(());
        }

        if rhs.tags.iter().any(|tag| lhs.tags.contains(tag)) {
            return Ok(());
        }

        match (&lhs.base, &rhs.base) {
            (Some(Base::Record(lfields)), Some(Base::Record(rfields))) => {
                for (field, ty) in rfields {
                    let Some(lhs_ty) = lfields.get(field) else {
                        let diagnostic = Diagnostic::error("type::error")
                            .message(format!(
                                "unsatisfiable constraint {} <: {}",
                                lhs.display(true),
                                rhs.display(false),
                            ))
                            .label(span, "originating from here");

                        return Err(diagnostic);
                    };

                    self.subty(lhs_ty.clone(), ty.clone(), span);
                }
            }

            (Some(Base::Tuple(litems)), Some(Base::Tuple(ritems))) => {
                if litems.len() != ritems.len() {
                    let diagnostic = Diagnostic::error("type::error")
                        .message(format!(
                            "unsatisfiable constraint {} <: {}",
                            lhs.display(true),
                            rhs.display(false),
                        ))
                        .label(span, "originating from here");

                    return Err(diagnostic);
                }

                for (lty, rty) in litems.iter().zip(ritems.iter()) {
                    self.subty(lty.clone(), rty.clone(), span);
                }
            }

            (Some(Base::Array(lhs)), Some(Base::Array(rhs))) => {
                self.subty(lhs.clone(), rhs.clone(), span);
            }

            (Some(Base::Func(li, lo)), Some(Base::Func(ri, ro))) => {
                self.subty(ri.clone(), li.clone(), span);
                self.subty(lo.clone(), ro.clone(), span);
            }

            (Some(Base::Ref(lhs)), Some(Base::Ref(rhs))) => {
                // references aren't covariant
                self.subty(lhs.clone(), rhs.clone(), span);
                self.subty(rhs.clone(), lhs.clone(), span);
            }

            (_, _) => {
                let diagnostic = Diagnostic::error("type::error")
                    .message(format!(
                        "unsatisfiable constraint {} <: {}",
                        lhs.display(true),
                        rhs.display(false),
                    ))
                    .label(span, "originating from here");

                return Err(diagnostic);
            }
        }

        Ok(())
    }

    pub fn finish(&mut self, reporter: &mut dyn Reporter) -> Result<(), ()> {
        if !self.errors.is_empty() {
            for error in self.errors.iter() {
                reporter.emit(error.clone());
            }

            return Err(());
        }

        Ok(())
    }
}
