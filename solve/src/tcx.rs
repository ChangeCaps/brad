use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasherDefault,
    mem,
};

use diagnostic::{Diagnostic, Reporter, Span};
use seahash::SeaHasher;

use crate::{App, Bounds, Conjunct, Tag, Term, Type, Var, ty::Base};

type SeaHashMap<K, V> = HashMap<K, V, BuildHasherDefault<SeaHasher>>;
type SeaHashSet<K> = HashSet<K, BuildHasherDefault<SeaHasher>>;

#[derive(Clone, Debug, Default)]
pub struct Tcx {
    applicables: SeaHashMap<Tag, (Type, Vec<Var>)>,
    bounds: SeaHashMap<Var, Bounds>,
    cache: SeaHashSet<Conjunct>,
    errors: Vec<Diagnostic>,
}

impl Tcx {
    pub fn new() -> Self {
        Self::default()
    }

    fn bounds_mut(&mut self, var: Var) -> &mut Bounds {
        self.bounds.entry(var).or_default()
    }

    pub fn add_applicable(&mut self, tag: Tag, body: Type, args: Vec<Var>) {
        self.applicables.insert(tag, (body, args));
    }

    #[allow(clippy::result_unit_err)]
    pub fn finish(&mut self, reporter: &mut dyn Reporter) -> Result<(), ()> {
        if self.errors.is_empty() {
            return Ok(());
        }

        for err in self.errors.drain(..) {
            reporter.emit(err);
        }

        Err(())
    }

    /// Add the constraint `lhs <: rhs` to the type context.
    pub fn subtype(&mut self, lhs: Type, rhs: Type, span: Span) {
        let nf = Self::make_normal_form(lhs, rhs);
        self.subtype_normal_form(nf, span);
    }

    /// Add the constraint `nf <: ⊥` to the type context.
    pub fn subtype_normal_form(&mut self, nf: Type, span: Span) {
        if let Err(err) = self.constrain(nf, span) {
            self.errors.push(err);
        }
    }

    pub fn instantiate(&mut self, mut ty: Type) -> Type {
        let mut subst = SeaHashMap::default();
        self.instantiate_impl(&mut ty, &mut subst);
        ty
    }

    fn instantiate_impl(&mut self, ty: &mut Type, subst: &mut SeaHashMap<Var, Var>) {
        for conjunct in ty.conjuncts_mut() {
            for var in conjunct.vars_mut() {
                if let Some(&sub) = subst.get(var) {
                    *var = sub;
                    continue;
                }

                let new_var = Var::fresh();
                subst.insert(*var, new_var);
                subst.insert(new_var, new_var);

                let mut bounds = self.bounds.get(var).cloned().unwrap_or_default();

                self.instantiate_impl(&mut bounds.lower, subst);
                self.instantiate_impl(&mut bounds.upper, subst);

                self.bounds.insert(new_var, bounds);

                *var = new_var;
            }

            for app in conjunct.apps_mut() {
                for arg in app.args_mut() {
                    self.instantiate_impl(arg, subst);
                }
            }

            for base in conjunct.bases_mut() {
                self.instantiate_base(base, subst);
            }
        }
    }

    fn instantiate_base(&mut self, base: &mut Base, subst: &mut SeaHashMap<Var, Var>) {
        match base {
            Base::None => {}

            Base::Record { fields } => {
                for (_, ty) in fields.iter_mut() {
                    self.instantiate_impl(ty, subst);
                }
            }

            Base::Tuple { fields } => {
                for ty in fields.iter_mut() {
                    self.instantiate_impl(ty, subst);
                }
            }

            Base::Array { element } => {
                self.instantiate_impl(element, subst);
            }

            Base::Function { input, output } => {
                self.instantiate_impl(input, subst);
                self.instantiate_impl(output, subst);
            }
        }
    }

    fn constrain(&mut self, nf: Type, span: Span) -> Result<(), Diagnostic> {
        let mut conjuncts = nf.into_conjuncts();

        while let Some(conjunct) = conjuncts.pop() {
            if self.cache.contains(&conjunct) {
                continue;
            }

            self.cache.insert(conjunct.clone());

            self.constrain_conjunct(&mut conjuncts, conjunct.clone(), span)?;
        }

        Ok(())
    }

    fn constrain_conjunct(
        &mut self,
        conjuncts: &mut Vec<Conjunct>,
        conjunct: Conjunct,
        span: Span,
    ) -> Result<(), Diagnostic> {
        let Some(conjunct) = self.constrain_vars(conjuncts, conjunct)? else {
            return Ok(());
        };

        let Some(conjunct) = self.constrain_apps(conjuncts, conjunct)? else {
            return Ok(());
        };

        let lhs = conjunct.positive;
        let rhs = conjunct.negative;

        if rhs.tags.iter().any(|tag| lhs.tags.contains(tag)) {
            return Ok(());
        }

        self.constrain_bases(conjuncts, lhs, rhs, span)
    }

    #[inline(always)]
    fn constrain_vars(
        &mut self,
        conjuncts: &mut Vec<Conjunct>,
        mut conjunct: Conjunct,
    ) -> Result<Option<Conjunct>, Diagnostic> {
        if let Some(var) = conjunct.positive.vars.pop() {
            if conjunct.negative.vars.binary_search(&var).is_ok() {
                return Ok(None);
            }

            let upper_neg = Type::from(conjunct);

            let bounds = self.bounds_mut(var);
            bounds.upper.inter_mut(upper_neg.clone().neg());

            let nf = bounds.lower.clone().inter(upper_neg);
            conjuncts.extend(nf.into_conjuncts());

            return Ok(None);
        }

        if let Some(var) = conjunct.negative.vars.pop() {
            if conjunct.positive.vars.binary_search(&var).is_ok() {
                return Ok(None);
            }

            let lower = Type::from(conjunct);

            let bounds = self.bounds_mut(var);
            bounds.lower.union_mut(lower.clone());

            let nf = Self::make_normal_form(lower, bounds.upper.clone());
            conjuncts.extend(nf.into_conjuncts());

            return Ok(None);
        }

        Ok(Some(conjunct))
    }

    #[inline(always)]
    fn constrain_apps(
        &mut self,
        conjuncts: &mut Vec<Conjunct>,
        mut conjunct: Conjunct,
    ) -> Result<Option<Conjunct>, Diagnostic> {
        if let Some(app) = conjunct.positive.apps.pop() {
            if conjunct.negative.apps.binary_search(&app).is_ok() {
                return Ok(None);
            }

            let upper = Type::from(conjunct).neg();
            let app = self.expand_application(app);

            let nf = Self::make_normal_form(app, upper);
            conjuncts.extend(nf.into_conjuncts());

            return Ok(None);
        }

        if let Some(app) = conjunct.negative.apps.pop() {
            if conjunct.positive.apps.binary_search(&app).is_ok() {
                return Ok(None);
            }

            let lower = Type::from(conjunct);
            let app = self.expand_application(app);

            let nf = Self::make_normal_form(lower, app);
            conjuncts.extend(nf.into_conjuncts());

            return Ok(None);
        }

        Ok(Some(conjunct))
    }

    #[inline(always)]
    fn constrain_bases(
        &mut self,
        conjuncts: &mut Vec<Conjunct>,
        lhs: Term,
        rhs: Term,
        span: Span,
    ) -> Result<(), Diagnostic> {
        match (lhs.base, rhs.base) {
            (
                ref lhs @ Base::Record {
                    fields: ref lhs_fields,
                },
                Base::Record { fields: rhs_fields },
            ) => {
                for (name, rhs_ty) in rhs_fields {
                    let Some(lhs_ty) = Self::find_field(lhs_fields, name) else {
                        let diagnostic = Diagnostic::error("missing::field")
                            .message(format!("field `{name}` not found in record `{}`", lhs))
                            .span(span);

                        return Err(diagnostic);
                    };

                    let nf = Self::make_normal_form(lhs_ty.clone(), rhs_ty.clone());
                    conjuncts.extend(nf.into_conjuncts());
                }

                Ok(())
            }

            (Base::Tuple { fields: lhs_fields }, Base::Tuple { fields: rhs_fields }) => {
                if lhs_fields.len() != rhs_fields.len() {
                    let diagnostic = Diagnostic::error("unsatisfiable::tuple")
                        .message(format!(
                            "tuples have different lengths: `{}` and `{}`",
                            lhs_fields.len(),
                            rhs_fields.len(),
                        ))
                        .span(span);

                    return Err(diagnostic);
                }

                for (lhs_ty, rhs_ty) in lhs_fields.into_iter().zip(rhs_fields) {
                    let nf = Self::make_normal_form(lhs_ty, rhs_ty);
                    conjuncts.extend(nf.into_conjuncts());
                }

                Ok(())
            }

            (
                Base::Array {
                    element: lhs_element,
                },
                Base::Array {
                    element: rhs_element,
                },
            ) => {
                let nf = Self::make_normal_form(lhs_element, rhs_element);
                conjuncts.extend(nf.into_conjuncts());

                Ok(())
            }

            (
                Base::Function {
                    input: lhs_input,
                    output: lhs_output,
                },
                Base::Function {
                    input: rhs_input,
                    output: rhs_output,
                },
            ) => {
                let input = Self::make_normal_form(rhs_input, lhs_input);
                let output = Self::make_normal_form(lhs_output, rhs_output);

                conjuncts.extend(input.into_conjuncts());
                conjuncts.extend(output.into_conjuncts());

                Ok(())
            }

            (lhs_base, rhs_base) => {
                let lhs = Term {
                    base: lhs_base,
                    ..lhs
                };

                let rhs = Term {
                    base: rhs_base,
                    ..rhs
                };

                let diagnostic = Diagnostic::error("unsatisfiable::base")
                    .message(format!(
                        "`{}` does not subtype `{}`",
                        lhs.display(true),
                        rhs.display(false),
                    ))
                    .span(span);

                Err(diagnostic)
            }
        }
    }

    fn expand_application(&self, app: App) -> Type {
        let (body, args) = &self.applicables[&app.tag()];
        let mut body = body.clone();

        assert_eq!(args.len(), app.args().len());

        let subst: SeaHashMap<_, _> = args.iter().copied().zip(app.args()).collect();
        Self::expand_impl(&mut body, &subst);

        body
    }

    fn expand_impl(ty: &mut Type, subst: &SeaHashMap<Var, &Type>) {
        for mut conjunct in mem::take(ty.conjuncts_mut()) {
            let mut new_type = Type::top();

            conjunct.positive.vars.retain(|var| {
                if let Some(&sub) = subst.get(var) {
                    new_type.inter_mut(sub.clone());
                    false
                } else {
                    true
                }
            });

            conjunct.negative.vars.retain(|var| {
                if let Some(&sub) = subst.get(var) {
                    new_type.inter_mut(sub.clone().neg());
                    false
                } else {
                    true
                }
            });

            for app in conjunct.apps_mut() {
                for arg in app.args_mut() {
                    Self::expand_impl(arg, subst);
                }
            }

            for base in conjunct.bases_mut() {
                Self::expand_base(base, subst);
            }

            ty.union_mut(new_type.inter(conjunct.into()));
        }
    }

    fn expand_base(base: &mut Base, subst: &SeaHashMap<Var, &Type>) {
        match base {
            Base::None => {}

            Base::Record { fields } => {
                for (_, ty) in fields.iter_mut() {
                    Self::expand_impl(ty, subst);
                }
            }

            Base::Tuple { fields } => {
                for ty in fields.iter_mut() {
                    Self::expand_impl(ty, subst);
                }
            }

            Base::Array { element } => {
                Self::expand_impl(element, subst);
            }

            Base::Function { input, output } => {
                Self::expand_impl(input, subst);
                Self::expand_impl(output, subst);
            }
        }
    }

    fn find_field<'a>(fields: &'a [(&str, Type)], name: &str) -> Option<&'a Type> {
        fields.iter().find_map(|(n, t)| (*n == name).then_some(t))
    }

    fn make_normal_form(lhs: Type, rhs: Type) -> Type {
        lhs.inter(rhs.neg())
    }
}
