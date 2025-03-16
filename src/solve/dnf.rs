use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    iter,
};

use super::{App, Field, Name, Solver, Ty, Var};

#[derive(Clone, Debug, Default)]
pub struct Dnf(pub Vec<Conjunct>);

#[derive(Clone, Debug, Default)]
pub struct Cnf(pub Vec<Disjunct>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Disjunct {
    pub rnf: Rnf,
    pub vars: BTreeSet<Var>,

    pub lnf: Lnf,
    pub nvars: BTreeSet<Var>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Conjunct {
    pub lnf: Lnf,
    pub vars: BTreeSet<Var>,

    pub rnf: Rnf,
    pub nvars: BTreeSet<Var>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lnf {
    Top,
    Base {
        names: BTreeSet<Name>,
        apps: BTreeSet<App>,
        base: LnfBase,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LnfBase {
    None,
    Record(BTreeMap<Field, Ty>),
    Tuple(Vec<Ty>),
    Func(Ty, Ty),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Rnf {
    Bot,
    Base {
        names: BTreeSet<Name>,
        apps: BTreeSet<App>,
        base: RnfBase,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RnfBase {
    None,
    Field(&'static str, Ty),
    Tuple(Vec<Ty>),
    Func(Ty, Ty),
}

impl Dnf {
    pub fn neg(self) -> Cnf {
        Cnf(self.0.into_iter().map(Conjunct::neg).collect())
    }

    pub fn lnf(lnf: Lnf) -> Self {
        Dnf(vec![Conjunct {
            lnf,
            vars: BTreeSet::new(),

            rnf: Rnf::Bot,
            nvars: BTreeSet::new(),
        }])
    }

    pub fn var(var: Var) -> Self {
        Dnf(vec![Conjunct {
            lnf: Lnf::Top,
            vars: BTreeSet::from([var]),

            rnf: Rnf::Bot,
            nvars: BTreeSet::new(),
        }])
    }

    pub fn to_ty(&self) -> Ty {
        let mut conjs = self.0.iter().map(Conjunct::to_ty);

        match conjs.next() {
            Some(conj) => conjs.fold(conj, Ty::union).simplify(),
            None => Ty::Bot,
        }
    }
}

impl Cnf {
    pub fn neg(self) -> Dnf {
        Dnf(self.0.into_iter().map(Disjunct::neg).collect())
    }

    pub fn rnf(rnf: Rnf) -> Self {
        Cnf(vec![Disjunct {
            rnf,
            vars: BTreeSet::new(),

            lnf: Lnf::Top,
            nvars: BTreeSet::new(),
        }])
    }

    pub fn var(var: Var) -> Self {
        Cnf(vec![Disjunct {
            rnf: Rnf::Bot,
            vars: BTreeSet::from([var]),

            lnf: Lnf::Top,
            nvars: BTreeSet::new(),
        }])
    }

    pub fn to_ty(&self) -> Ty {
        Ty::neg(self.clone().neg().to_ty()).simplify()
    }
}

impl Disjunct {
    pub fn field(field: Field, ty: Ty) -> Self {
        Disjunct {
            rnf: Rnf::Base {
                names: BTreeSet::new(),
                apps: BTreeSet::new(),
                base: RnfBase::Field(field, ty),
            },
            nvars: BTreeSet::new(),

            lnf: Lnf::Top,
            vars: BTreeSet::new(),
        }
    }

    pub fn neg(self) -> Conjunct {
        Conjunct {
            lnf: self.lnf,
            vars: self.nvars,

            rnf: self.rnf,
            nvars: self.vars,
        }
    }

    pub fn to_ty(&self) -> Ty {
        Ty::neg(self.clone().neg().to_ty()).simplify()
    }
}

impl Conjunct {
    pub fn neg(self) -> Disjunct {
        Disjunct {
            lnf: self.lnf,
            vars: self.nvars,

            rnf: self.rnf,
            nvars: self.vars,
        }
    }

    pub fn to_ty(&self) -> Ty {
        let rnf = iter::once(self.rnf.to_ty()).map(Ty::neg);
        let vars = self.vars.iter().cloned().map(Ty::Var);
        let nvars = self.nvars.iter().cloned().map(Ty::Var).map(Ty::neg);

        vars.chain(nvars)
            .chain(rnf)
            .fold(self.lnf.to_ty(), Ty::inter)
            .simplify()
    }
}

impl Lnf {
    pub fn bot() -> Self {
        Lnf::Base {
            names: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::None,
        }
    }

    pub fn name(name: Name) -> Self {
        Lnf::Base {
            names: BTreeSet::from([name]),
            apps: BTreeSet::new(),
            base: LnfBase::None,
        }
    }

    pub fn tuple(tys: Vec<Ty>) -> Self {
        Lnf::Base {
            names: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::Tuple(tys),
        }
    }

    pub fn app(app: App) -> Self {
        Lnf::Base {
            names: BTreeSet::new(),
            apps: BTreeSet::from([app]),
            base: LnfBase::None,
        }
    }

    pub fn to_ty(&self) -> Ty {
        match self {
            Lnf::Top => Ty::Top,
            Lnf::Base { names, apps, base } => {
                let base = match base {
                    LnfBase::None => {
                        if names.is_empty() && apps.is_empty() {
                            Ty::Bot
                        } else {
                            Ty::Top
                        }
                    }

                    LnfBase::Func(input, output) => {
                        let input = input.clone();
                        let output = output.clone();

                        Ty::func(input, output)
                    }

                    LnfBase::Record(fields) => Ty::Record(fields.clone()),

                    LnfBase::Tuple(tys) => Ty::Tuple(tys.clone()),
                };

                let apps = apps.iter().cloned().map(Ty::App);

                names
                    .iter()
                    .cloned()
                    .map(Ty::Name)
                    .chain(apps)
                    .fold(base, |l, r| Ty::inter(r, l))
                    .simplify()
            }
        }
    }

    pub fn is_top(&self) -> bool {
        matches!(self, Lnf::Top)
    }

    pub fn is_bot(&self) -> bool {
        matches!(
            self,
            Lnf::Base {
                names,
                apps,
                base: LnfBase::None
            } if names.is_empty() && apps.is_empty()
        )
    }
}

impl Rnf {
    pub fn top() -> Self {
        Rnf::Base {
            names: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::None,
        }
    }

    pub fn name(name: Name) -> Self {
        Rnf::Base {
            names: BTreeSet::from([name]),
            apps: BTreeSet::new(),
            base: RnfBase::None,
        }
    }

    pub fn tuple(tys: Vec<Ty>) -> Self {
        Rnf::Base {
            names: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::Tuple(tys),
        }
    }

    pub fn app(app: App) -> Self {
        Rnf::Base {
            names: BTreeSet::new(),
            apps: BTreeSet::from([app]),
            base: RnfBase::None,
        }
    }

    pub fn to_ty(&self) -> Ty {
        match self {
            Rnf::Bot => Ty::Bot,
            Rnf::Base { names, apps, base } => {
                let base = match base {
                    RnfBase::None => {
                        if names.is_empty() && apps.is_empty() {
                            Ty::Top
                        } else {
                            Ty::Bot
                        }
                    }

                    RnfBase::Field(field, ty) => Ty::record([(*field, ty.clone())]),
                    RnfBase::Func(input, output) => Ty::func(input.clone(), output.clone()),
                    RnfBase::Tuple(tys) => Ty::tuple(tys.clone()),
                };

                let apps = apps.iter().cloned().map(Ty::App);

                names
                    .iter()
                    .cloned()
                    .map(Ty::Name)
                    .chain(apps)
                    .fold(base, Ty::union)
                    .simplify()
            }
        }
    }

    pub fn is_top(&self) -> bool {
        matches!(self, Rnf::Base { names, apps, base: RnfBase::None } if names.is_empty() && apps.is_empty())
    }

    pub fn is_bot(&self) -> bool {
        matches!(self, Rnf::Bot)
    }
}

impl Solver {
    pub fn dnf(&self, ty: &Ty) -> Dnf {
        match ty {
            Ty::Top => Dnf::lnf(Lnf::Top),
            Ty::Bot => Dnf::lnf(Lnf::bot()),

            Ty::Name(name) => Dnf::lnf(Lnf::name(name)),

            Ty::Neg(ty) => self.cnf(ty).neg(),

            Ty::Var(var) => Dnf::var(*var),

            Ty::Union(t1, t2) => {
                let Dnf(dnf1) = self.dnf(t1);
                let Dnf(dnf2) = self.dnf(t2);

                let mut conjuncts = Vec::new();

                for c1 in dnf1 {
                    conjuncts.push(c1);
                }

                for c2 in dnf2 {
                    conjuncts.push(c2);
                }

                self.simplify_dnf(Dnf(conjuncts))
            }

            Ty::Inter(t1, t2) => {
                let Dnf(dnf1) = self.dnf(t1);
                let Dnf(dnf2) = self.dnf(t2);

                let mut conjuncts: Vec<Conjunct> = Vec::new();

                for c1 in dnf1 {
                    for c2 in dnf2.clone() {
                        let conjunct = self.inter_conjunct(c1.clone(), c2);
                        conjuncts.push(conjunct);
                    }
                }

                self.simplify_dnf(Dnf(conjuncts))
            }

            Ty::Record(fields) => {
                let mut dnfs = BTreeMap::new();

                for (field, ty) in fields {
                    dnfs.insert(*field, ty.clone());
                }

                Dnf::lnf(Lnf::Base {
                    names: BTreeSet::new(),
                    apps: BTreeSet::new(),
                    base: LnfBase::Record(dnfs),
                })
            }

            Ty::Func(input, output) => {
                let input = input.as_ref().clone();
                let output = output.as_ref().clone();

                Dnf::lnf(Lnf::Base {
                    names: BTreeSet::new(),
                    apps: BTreeSet::new(),
                    base: LnfBase::Func(input, output),
                })
            }

            Ty::Tuple(tys) => Dnf::lnf(Lnf::tuple(tys.clone())),
            Ty::App(app) => Dnf::lnf(Lnf::app(app.clone())),
        }
    }

    pub(super) fn cnf(&self, ty: &Ty) -> Cnf {
        match ty {
            Ty::Top => Cnf::rnf(Rnf::top()),
            Ty::Bot => Cnf::rnf(Rnf::Bot),

            Ty::Name(name) => Cnf::rnf(Rnf::name(name)),

            Ty::Neg(ty) => self.dnf(ty).neg(),

            Ty::Var(var) => Cnf::var(*var),

            Ty::Inter(t1, t2) => {
                let Cnf(cnf1) = self.cnf(t1);
                let Cnf(cnf2) = self.cnf(t2);

                let mut disjuncts = Vec::new();

                for c1 in cnf1 {
                    disjuncts.push(c1);
                }

                for c2 in cnf2 {
                    disjuncts.push(c2);
                }

                self.simplify_cnf(Cnf(disjuncts))
            }

            Ty::Union(t1, t2) => {
                let Cnf(cnf1) = self.cnf(t1);
                let Cnf(cnf2) = self.cnf(t2);

                let mut disjuncts: Vec<Disjunct> = Vec::new();

                for c1 in cnf1 {
                    for c2 in cnf2.clone() {
                        let disjunct = self.union_disjunct(c1.clone(), c2);
                        disjuncts.push(disjunct);
                    }
                }

                self.simplify_cnf(Cnf(disjuncts))
            }

            Ty::Record(fields) => {
                let mut disjuncts = Vec::new();

                for (field, ty) in fields {
                    disjuncts.push(Disjunct::field(field, ty.clone()));
                }

                Cnf(disjuncts)
            }

            Ty::Func(input, output) => {
                let input = input.as_ref().clone();
                let output = output.as_ref().clone();

                Cnf::rnf(Rnf::Base {
                    names: BTreeSet::new(),
                    apps: BTreeSet::new(),
                    base: RnfBase::Func(input, output),
                })
            }

            Ty::Tuple(tys) => Cnf::rnf(Rnf::tuple(tys.clone())),
            Ty::App(app) => Cnf::rnf(Rnf::app(app.clone())),
        }
    }

    pub(super) fn inter_conjunct(&self, lhs: Conjunct, rhs: Conjunct) -> Conjunct {
        Conjunct {
            lnf: self.inter_lnf(lhs.lnf, rhs.lnf),
            vars: lhs.vars.union(&rhs.vars).cloned().collect(),

            rnf: self.union_rnf(lhs.rnf, rhs.rnf),
            nvars: lhs.nvars.union(&rhs.nvars).cloned().collect(),
        }
    }

    pub(super) fn union_disjunct(&self, lhs: Disjunct, rhs: Disjunct) -> Disjunct {
        Disjunct {
            rnf: self.union_rnf(lhs.rnf, rhs.rnf),
            vars: lhs.vars.union(&rhs.vars).cloned().collect(),

            lnf: self.inter_lnf(lhs.lnf, rhs.lnf),
            nvars: lhs.nvars.union(&rhs.nvars).cloned().collect(),
        }
    }

    pub(super) fn inter_lnf(&self, l1: Lnf, l2: Lnf) -> Lnf {
        match (l1, l2) {
            (Lnf::Top, l2) => l2,
            (l1, Lnf::Top) => l1,

            (l1, l2) if l1.is_bot() || l2.is_bot() => Lnf::bot(),

            (
                Lnf::Base {
                    names: n1,
                    apps: a1,
                    base: b1,
                },
                Lnf::Base {
                    names: n2,
                    apps: a2,
                    base: b2,
                },
            ) => {
                let names = n1.union(&n2).cloned().collect();
                let apps = a1.union(&a2).cloned().collect();

                let base = match (b1, b2) {
                    (LnfBase::None, base) | (base, LnfBase::None) => base,

                    (LnfBase::Func(i1, o1), LnfBase::Func(i2, o2)) => {
                        let input = Ty::inter(i1.clone(), i2.clone()).simplify();
                        let output = Ty::union(o1.clone(), o2.clone()).simplify();

                        LnfBase::Func(input, output)
                    }

                    (LnfBase::Record(mut f1), LnfBase::Record(f2)) => {
                        for (field, t2) in f2 {
                            match f1.entry(field) {
                                Entry::Vacant(entry) => {
                                    entry.insert(t2);
                                }

                                Entry::Occupied(mut entry) => {
                                    let t1 = entry.get_mut();
                                    *t1 = Ty::inter(t1.clone(), t2.clone()).simplify();
                                }
                            }
                        }

                        LnfBase::Record(f1)
                    }

                    (LnfBase::Tuple(t1), LnfBase::Tuple(t2)) if t1.len() == t2.len() => {
                        let tys = t1
                            .into_iter()
                            .zip(t2)
                            .map(|(t1, t2)| Ty::inter(t1, t2).simplify())
                            .collect();

                        LnfBase::Tuple(tys)
                    }

                    (_, _) => LnfBase::None,
                };

                Lnf::Base { names, apps, base }
            }
        }
    }

    pub(super) fn union_rnf(&self, r1: Rnf, r2: Rnf) -> Rnf {
        match (r1, r2) {
            (Rnf::Bot, r2) => r2,
            (r1, Rnf::Bot) => r1,

            (r1, r2) if r1.is_top() || r2.is_top() => Rnf::top(),

            (
                Rnf::Base {
                    names: n1,
                    apps: a1,
                    base: b1,
                },
                Rnf::Base {
                    names: n2,
                    apps: a2,
                    base: b2,
                },
            ) => {
                let names = n1.union(&n2).cloned().collect();
                let apps = a1.union(&a2).cloned().collect();

                let base = match (b1, b2) {
                    (RnfBase::None, base) | (base, RnfBase::None) => base,

                    (RnfBase::Func(i1, o1), RnfBase::Func(i2, o2)) => {
                        let input = Ty::union(i1.clone(), i2.clone()).simplify();
                        let output = Ty::inter(o1.clone(), o2.clone()).simplify();

                        RnfBase::Func(input, output)
                    }

                    (RnfBase::Field(n1, t1), RnfBase::Field(n2, t2)) => {
                        if n1 == n2 {
                            RnfBase::Field(n1, Ty::union(t1.clone(), t2.clone()).simplify())
                        } else {
                            RnfBase::None
                        }
                    }

                    (RnfBase::Tuple(t1), RnfBase::Tuple(t2)) if t1.len() == t2.len() => {
                        let tys = t1
                            .into_iter()
                            .zip(t2)
                            .map(|(t1, t2)| Ty::union(t1, t2).simplify())
                            .collect();

                        RnfBase::Tuple(tys)
                    }

                    (_, _) => RnfBase::None,
                };

                Rnf::Base { names, apps, base }
            }
        }
    }

    /// Check if 'lhs' is a subtype of 'rhs', that is `lhs <: rhs`.
    pub(super) fn is_subty_of(&self, lhs: &Ty, rhs: &Ty) -> bool {
        if lhs == rhs {
            return true;
        }

        let key = (lhs.clone(), rhs.clone());

        if *self.cache.get(&key).unwrap_or(&false) {
            return true;
        }

        match (lhs, rhs) {
            (_, Ty::Top) => true,
            (Ty::Bot, _) => true,

            (Ty::Name(lhs), Ty::Name(rhs)) => lhs == rhs,

            (lhs, Ty::Neg(rhs)) => !self.is_subty_of(lhs, rhs),

            (Ty::Union(t1, t2), rhs) => self.is_subty_of(t1, rhs) && self.is_subty_of(t2, rhs),
            (Ty::Inter(t1, t2), rhs) => self.is_subty_of(t1, rhs) || self.is_subty_of(t2, rhs),

            (lhs, Ty::Union(t1, t2)) => self.is_subty_of(lhs, t1) || self.is_subty_of(lhs, t2),
            (lhs, Ty::Inter(t1, t2)) => self.is_subty_of(lhs, t1) && self.is_subty_of(lhs, t2),

            (Ty::Record(lhs), Ty::Record(rhs)) => {
                for (field, rhs) in rhs {
                    match lhs.get(field) {
                        Some(lhs) => {
                            if !self.is_subty_of(lhs, rhs) {
                                return false;
                            }
                        }

                        None => return false,
                    }
                }

                true
            }

            (Ty::Func(i1, o1), Ty::Func(i2, o2)) => {
                self.is_subty_of(i2, i1) && self.is_subty_of(o1, o2)
            }

            (_, _) => false,
        }
    }

    pub(super) fn simplify_dnf(&self, Dnf(conjuncts): Dnf) -> Dnf {
        if !self.options.simplify_normal_forms {
            return Dnf(conjuncts);
        }

        let mut new_conjuncts: Vec<Conjunct> = Vec::new();

        'outer: for conj in conjuncts {
            for new_conj in new_conjuncts.iter_mut() {
                if self.is_subty_of(&conj.to_ty(), &new_conj.to_ty()) {
                    continue 'outer;
                }

                if self.is_subty_of(&new_conj.to_ty(), &conj.to_ty()) {
                    *new_conj = conj;
                    continue 'outer;
                }
            }

            new_conjuncts.push(conj);
        }

        Dnf(new_conjuncts)
    }

    pub(super) fn simplify_cnf(&self, Cnf(disjuncts): Cnf) -> Cnf {
        if !self.options.simplify_normal_forms {
            return Cnf(disjuncts);
        }

        let mut new_disjuncts: Vec<Disjunct> = Vec::new();

        'outer: for disj in disjuncts {
            for new_disj in new_disjuncts.iter_mut() {
                if self.is_subty_of(&new_disj.to_ty(), &disj.to_ty()) {
                    continue 'outer;
                }

                if self.is_subty_of(&disj.to_ty(), &new_disj.to_ty()) {
                    *new_disj = disj;
                    continue 'outer;
                }
            }

            new_disjuncts.push(disj);
        }

        Cnf(new_disjuncts)
    }
}
