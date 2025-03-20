use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    iter,
};

use super::{App, Field, Solver, Tag, Ty, Var};

#[derive(Clone, Debug, Default)]
pub struct Dnf(pub Vec<Conjunct>);

#[derive(Clone, Debug, Default)]
pub struct Cnf(pub Vec<Disjunct>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Conjunct {
    pub lnf: Lnf,
    pub vars: BTreeSet<Var>,

    /* negated */
    pub rnf: Rnf,
    pub nvars: BTreeSet<Var>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Disjunct {
    pub rnf: Rnf,
    pub vars: BTreeSet<Var>,

    /* negated */
    pub lnf: Lnf,
    pub nvars: BTreeSet<Var>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lnf {
    Top,
    Base {
        tags: BTreeSet<Tag>,
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
    List(Ty),
    Ref(Ty),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Rnf {
    Bot,
    Base {
        tags: BTreeSet<Tag>,
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
    List(Ty),
    Ref(Ty),
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
            Some(conj) => conjs.fold(conj, Ty::union),
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
        Ty::neg(self.clone().neg().to_ty())
    }
}

impl Disjunct {
    pub fn field(field: Field, ty: Ty) -> Self {
        Disjunct {
            rnf: Rnf::Base {
                tags: BTreeSet::new(),
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
        Ty::neg(self.clone().neg().to_ty())
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
    }
}

impl Lnf {
    pub fn bot() -> Self {
        Lnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::None,
        }
    }

    pub fn tag(tag: Tag) -> Self {
        Lnf::Base {
            tags: BTreeSet::from([tag]),
            apps: BTreeSet::new(),
            base: LnfBase::None,
        }
    }

    pub fn tuple(tys: Vec<Ty>) -> Self {
        Lnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::Tuple(tys),
        }
    }

    pub fn func(input: Ty, output: Ty) -> Self {
        Lnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::Func(input, output),
        }
    }

    pub fn list(ty: Ty) -> Self {
        Lnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::List(ty),
        }
    }

    pub fn ref_(ty: Ty) -> Self {
        Lnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: LnfBase::Ref(ty),
        }
    }

    pub fn app(app: App) -> Self {
        Lnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::from([app]),
            base: LnfBase::None,
        }
    }

    pub fn to_ty(&self) -> Ty {
        match self {
            Lnf::Top => Ty::Top,
            Lnf::Base { tags, apps, base } => {
                let base = match base {
                    LnfBase::None => {
                        if tags.is_empty() && apps.is_empty() {
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

                    LnfBase::List(ty) => Ty::list(ty.clone()),
                    LnfBase::Ref(ty) => Ty::ref_(ty.clone()),

                    LnfBase::Record(fields) => Ty::Record(fields.clone()),

                    LnfBase::Tuple(tys) => Ty::Tuple(tys.clone()),
                };

                let apps = apps.iter().cloned().map(Ty::App);

                tags.iter()
                    .cloned()
                    .map(Ty::Tag)
                    .chain(apps)
                    .fold(base, |l, r| Ty::inter(r, l))
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
                tags,
                apps,
                base: LnfBase::None
            } if tags.is_empty() && apps.is_empty()
        )
    }
}

impl Rnf {
    pub fn top() -> Self {
        Rnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::None,
        }
    }

    pub fn tag(tag: Tag) -> Self {
        Rnf::Base {
            tags: BTreeSet::from([tag]),
            apps: BTreeSet::new(),
            base: RnfBase::None,
        }
    }

    pub fn tuple(tys: Vec<Ty>) -> Self {
        Rnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::Tuple(tys),
        }
    }

    pub fn func(input: Ty, output: Ty) -> Self {
        Rnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::Func(input, output),
        }
    }

    pub fn list(ty: Ty) -> Self {
        Rnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::List(ty),
        }
    }

    pub fn ref_(ty: Ty) -> Self {
        Rnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: RnfBase::Ref(ty),
        }
    }

    pub fn app(app: App) -> Self {
        Rnf::Base {
            tags: BTreeSet::new(),
            apps: BTreeSet::from([app]),
            base: RnfBase::None,
        }
    }

    pub fn to_ty(&self) -> Ty {
        match self {
            Rnf::Bot => Ty::Bot,
            Rnf::Base { tags, apps, base } => {
                let base = match base {
                    RnfBase::None => {
                        if tags.is_empty() && apps.is_empty() {
                            Ty::Top
                        } else {
                            Ty::Bot
                        }
                    }

                    RnfBase::Field(field, ty) => Ty::record([(*field, ty.clone())]),
                    RnfBase::List(ty) => Ty::list(ty.clone()),
                    RnfBase::Ref(ty) => Ty::ref_(ty.clone()),
                    RnfBase::Func(input, output) => Ty::func(input.clone(), output.clone()),
                    RnfBase::Tuple(tys) => Ty::tuple(tys.clone()),
                };

                let apps = apps.iter().cloned().map(Ty::App);

                tags.iter()
                    .cloned()
                    .map(Ty::Tag)
                    .chain(apps)
                    .fold(base, Ty::union)
            }
        }
    }

    pub fn is_top(&self) -> bool {
        matches!(self, Rnf::Base { tags, apps, base: RnfBase::None } if tags.is_empty() && apps.is_empty())
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

            Ty::Tag(tag) => Dnf::lnf(Lnf::tag(*tag)),

            Ty::Neg(ty) => self.cnf(ty).neg(),

            Ty::Var(var) => Dnf::var(*var),

            // (a | b | ..) | (c | d | ..) | .. => a | b | c | d | ..
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

                Dnf(conjuncts)
            }

            // (a | b | ..) & (c | d | ..) & .. => (a & c & ..) | (a & d & ..) | ..
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

                Dnf(conjuncts)
            }

            Ty::Record(fields) => {
                let mut dnfs = BTreeMap::new();

                for (field, ty) in fields {
                    let ty = ty.clone();
                    dnfs.insert(*field, ty);
                }

                Dnf::lnf(Lnf::Base {
                    tags: BTreeSet::new(),
                    apps: BTreeSet::new(),
                    base: LnfBase::Record(dnfs),
                })
            }

            Ty::Func(input, output) => {
                let input = input.as_ref().clone();
                let output = output.as_ref().clone();

                Dnf::lnf(Lnf::func(input, output))
            }

            Ty::List(ty) => Dnf::lnf(Lnf::list(ty.as_ref().clone())),
            Ty::Ref(ty) => Dnf::lnf(Lnf::ref_(ty.as_ref().clone())),

            Ty::Tuple(tys) => Dnf::lnf(Lnf::tuple(tys.clone())),
            Ty::App(app) => Dnf::lnf(Lnf::app(app.clone())),
        }
    }

    pub(super) fn cnf(&self, ty: &Ty) -> Cnf {
        match ty {
            Ty::Top => Cnf::rnf(Rnf::top()),
            Ty::Bot => Cnf::rnf(Rnf::Bot),

            Ty::Tag(tag) => Cnf::rnf(Rnf::tag(*tag)),

            Ty::Neg(ty) => self.dnf(ty).neg(),

            Ty::Var(var) => Cnf::var(*var),

            // (a & b & ..) & (c & d & ..) & .. => a & b & c & d & ..
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

                self.simplify_cnf_strict(Cnf(disjuncts))
            }

            // (a | b | ..) & (c | d | ..) & .. => (a & c & ..) | (a & d & ..) | ..
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

                self.simplify_cnf_strict(Cnf(disjuncts))
            }

            Ty::Record(fields) => {
                let mut disjuncts = Vec::new();

                for (field, ty) in fields {
                    let ty = ty.clone();
                    disjuncts.push(Disjunct::field(field, ty));
                }

                Cnf(disjuncts)
            }

            Ty::Func(input, output) => {
                let input = input.as_ref().clone();
                let output = output.as_ref().clone();

                Cnf::rnf(Rnf::func(input, output))
            }

            Ty::List(ty) => Cnf::rnf(Rnf::list(ty.as_ref().clone())),
            Ty::Ref(ty) => Cnf::rnf(Rnf::ref_(ty.as_ref().clone())),

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
                    tags: n1,
                    apps: a1,
                    base: b1,
                },
                Lnf::Base {
                    tags: n2,
                    apps: a2,
                    base: b2,
                },
            ) => {
                let tags = n1.union(&n2).cloned().collect();
                let apps = a1.union(&a2).cloned().collect();

                let base = match (b1, b2) {
                    (LnfBase::None, base) | (base, LnfBase::None) => base,

                    (LnfBase::Func(i1, o1), LnfBase::Func(i2, o2)) => {
                        let input = Ty::inter(i1.clone(), i2.clone());
                        let output = Ty::union(o1.clone(), o2.clone());

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
                                    *t1 = Ty::inter(t1.clone(), t2.clone());
                                }
                            }
                        }

                        LnfBase::Record(f1)
                    }

                    (LnfBase::Tuple(t1), LnfBase::Tuple(t2)) if t1.len() == t2.len() => {
                        let tys = t1
                            .into_iter()
                            .zip(t2)
                            .map(|(t1, t2)| Ty::inter(t1, t2))
                            .collect();

                        LnfBase::Tuple(tys)
                    }

                    (_, _) => LnfBase::None,
                };

                Lnf::Base { tags, apps, base }
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
                    tags: n1,
                    apps: a1,
                    base: b1,
                },
                Rnf::Base {
                    tags: n2,
                    apps: a2,
                    base: b2,
                },
            ) => {
                let tags = n1.union(&n2).cloned().collect();
                let apps = a1.union(&a2).cloned().collect();

                let base = match (b1, b2) {
                    (RnfBase::None, base) | (base, RnfBase::None) => base,

                    (RnfBase::Func(i1, o1), RnfBase::Func(i2, o2)) => {
                        let input = Ty::union(i1.clone(), i2.clone());
                        let output = Ty::inter(o1.clone(), o2.clone());

                        RnfBase::Func(input, output)
                    }

                    (RnfBase::Field(n1, t1), RnfBase::Field(n2, t2)) => {
                        if n1 == n2 {
                            RnfBase::Field(n1, Ty::union(t1.clone(), t2.clone()))
                        } else {
                            RnfBase::None
                        }
                    }

                    (RnfBase::Tuple(t1), RnfBase::Tuple(t2)) if t1.len() == t2.len() => {
                        let tys = t1
                            .into_iter()
                            .zip(t2)
                            .map(|(t1, t2)| Ty::union(t1, t2))
                            .collect();

                        RnfBase::Tuple(tys)
                    }

                    (_, _) => RnfBase::None,
                };

                Rnf::Base { tags, apps, base }
            }
        }
    }
}
