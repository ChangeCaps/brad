use super::{App, Cnf, Conjunct, Disjunct, Dnf, Solver, Ty};

impl Solver {
    pub fn simplify(&self, ty: &Ty) -> Ty {
        let dnf = self.dnf(ty);
        let dnf = self.simplify_dnf(dnf);
        self.simplify_impl(&dnf.to_ty())
    }

    fn simplify_impl(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Union(t1, t2) => {
                let mut queue = vec![t1.clone(), t2.clone()];
                let mut clean = vec![Ty::Bot];

                'out: while let Some(ty) = queue.pop() {
                    let ty = self.simplify_impl(&ty);

                    if let Ty::Union(t1, t2) = ty {
                        queue.push(t1);
                        queue.push(t2);
                        continue;
                    }

                    for clean_ty in &mut clean {
                        if self.is_subty(&ty, clean_ty) {
                            continue 'out;
                        }

                        if self.is_subty(clean_ty, &ty) {
                            *clean_ty = ty;
                            continue 'out;
                        }
                    }

                    clean.push(ty);
                }

                let first = clean.remove(0);
                clean.into_iter().fold(first, Ty::union)
            }

            Ty::Inter(t1, t2) => {
                let mut queue = vec![t1.clone(), t2.clone()];
                let mut clean = vec![Ty::Top];

                'out: while let Some(ty) = queue.pop() {
                    let ty = self.simplify_impl(&ty);

                    if let Ty::Inter(t1, t2) = ty {
                        queue.push(t1);
                        queue.push(t2);
                        continue;
                    }

                    for clean_ty in &mut clean {
                        if self.is_subty(clean_ty, &ty) {
                            continue 'out;
                        }

                        if self.is_subty(&ty, clean_ty) {
                            *clean_ty = ty;
                            continue 'out;
                        }
                    }

                    clean.push(ty);
                }

                let first = clean.remove(0);
                clean.into_iter().fold(first, Ty::inter)
            }

            Ty::Neg(ty) => match ty.as_ref() {
                Ty::Top => Ty::Bot,
                Ty::Bot => Ty::Top,
                Ty::Neg(ty) => self.simplify_impl(ty),

                Ty::Union(t1, t2) => {
                    let t1 = self.simplify_impl(&Ty::neg(t1.as_ref().clone()));
                    let t2 = self.simplify_impl(&Ty::neg(t2.as_ref().clone()));

                    self.simplify_impl(&Ty::inter(t1, t2))
                }

                Ty::Inter(t1, t2) => {
                    let t1 = self.simplify_impl(&Ty::neg(t1.as_ref().clone()));
                    let t2 = self.simplify_impl(&Ty::neg(t2.as_ref().clone()));

                    self.simplify_impl(&Ty::union(t1, t2))
                }

                _ => Ty::neg(self.simplify_impl(ty)),
            },

            Ty::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| (*name, self.simplify(ty)))
                    .collect();

                Ty::Record(fields)
            }

            Ty::Tuple(items) => {
                let items = items.iter().map(|item| self.simplify(item)).collect();

                Ty::Tuple(items)
            }

            Ty::Func(input, output) => {
                let input = self.simplify(input);
                let output = self.simplify(output);

                Ty::func(input, output)
            }

            Ty::List(ty) => Ty::list(self.simplify(ty)),

            Ty::Ref(ty) => Ty::ref_(self.simplify(ty)),

            Ty::App(app) => {
                let args = app.args.iter().map(|arg| self.simplify(arg)).collect();

                Ty::App(App {
                    name: app.name,
                    args,
                })
            }

            Ty::Tag(_) | Ty::Top | Ty::Bot | Ty::Var(_) => ty.clone(),
        }
    }

    pub fn is_equal(&self, lhs: &Ty, rhs: &Ty) -> bool {
        self.is_subty(lhs, rhs) && self.is_subty(rhs, lhs)
    }

    /// Check if 'lhs' is a subtype of 'rhs', that is `lhs <: rhs`.
    pub fn is_subty(&self, lhs: &Ty, rhs: &Ty) -> bool {
        self.is_subty_impl(lhs, rhs, false)
    }

    pub fn is_subty_strict(&self, lhs: &Ty, rhs: &Ty) -> bool {
        self.is_subty_impl(lhs, rhs, true)
    }

    fn is_subty_impl(&self, lhs: &Ty, rhs: &Ty, strict: bool) -> bool {
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
            (Ty::Top, _) => false,
            (_, Ty::Bot) => false,

            (Ty::Tag(lhs), Ty::Tag(rhs)) => lhs == rhs,

            (lhs, Ty::Neg(rhs)) => {
                let inter = Ty::inter(lhs.clone(), rhs.as_ref().clone());
                self.is_subty(&inter, &Ty::Bot)
            }

            (Ty::Neg(lhs), rhs) => {
                let union = Ty::union(lhs.as_ref().clone(), rhs.clone());
                self.is_subty(&Ty::Top, &union)
            }

            (Ty::Union(t1, t2), rhs) => self.is_subty(t1, rhs) && self.is_subty(t2, rhs),
            (Ty::Inter(t1, t2), rhs) => self.is_subty(t1, rhs) || self.is_subty(t2, rhs),

            (lhs, Ty::Union(t1, t2)) => self.is_subty(lhs, t1) || self.is_subty(lhs, t2),
            (lhs, Ty::Inter(t1, t2)) => self.is_subty(lhs, t1) && self.is_subty(lhs, t2),

            (Ty::Record(lhs), Ty::Record(rhs)) => {
                for (field, rhs) in rhs {
                    match lhs.get(field) {
                        Some(lhs) => {
                            if !self.is_subty(lhs, rhs) {
                                return false;
                            }
                        }

                        None => return false,
                    }
                }

                true
            }

            (Ty::Func(i1, o1), Ty::Func(i2, o2)) => self.is_subty(i2, i1) && self.is_subty(o1, o2),

            (Ty::Tuple(lhs), Ty::Tuple(rhs)) if lhs.len() == rhs.len() => lhs
                .iter()
                .zip(rhs)
                .all(|(lhs, rhs)| self.is_subty(lhs, rhs)),

            (Ty::List(lhs), Ty::List(rhs)) => self.is_subty(lhs, rhs),

            (Ty::Ref(lhs), Ty::Ref(rhs)) => self.is_subty(lhs, rhs) && self.is_subty(rhs, lhs),

            (Ty::App(lhs), Ty::App(rhs)) => {
                if lhs.name != rhs.name {
                    return false;
                }

                if lhs.args.len() == rhs.args.len() {
                    return false;
                }

                for (lhs, rhs) in lhs.args.iter().zip(&rhs.args) {
                    if !self.is_subty(lhs, rhs) {
                        return false;
                    }
                }

                true
            }

            (Ty::App(app), rhs) => {
                let lhs = self.expand(app);
                self.is_subty(&lhs, rhs)
            }

            (lhs, Ty::App(app)) => {
                let rhs = self.expand(app);
                self.is_subty(lhs, &rhs)
            }

            (Ty::Var(lhs), Ty::Var(rhs)) if lhs == rhs => true,

            (Ty::Var(_), _) | (_, Ty::Var(_)) if strict => false,

            (Ty::Var(lhs), rhs) => {
                let bounds = self.bounds.get(lhs).unwrap();

                for bound in &bounds.ubs {
                    if !self.is_subty(bound, rhs) {
                        return false;
                    }
                }

                true
            }

            (lhs, Ty::Var(rhs)) => {
                let bounds = self.bounds.get(rhs).unwrap();

                for bound in &bounds.lbs {
                    if !self.is_subty(lhs, bound) {
                        return false;
                    }
                }

                true
            }

            (Ty::Func(..), _) | (_, Ty::Func(..)) => false,
            (Ty::Tuple(..), _) | (_, Ty::Tuple(..)) => false,
            (Ty::List(..), _) | (_, Ty::List(..)) => false,
            (Ty::Ref(..), _) | (_, Ty::Ref(..)) => false,
            (Ty::Record(..), _) | (_, Ty::Record(..)) => false,
        }
    }

    pub fn simplify_dnf(&self, Dnf(conjuncts): Dnf) -> Dnf {
        self.simplify_dnf_impl(Dnf(conjuncts), false)
    }

    pub fn simplify_dnf_strict(&self, Dnf(conjuncts): Dnf) -> Dnf {
        self.simplify_dnf_impl(Dnf(conjuncts), true)
    }

    fn simplify_dnf_impl(&self, Dnf(conjuncts): Dnf, strict: bool) -> Dnf {
        if !self.options.simplify_normal_forms {
            return Dnf(conjuncts);
        }

        let mut new_conjuncts: Vec<Conjunct> = Vec::new();

        'outer: for conj in conjuncts {
            for new_conj in new_conjuncts.iter_mut() {
                if self.is_subty_impl(&conj.to_ty(), &new_conj.to_ty(), strict) {
                    continue 'outer;
                }

                if self.is_subty_impl(&new_conj.to_ty(), &conj.to_ty(), strict) {
                    *new_conj = conj;
                    continue 'outer;
                }
            }

            new_conjuncts.push(conj);
        }

        Dnf(new_conjuncts)
    }

    pub fn simplify_cnf(&self, Cnf(disjuncts): Cnf) -> Cnf {
        self.simplify_cnf_impl(Cnf(disjuncts), false)
    }

    pub fn simplify_cnf_strict(&self, Cnf(disjuncts): Cnf) -> Cnf {
        self.simplify_cnf_impl(Cnf(disjuncts), true)
    }

    fn simplify_cnf_impl(&self, Cnf(disjuncts): Cnf, strict: bool) -> Cnf {
        if !self.options.simplify_normal_forms {
            return Cnf(disjuncts);
        }

        let mut new_disjuncts: Vec<Disjunct> = Vec::new();

        'outer: for disj in disjuncts {
            for new_disj in new_disjuncts.iter_mut() {
                if self.is_subty_impl(&new_disj.to_ty(), &disj.to_ty(), strict) {
                    continue 'outer;
                }

                if self.is_subty_impl(&disj.to_ty(), &new_disj.to_ty(), strict) {
                    *new_disj = disj;
                    continue 'outer;
                }
            }

            new_disjuncts.push(disj);
        }

        Cnf(new_disjuncts)
    }
}
