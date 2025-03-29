use super::{Base, Conj, Solver, Term, Ty};

impl Solver {
    pub fn simplify(&mut self, mut ty: Ty) -> Ty {
        ty.0.retain_mut(|conj| self.simplify_conj(conj));

        let mut i = 0;

        while i + 1 < ty.0.len() {
            let mut j = i + 1;

            while j < ty.0.len() {
                let t0 = Ty::from(ty.0[i].clone());
                let t1 = Ty::from(ty.0[j].clone());

                if self.is_subty(&t0, &t1) {
                    ty.0.remove(i);
                    break;
                } else if self.is_subty(&t1, &t0) {
                    ty.0.remove(j);
                } else {
                    j += 1;
                }
            }

            i += 1;
        }

        ty
    }

    fn simplify_conj(&mut self, conj: &mut Conj) -> bool {
        conj.pos = self.simplify_term(conj.pos.clone());
        conj.neg = self.simplify_term(conj.neg.clone());

        if !conj.pos.tags.is_empty() {
            conj.pos.tags.retain(|tag| !conj.neg.tags.remove(tag));

            if conj.pos.tags.is_empty() {
                return false;
            }
        }

        if !conj.pos.apps.is_empty() {
            conj.pos.apps.retain(|app| !conj.neg.apps.remove(app));

            if conj.pos.apps.is_empty() {
                return false;
            }
        }

        if !conj.neg.is_extreme() && self.is_term_subty(conj.pos.clone(), conj.neg.clone()) {
            return false;
        }

        true
    }

    fn simplify_term(&mut self, term: Term) -> Term {
        let mut new = Term::extreme();

        for var in term.vars {
            let lhs = Ty::var(var);
            let rhs = Ty::term_pos(new.clone());

            if self.is_subty(&lhs, &rhs) {
                new = Term::var(var);
            } else if !self.is_subty(&rhs, &lhs) {
                new.vars.insert(var);
            }
        }

        for mut app in term.apps {
            for arg in &mut app.args {
                *arg = self.simplify(arg.clone());
            }

            let lhs = Ty::app(app.tag, app.args.clone());
            let rhs = Ty::term_pos(new.clone());

            if self.is_subty(&lhs, &rhs) {
                new = Term::app(app.tag, app.args);
            } else if !self.is_subty(&rhs, &lhs) {
                new.apps.insert(app);
            }
        }

        for tag in term.tags {
            let lhs = Ty::tag(tag);
            let rhs = Ty::term_pos(new.clone());

            if self.is_subty(&lhs, &rhs) {
                new = Term::tag(tag);
            } else if !self.is_subty(&rhs, &lhs) {
                new.tags.insert(tag);
            }
        }

        if let Some(mut base) = term.base {
            self.simplify_base(&mut base);

            let lhs = Ty::base(base.clone());
            let rhs = Ty::term_pos(new.clone());

            if self.is_subty(&lhs, &rhs) {
                new = Term::base(base);
            } else if !self.is_subty(&rhs, &lhs) {
                new.base = Some(base);
            }
        }

        new
    }

    fn simplify_base(&mut self, base: &mut Base) {
        match base {
            Base::Record(fields) => {
                for ty in fields.values_mut() {
                    *ty = self.simplify(ty.clone());
                }
            }

            Base::Tuple(tys) => {
                for ty in tys {
                    *ty = self.simplify(ty.clone());
                }
            }

            Base::Array(ty) => {
                *ty = self.simplify(ty.clone());
            }

            Base::Func(i, o) => {
                *i = self.simplify(i.clone());
                *o = self.simplify(o.clone());
            }

            Base::Ref(ty) => {
                *ty = self.simplify(ty.clone());
            }
        }
    }

    pub fn is_subty(&mut self, lhs: &Ty, rhs: &Ty) -> bool {
        let key = (lhs.clone(), rhs.clone());

        if let Some(&result) = self.cache.get(&key) {
            return result != Some(false);
        }

        self.cache.insert(key.clone(), None);

        let result = self.is_subty_uncached(lhs, rhs);

        self.cache.insert(key, Some(result));

        result
    }

    fn is_subty_uncached(&mut self, lhs: &Ty, rhs: &Ty) -> bool {
        let mut combined = lhs.clone();
        combined.inter(rhs.clone().neg());

        for conj in combined.0 {
            if !self.is_term_subty(conj.pos, conj.neg) {
                return false;
            }
        }

        true
    }

    fn is_term_subty(&mut self, mut lhs: Term, mut rhs: Term) -> bool {
        if let Some(var) = lhs.vars.pop_first() {
            let mut bound = Ty::term_neg(rhs).neg();
            bound.union(Ty::term_pos(lhs).neg());

            let lower = self.bounds[&var].lower.clone();

            return self.is_subty(&lower, &bound);
        }

        if let Some(var) = rhs.vars.pop_first() {
            let mut bound = Ty::term_pos(lhs);
            bound.inter(Ty::term_neg(rhs));

            let upper = self.bounds[&var].upper.clone();

            return self.is_subty(&bound, &upper);
        }

        if let Some(app) = lhs.apps.pop_first() {
            let mut rhs = Ty::term_neg(rhs).neg();
            rhs.union(Ty::term_pos(lhs).neg());

            let body = self.expand(app);

            return self.is_subty(&body, &rhs);
        }

        if let Some(app) = rhs.apps.pop_first() {
            let mut lhs = Ty::term_pos(lhs);
            lhs.inter(Ty::term_neg(rhs));

            let body = self.expand(app);

            return self.is_subty(&lhs, &body);
        }

        if rhs.tags.iter().any(|tag| lhs.tags.contains(tag)) {
            return true;
        }

        match (&lhs.base, &rhs.base) {
            (Some(Base::Record(lfields)), Some(Base::Record(rfields))) => {
                for (field, ty) in rfields {
                    let Some(lhs_ty) = lfields.get(field) else {
                        return false;
                    };

                    if !self.is_subty(lhs_ty, ty) {
                        return false;
                    }
                }

                true
            }

            (Some(Base::Tuple(litems)), Some(Base::Tuple(ritems))) => {
                if litems.len() != ritems.len() {
                    return false;
                }

                for (lty, rty) in litems.iter().zip(ritems.iter()) {
                    if !self.is_subty(lty, rty) {
                        return false;
                    }
                }

                true
            }

            (Some(Base::Array(lhs)), Some(Base::Array(rhs))) => self.is_subty(lhs, rhs),

            (Some(Base::Func(li, lo)), Some(Base::Func(ri, ro))) => {
                self.is_subty(ri, li) && self.is_subty(lo, ro)
            }

            (Some(Base::Ref(lhs)), Some(Base::Ref(rhs))) => {
                // references aren't covariant
                self.is_subty(lhs, rhs) && self.is_subty(rhs, lhs)
            }

            (_, _) => return false,
        }
    }
}
