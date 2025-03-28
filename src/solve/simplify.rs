use super::{Base, Conj, Solver, Term, Ty};

impl Solver {
    pub fn simplify(&self, mut ty: Ty) -> Ty {
        let mut i = 0;

        while i + 1 < ty.0.len() {
            let mut j = i + 1;

            while j < ty.0.len() {
                if self.is_conj_subty(&ty.0[i], &ty.0[j]) {
                    ty.0.remove(j);
                } else if self.is_conj_subty(&ty.0[j], &ty.0[i]) {
                    ty.0.remove(i);
                } else {
                    j += 1;
                }
            }

            i += 1;
        }

        ty
    }

    pub fn is_subty(&self, lhs: &Ty, rhs: &Ty) -> bool {
        if lhs == rhs {
            return true;
        }

        let key = (lhs.clone(), rhs.clone());

        if self.cache.get(&key).is_some_and(|b| *b) {
            return true;
        }

        for lc in &lhs.0 {
            for rc in &rhs.0 {
                if !self.is_conj_subty(lc, rc) {
                    return false;
                }
            }
        }

        true
    }

    fn is_conj_subty(&self, lhs: &Conj, rhs: &Conj) -> bool {
        if lhs == rhs {
            return true;
        }

        if !self.is_term_subty_pos(&lhs.pos, &rhs.pos) {
            return false;
        }

        if !self.is_term_subty_neg(&rhs.neg, &lhs.neg) {
            return false;
        }

        true
    }

    fn is_term_subty_pos(&self, lhs: &Term, rhs: &Term) -> bool {
        if lhs == rhs {
            return true;
        }

        if !rhs.vars.is_subset(&lhs.vars) {
            return false;
        }

        if !rhs.apps.is_subset(&lhs.apps) {
            return false;
        }

        if !rhs.tags.is_subset(&lhs.tags) {
            return false;
        }

        match (&lhs.base, &rhs.base) {
            (None, None) => true,
            (None, Some(_)) => false,
            (Some(_), None) => true,
            (Some(lhs), Some(rhs)) => self.base_is_subty(lhs, rhs),
        }
    }

    fn is_term_subty_neg(&self, lhs: &Term, rhs: &Term) -> bool {
        if lhs == rhs {
            return true;
        }

        if !lhs.vars.is_subset(&rhs.vars) {
            return false;
        }

        if !lhs.apps.is_subset(&rhs.apps) {
            return false;
        }

        if !lhs.tags.is_subset(&rhs.tags) {
            return false;
        }

        match (&lhs.base, &rhs.base) {
            (None, None) => true,
            (None, Some(_)) => true,
            (Some(_), None) => false,
            (Some(lhs), Some(rhs)) => self.base_is_subty(lhs, rhs),
        }
    }

    fn base_is_subty(&self, lhs: &Base, rhs: &Base) -> bool {
        match (lhs, rhs) {
            (Base::Record(lhs), Base::Record(rhs)) => {
                for (n, r) in rhs {
                    let Some(l) = lhs.get(n) else {
                        return false;
                    };

                    if !self.is_subty(l, r) {
                        return false;
                    }
                }

                true
            }

            (Base::Tuple(lhs), Base::Tuple(rhs)) => {
                if lhs.len() != rhs.len() {
                    return false;
                }

                for (l, r) in lhs.iter().zip(rhs.iter()) {
                    if !self.is_subty(l, r) {
                        return false;
                    }
                }

                true
            }

            (Base::Array(lhs), Base::Array(rhs)) => self.is_subty(lhs, rhs),

            (Base::Func(li, lo), Base::Func(ri, ro)) => {
                self.is_subty(ri, li) && self.is_subty(lo, ro)
            }

            (Base::Ref(lhs), Base::Ref(rhs)) => self.is_subty(lhs, rhs) && self.is_subty(rhs, lhs),

            (_, _) => false,
        }
    }
}
