use std::{collections::HashSet, mem};

use super::{Base, Solver, Ty, Var};

impl Solver {
    pub fn format_ty(&self, ty: Ty) -> String {
        let ty = self.inline_variables(ty, &HashSet::new(), true);
        let ty = self.simplify(ty);

        ty.to_string()
    }

    fn inline_variables(&self, Ty(ty): Ty, vars: &HashSet<Var>, pos: bool) -> Ty {
        let mut new = Ty::never();

        for mut conj in ty {
            let mut conj_ty = Ty::always();

            for var in mem::take(&mut conj.pos.vars) {
                let bounds = &self.bounds[&var];

                if vars.contains(&var) {
                    conj_ty.inter(Ty::var(var));
                    continue;
                }

                let mut vars = vars.clone();
                vars.insert(var);

                match pos {
                    true => {
                        let bound = self.inline_variables(bounds.lower.clone(), &vars, pos);
                        conj_ty.inter(bound.inter_with(Ty::var(var)));
                    }

                    false => {
                        let bound = self.inline_variables(bounds.upper.clone(), &vars, pos);
                        conj_ty.inter(bound.inter_with(Ty::var(var)));
                    }
                }
            }

            for var in mem::take(&mut conj.neg.vars) {
                let bounds = &self.bounds[&var];

                if vars.contains(&var) {
                    conj_ty.inter(Ty::var(var));
                    continue;
                }

                let mut vars = vars.clone();
                vars.insert(var);

                match pos {
                    true => {
                        let bound = self.inline_variables(bounds.upper.clone(), &vars, !pos);
                        conj_ty.inter(bound.inter_with(Ty::var(var)).neg());
                    }

                    false => {
                        let bound = self.inline_variables(bounds.lower.clone(), &vars, !pos);
                        conj_ty.inter(bound.inter_with(Ty::var(var)).neg());
                    }
                }
            }

            for mut app in mem::take(&mut conj.pos.apps) {
                for arg in &mut app.args {
                    *arg = self.inline_variables(arg.clone(), vars, pos);
                }

                conj_ty.inter(Ty::app(app.tag, app.args));
            }

            for mut app in mem::take(&mut conj.neg.apps) {
                for arg in &mut app.args {
                    *arg = self.inline_variables(arg.clone(), vars, !pos);
                }

                conj_ty.inter(Ty::app(app.tag, app.args));
            }

            if let Some(ref mut base) = conj.pos.base {
                self.inline_variables_base(base, vars, pos);
            }

            if let Some(ref mut base) = conj.neg.base {
                self.inline_variables_base(base, vars, !pos);
            }

            conj_ty.inter(Ty(vec![conj]));
            new.union(conj_ty);
        }

        new
    }

    fn inline_variables_base(&self, base: &mut Base, vars: &HashSet<Var>, pos: bool) {
        match base {
            Base::Record(fields) => {
                for ty in fields.values_mut() {
                    *ty = self.inline_variables(ty.clone(), vars, pos);
                }
            }

            Base::Tuple(items) => {
                for ty in items {
                    *ty = self.inline_variables(ty.clone(), vars, pos);
                }
            }

            Base::Array(ty) => {
                *ty = self.inline_variables(ty.clone(), vars, pos);
            }

            Base::Func(input, output) => {
                *input = self.inline_variables(input.clone(), vars, !pos);
                *output = self.inline_variables(output.clone(), vars, pos);
            }

            Base::Ref(ty) => {
                *ty = self.inline_variables(ty.clone(), vars, pos);
            }
        }
    }
}
