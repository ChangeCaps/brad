use std::{
    collections::{HashMap, HashSet},
    io::{self, Write},
    ops::Neg,
};

use super::{App, Solver, Ty, Var};

#[derive(Clone, Copy, PartialEq)]
enum Pol {
    Pos,
    Neg,
}

impl Neg for Pol {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Pol::Pos => Pol::Neg,
            Pol::Neg => Pol::Pos,
        }
    }
}

impl Solver {
    pub fn format_ty(&self, ty: &Ty) -> String {
        let ty = self.inline_vars(&HashSet::new(), ty, Pol::Pos);
        let ty = self.simplify(&ty);
        let ty = self.remove_duplicates(ty);

        let mut vars = HashMap::new();
        let ty = self.format_ty_impl(&ty, &mut vars);

        match vars.is_empty() {
            true => ty,
            false => {
                let where_bounds = self.format_where_bounds(&mut vars);

                let mut vars = vars
                    .into_values()
                    .map(|name| format!("'{name}"))
                    .collect::<Vec<_>>();

                vars.sort();

                if where_bounds.is_empty() {
                    format!("forall {}. {}", vars.join(","), ty)
                } else {
                    format!("forall {}. {} where {}", vars.join(","), ty, where_bounds)
                }
            }
        }
    }

    fn format_where_bounds(&self, vars: &mut HashMap<Var, String>) -> String {
        let mut where_bounds = Vec::new();
        let var_len = vars.len();

        for (var, name) in vars.clone() {
            let bounds = self.bounds.get(&var).unwrap();

            let lb = bounds.lbs.iter().cloned().fold(Ty::Bot, Ty::union);
            let ub = bounds.ubs.iter().cloned().fold(Ty::Top, Ty::inter);

            let lb = self.simplify(&lb);
            let ub = self.simplify(&ub);

            let lb = self.remove_duplicates(lb);
            let ub = self.remove_duplicates(ub);

            let show_lb = lb != Ty::Bot && !self.is_equal(&lb, &Ty::Var(var));
            let show_ub = ub != Ty::Top && !self.is_equal(&ub, &Ty::Var(var));

            if !show_lb && !show_ub {
                continue;
            }

            let mut bounds = String::new();

            if show_lb {
                let ty = self.format_ty_impl(&lb, vars);
                bounds.push_str(&format!("{} <: ", ty));
            }

            bounds.push('\'');
            bounds.push_str(&name);
            if show_ub {
                let ty = self.format_ty_impl(&ub, vars);
                bounds.push_str(&format!(" <: {}", ty));
            }

            where_bounds.push(bounds);
        }

        if vars.len() != var_len {
            // FIXME: this is a hack
            return self.format_where_bounds(vars);
        }

        where_bounds.into_iter().collect::<Vec<_>>().join(", ")
    }

    fn remove_duplicates(&self, ty: Ty) -> Ty {
        let vars = Self::get_vars(&ty);
        let mut map = HashMap::new();

        for var in &vars {
            for other in &vars {
                if other == var {
                    continue;
                }

                let v1 = Ty::Var(*var);
                let v2 = Ty::Var(*other);

                if self.is_equal(&v1, &v2) && !map.contains_key(&v1) && !map.contains_key(&v2) {
                    map.insert(v1, v2);
                }
            }
        }

        ty.subst(&map)
    }

    fn get_vars(ty: &Ty) -> HashSet<Var> {
        let mut vars = HashSet::new();

        ty.visit(|ty| {
            if let Ty::Var(var) = ty {
                vars.insert(*var);
            }
        });

        vars
    }

    fn inline_vars(&self, seen: &HashSet<Var>, ty: &Ty, pol: Pol) -> Ty {
        match ty {
            Ty::Union(t1, t2) => Ty::union(
                self.inline_vars(seen, t1, pol),
                self.inline_vars(seen, t2, pol),
            ),
            Ty::Inter(t1, t2) => Ty::inter(
                self.inline_vars(seen, t1, pol),
                self.inline_vars(seen, t2, pol),
            ),

            Ty::Neg(ty) => Ty::neg(self.inline_vars(seen, ty, -pol)),

            Ty::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| (*name, self.inline_vars(seen, ty, pol)))
                    .collect();

                Ty::Record(fields)
            }

            Ty::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|item| self.inline_vars(seen, item, pol))
                    .collect();

                Ty::Tuple(items)
            }

            Ty::Func(input, output) => {
                let input = self.inline_vars(seen, input, -pol);
                let output = self.inline_vars(seen, output, pol);

                Ty::func(input, output)
            }

            Ty::List(ty) => self.inline_vars(seen, ty, pol),

            Ty::Ref(ty) => self.inline_vars(seen, ty, pol),

            Ty::App(app) => {
                let args = app
                    .args
                    .iter()
                    .map(|arg| self.inline_vars(seen, arg, pol))
                    .collect();

                Ty::App(App {
                    name: app.name,
                    args,
                })
            }

            Ty::Var(var) => {
                // if the variable is already seen, that means we are in a recursive type
                // and we should not inline the variable
                if seen.contains(var) {
                    return ty.clone();
                }

                let mut seen = seen.clone();
                seen.insert(*var);

                let bounds = self.bounds.get(var).unwrap();

                let lb = bounds.lbs.iter().cloned().fold(Ty::Bot, Ty::union);
                let ub = bounds.ubs.iter().cloned().fold(Ty::Top, Ty::inter);

                if !bounds.lbs.is_empty() && pol == Pol::Pos {
                    let bound = self.inline_vars(&seen, &lb, pol);
                    return Ty::union(Ty::Var(*var), bound);
                }

                if !bounds.ubs.is_empty() && pol == Pol::Neg {
                    let bound = self.inline_vars(&seen, &ub, pol);
                    return bound;
                }

                Ty::Var(*var)
            }

            Ty::Tag(_) | Ty::Top | Ty::Bot => ty.clone(),
        }
    }

    fn format_ty_impl(&self, ty: &Ty, vars: &mut HashMap<Var, String>) -> String {
        let mut buf = Vec::new();

        self.write_ty_impl(&mut buf, ty, vars, 0).unwrap();

        String::from_utf8_lossy(&buf).to_string()
    }

    fn write_ty_impl(
        &self,
        w: &mut dyn Write,
        ty: &Ty,
        vars: &mut HashMap<Var, String>,
        prec: usize,
    ) -> io::Result<()> {
        if self.prec(ty) < prec {
            write!(w, "(")?;
            self.write_ty_impl(w, ty, vars, self.prec(ty))?;
            write!(w, ")")?;
            return Ok(());
        }

        match ty {
            Ty::Union(t1, t2) => {
                self.write_ty_impl(w, t1, vars, self.prec(ty))?;
                write!(w, " | ")?;
                self.write_ty_impl(w, t2, vars, self.prec(ty))?;
            }

            Ty::Inter(t1, t2) => {
                self.write_ty_impl(w, t1, vars, self.prec(ty))?;
                write!(w, " & ")?;
                self.write_ty_impl(w, t2, vars, self.prec(ty))?;
            }

            Ty::Neg(inner) => {
                write!(w, "~")?;
                self.write_ty_impl(w, inner, vars, self.prec(ty))?;
            }

            Ty::Tag(tag) => {
                write!(w, "#{}", tag)?;
            }

            Ty::Record(fields) => {
                if fields.is_empty() {
                    return write!(w, "{{}}");
                }

                write!(w, "{{ ")?;

                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }

                    write!(w, "{}: ", name)?;

                    self.write_ty_impl(w, ty, vars, 0)?;
                }

                write!(w, " }}")?;
            }

            Ty::Tuple(fields) => {
                for (i, ty) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(w, " * ")?;
                    }

                    self.write_ty_impl(w, ty, vars, 0)?;
                }
            }

            Ty::Func(input, output) => {
                self.write_ty_impl(w, input, vars, self.prec(ty) + 1)?;
                write!(w, " -> ")?;
                self.write_ty_impl(w, output, vars, self.prec(ty))?;
            }

            Ty::List(inner) => {
                write!(w, "[")?;
                self.write_ty_impl(w, inner, vars, 0)?;
                write!(w, "]")?;
            }

            Ty::Ref(inner) => {
                write!(w, "ref ")?;
                self.write_ty_impl(w, inner, vars, self.prec(ty))?;
            }

            Ty::App(app) => {
                write!(w, "{}", app.name)?;

                if !app.args.is_empty() {
                    write!(w, "<")?;

                    for (i, arg) in app.args.iter().enumerate() {
                        if i > 0 {
                            write!(w, ", ")?;
                        }

                        self.write_ty_impl(w, arg, vars, 0)?;
                    }

                    write!(w, ">")?;
                }
            }

            Ty::Top => {
                write!(w, "⊤")?;
            }

            Ty::Bot => {
                write!(w, "!")?;
            }

            Ty::Var(var) => {
                if let Some(name) = vars.get(var) {
                    return write!(w, "'{}", name);
                }

                let name = Self::generate_name(vars.len());
                vars.insert(*var, name.clone());

                write!(w, "'{}", name)?;
            }
        }

        Ok(())
    }

    fn prec(&self, ty: &Ty) -> usize {
        match ty {
            Ty::Tag(_)
            | Ty::Record(_)
            | Ty::List(_)
            | Ty::App(_)
            | Ty::Top
            | Ty::Bot
            | Ty::Var(_) => 6,

            Ty::Neg(_) => 5,
            Ty::Ref(_) => 4,
            Ty::Inter(_, _) => 3,
            Ty::Union(_, _) => 2,
            Ty::Func(_, _) => 1,
            Ty::Tuple(_) => 0,
        }
    }

    fn generate_name(mut index: usize) -> String {
        let letters = "abcdefghijklmnopqrstuvwxyz";
        let letters = letters.as_bytes();

        let mut name = String::new();

        while index >= letters.len() {
            let letter = letters[index % letters.len()] as char;
            index /= letters.len();
            index -= 1;
            name.push(letter);
        }

        name.push(letters[index] as char);

        name
    }
}
