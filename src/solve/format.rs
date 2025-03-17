use std::{
    collections::{HashMap, HashSet},
    io::{self, Write},
};

use super::{Solver, Ty, Var};

impl Solver {
    pub fn format_ty(&self, ty: &Ty) -> String {
        let mut buf = Vec::new();
        let mut vars = HashMap::new();

        self.format_ty_inner(
            &mut buf,
            ty,
            &mut vars,       // vars
            &HashSet::new(), // seen vars
            0,
        )
        .unwrap();

        let ty = String::from_utf8_lossy(&buf);

        match vars.is_empty() {
            true => ty.to_string(),
            false => {
                let vars = vars
                    .into_values()
                    .map(|name| format!("'{name}"))
                    .collect::<Vec<_>>();

                format!("forall {}. {}", vars.join(","), ty)
            }
        }
    }

    fn format_ty_inner(
        &self,
        w: &mut dyn Write,
        ty: &Ty,
        vars: &mut HashMap<Var, String>,
        seen: &HashSet<Var>,
        prec: usize,
    ) -> io::Result<()> {
        if self.prec(ty) < prec {
            write!(w, "(")?;
            self.format_ty_inner(w, ty, vars, seen, self.prec(ty))?;
            write!(w, ")")?;
            return Ok(());
        }

        match ty {
            Ty::Union(t1, t2) => {
                self.format_ty_inner(w, t1, vars, seen, self.prec(ty))?;
                write!(w, " | ")?;
                self.format_ty_inner(w, t2, vars, seen, self.prec(ty))?;
            }

            Ty::Inter(t1, t2) => {
                self.format_ty_inner(w, t1, vars, seen, self.prec(ty))?;
                write!(w, " & ")?;
                self.format_ty_inner(w, t2, vars, seen, self.prec(ty))?;
            }

            Ty::Neg(inner) => {
                write!(w, "~")?;
                self.format_ty_inner(w, inner, vars, seen, self.prec(ty))?;
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

                    self.format_ty_inner(w, ty, vars, seen, 0)?;
                }

                write!(w, " }}")?;
            }

            Ty::Tuple(fields) => {
                for (i, ty) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(w, " * ")?;
                    }

                    self.format_ty_inner(w, ty, vars, seen, 0)?;
                }
            }

            Ty::Func(input, output) => {
                self.format_ty_inner(w, input, vars, seen, self.prec(ty) + 1)?;
                write!(w, " -> ")?;
                self.format_ty_inner(w, output, vars, seen, self.prec(ty))?;
            }

            Ty::List(inner) => {
                write!(w, "[")?;
                self.format_ty_inner(w, inner, vars, seen, 0)?;
                write!(w, "]")?;
            }

            Ty::Ref(inner) => {
                write!(w, "ref ")?;
                self.format_ty_inner(w, inner, vars, seen, self.prec(ty))?;
            }

            Ty::App(app) => {
                write!(w, "{}", app.name)?;

                if !app.args.is_empty() {
                    write!(w, "<")?;

                    for (i, arg) in app.args.iter().enumerate() {
                        if i > 0 {
                            write!(w, ", ")?;
                        }

                        self.format_ty_inner(w, arg, vars, seen, 0)?;
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
                let mut cons = self.variables.get(&var.index).unwrap().clone();

                if seen.contains(var) {
                    return write!(w, "...");
                }

                let mut seen = seen.clone();
                seen.insert(*var);

                cons.lbs.retain(|lb| lb != ty);
                cons.ubs.retain(|ub| ub != ty);

                if !cons.lbs.is_empty() {
                    let lb = cons.lbs.iter().cloned().fold(Ty::Bot, Ty::union);
                    let lb = self.simplify_dnf(self.dnf(&lb)).to_ty().simplify();

                    return self.format_ty_inner(w, &lb, vars, &seen, prec);
                }

                if !cons.ubs.is_empty() {
                    let ub = cons.ubs.iter().cloned().fold(Ty::Top, Ty::inter);
                    let ub = self.simplify_cnf(self.cnf(&ub)).to_ty().simplify();

                    return self.format_ty_inner(w, &ub, vars, &seen, prec);
                }

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
