use std::{cmp::Ordering, collections::HashMap, mem, ops::Neg};

use colored::Colorize;

use crate::solve::SeaHashMap;

use super::{Base, Conj, SeaHashSet, Solver, Term, Ty, Var};

#[repr(u8)]
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum Prec {
    None = 0,
    Func = 1,
    Disj = 2,
    Ref = 3,
    Conj = 4,
    Tuple = 5,
}

impl PartialOrd for Prec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Prec {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as u8).cmp(&(*other as u8))
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum Polarity {
    Pos,
    Neg,
}

impl Polarity {
    fn select<T>(&self, pos: T, neg: T) -> T {
        match self {
            Polarity::Pos => pos,
            Polarity::Neg => neg,
        }
    }
}

impl Neg for Polarity {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Polarity::Pos => Polarity::Neg,
            Polarity::Neg => Polarity::Pos,
        }
    }
}

#[derive(Debug)]
struct VariableInfo {
    /// Number of occurences in positive position.
    positive: Option<usize>,

    /// Number of occurences in negative position.
    negative: Option<usize>,
}

impl Default for VariableInfo {
    fn default() -> Self {
        Self {
            positive: Some(0),
            negative: Some(0),
        }
    }
}

impl VariableInfo {
    fn occurences(&self, polarity: Polarity) -> Option<usize> {
        match polarity {
            Polarity::Pos => self.positive,
            Polarity::Neg => self.negative,
        }
    }
}

impl Solver {
    pub fn format_ty(&mut self, ty: Ty) -> String {
        let seen = SeaHashSet::default();
        let mut info = SeaHashMap::default();
        self.enumerate_variables(&ty, &seen, &mut info, Polarity::Pos);

        let ty = self.simplify(ty);
        let ty = self.inline_bounds(ty, &info, Polarity::Pos);

        let mut vars = self.find_variables(&ty);
        let subst = self.substitute_variables(&mut vars);
        let names = Self::name_variables(&vars, &subst);

        let forall = self.format_forall(&vars, &names);
        let ty = self.format_ty_impl(&ty, &names, Prec::None);
        let where_ = self.format_where(&vars, &names);

        format!("{}{}{}", forall, ty, where_)
    }

    fn format_forall(&self, vars: &SeaHashSet<Var>, names: &HashMap<Var, String>) -> String {
        if vars.is_empty() {
            return String::new();
        }

        let vars: Vec<_> = vars.iter().map(|var| format!("'{}", names[var])).collect();

        let forall = "forall".purple();
        format!("{forall} {}. ", vars.join(","))
    }

    fn format_where(&mut self, vars: &SeaHashSet<Var>, names: &HashMap<Var, String>) -> String {
        let mut bounds = Vec::new();

        for var in vars {
            let var_bounds = self.bounds[var].clone();

            let lower = self.simplify(var_bounds.lower);
            let upper = self.simplify(var_bounds.upper);

            let lower = self.format_ty_impl(&lower, names, Prec::None);
            let upper = self.format_ty_impl(&upper, names, Prec::None);

            let show_lower = lower != "!" && lower != format!("'{}", names[var]);
            let show_upper = upper != "⊤" && upper != format!("'{}", names[var]);

            if !show_lower && !show_upper {
                continue;
            }

            let mut bound = String::new();

            if show_lower {
                bound.push_str(&format!("{} <: ", lower));
            }

            bound.push_str(&format!("'{}", names[var]));

            if show_upper {
                bound.push_str(&format!(" <: {}", upper));
            }

            bounds.push(bound);
        }

        if bounds.is_empty() {
            return String::new();
        }

        let where_ = "where".purple();
        format!(" {where_} {}", bounds.join(", "))
    }

    fn format_ty_impl(&self, Ty(conjs): &Ty, names: &HashMap<Var, String>, p: Prec) -> String {
        if conjs.is_empty() {
            return String::from("!");
        }

        let prec = match conjs.len() {
            1 => p,
            _ => Prec::Conj,
        };

        let mut parts = Vec::new();

        for conj in conjs {
            parts.push(self.format_conj(conj, names, prec));
        }

        match prec < p {
            true => format!("({})", parts.join(" | ")),
            false => parts.join(" | "),
        }
    }

    fn format_conj(&self, conj: &Conj, names: &HashMap<Var, String>, p: Prec) -> String {
        match (conj.pos.is_extreme(), conj.neg.is_extreme()) {
            // top
            (true, true) => String::from("⊤"),
            (false, true) => self.format_term(&conj.pos, names, p, true),
            (true, false) => self.format_term(&conj.neg, names, p, false),
            (false, false) => {
                let pos = self.format_term(&conj.pos, names, Prec::Conj, true);
                let neg = self.format_term(&conj.neg, names, Prec::Conj, false);

                match Prec::Conj < p {
                    true => format!("({} & {})", pos, neg),
                    false => format!("{} & {}", pos, neg),
                }
            }
        }
    }

    fn format_term(&self, term: &Term, names: &HashMap<Var, String>, p: Prec, pos: bool) -> String {
        let mut parts = Vec::new();

        for var in &term.vars {
            parts.push(format!("'{}", names[var]));
        }

        for tag in &term.tags {
            parts.push(tag.to_string());
        }

        for app in &term.apps {
            let mut args = Vec::new();

            for arg in &app.args {
                args.push(self.format_ty_impl(arg, names, Prec::None));
            }

            match args.is_empty() {
                true => parts.push(app.tag.name.to_string()),
                false => parts.push(format!("{}<{}>", app.tag.name, args.join(", "))),
            }
        }

        let prec = match parts.len() {
            0 => p,
            _ => Prec::Conj,
        };

        if let Some(base) = &term.base {
            match base {
                Base::Record(fields) => {
                    let fields: Vec<_> = fields
                        .iter()
                        .map(|(field, ty)| {
                            format!("{}: {}", field, self.format_ty_impl(ty, names, Prec::None))
                        })
                        .collect();

                    parts.push(format!("{{ {} }}", fields.join("; ")));
                }

                Base::Tuple(items) => {
                    let items: Vec<_> = items
                        .iter()
                        .map(|ty| self.format_ty_impl(ty, names, Prec::Tuple))
                        .collect();

                    match Prec::Tuple < prec {
                        true => parts.push(format!("({})", items.join(" * "))),
                        false => parts.push(items.join(" * ")),
                    }
                }

                Base::Array(ty) => {
                    let ty = self.format_ty_impl(ty, names, Prec::None);
                    parts.push(format!("[{}]", ty));
                }

                Base::Func(input, output) => {
                    let input = self.format_ty_impl(input, names, Prec::Func);
                    let output = self.format_ty_impl(output, names, Prec::None);

                    match Prec::Func <= prec {
                        true => parts.push(format!("({} -> {})", input, output)),
                        false => parts.push(format!("{} -> {}", input, output)),
                    }
                }

                Base::Ref(ty) => {
                    let ty = self.format_ty_impl(ty, names, Prec::Ref);

                    match Prec::Ref <= prec {
                        true => parts.push(format!("ref ({})", ty)),
                        false => parts.push(format!("ref {}", ty)),
                    }
                }
            }
        }

        if parts.is_empty() {
            return String::from("⊤");
        }

        if !pos {
            for part in &mut parts {
                *part = format!("~{}", part);
            }
        }

        let prec = match parts.len() {
            1 => p,
            _ => Prec::Conj,
        };

        match prec < p {
            true => format!("({})", parts.join(" & ")),
            false => parts.join(" & "),
        }
    }

    fn name_variables(vars: &SeaHashSet<Var>, subst: &HashMap<Var, Var>) -> HashMap<Var, String> {
        let mut names = HashMap::new();

        for (i, var) in vars.iter().enumerate() {
            names.insert(*var, Self::generate_name(i));
        }

        for (&var, &(mut other)) in subst {
            while let Some(&var) = subst.get(&other) {
                other = var;
            }

            names.insert(var, names[&other].clone());
        }

        names
    }

    fn substitute_variables(&mut self, vars: &mut SeaHashSet<Var>) -> HashMap<Var, Var> {
        let mut subst = HashMap::new();

        for sub in vars.clone() {
            for sup in vars.clone() {
                if sub == sup || !(vars.contains(&sub) && vars.contains(&sup)) {
                    continue;
                }

                let sub_bounds = self.bounds[&sub].clone();
                let sup_bounds = self.bounds[&sup].clone();

                if self.is_subty(&sub_bounds.lower, &sup_bounds.lower)
                    && self.is_subty(&sup_bounds.lower, &sub_bounds.lower)
                    && self.is_subty(&sub_bounds.upper, &sup_bounds.upper)
                    && self.is_subty(&sup_bounds.upper, &sub_bounds.upper)
                {
                    vars.remove(&sub);
                    subst.insert(sub, sup);
                }
            }
        }

        subst
    }

    fn inline_bounds(
        &mut self,
        Ty(ty): Ty,
        info: &SeaHashMap<Var, VariableInfo>,
        polarity: Polarity,
    ) -> Ty {
        let mut new = Ty::never();

        for mut conj in ty {
            let mut conj_ty = Ty::always();

            for var in mem::take(&mut conj.pos.vars) {
                conj_ty.inter(self.inline_bound(var, info, polarity));
            }

            for var in mem::take(&mut conj.neg.vars) {
                conj_ty.inter(self.inline_bound(var, info, -polarity).neg());
            }

            for mut app in mem::take(&mut conj.pos.apps) {
                for arg in &mut app.args {
                    *arg = self.inline_bounds(arg.clone(), info, polarity);
                }

                conj_ty.inter(Ty::app(app.tag, app.args));
            }

            for mut app in mem::take(&mut conj.neg.apps) {
                for arg in &mut app.args {
                    *arg = self.inline_bounds(arg.clone(), info, -polarity);
                }

                conj_ty.inter(Ty::app(app.tag, app.args).neg());
            }

            if let Some(ref mut base) = conj.pos.base {
                self.inline_bounds_base(base, info, polarity);
            }

            if let Some(ref mut base) = conj.neg.base {
                self.inline_bounds_base(base, info, -polarity);
            }

            conj_ty.inter(Ty(vec![conj]));
            new.union(conj_ty);
        }

        new
    }

    fn inline_bound(
        &mut self,
        var: Var,
        infos: &SeaHashMap<Var, VariableInfo>,
        polarity: Polarity,
    ) -> Ty {
        let info = &infos[&var];
        let bounds = &self.bounds[&var];

        if info.occurences(polarity).is_none() {
            return Ty::var(var);
        }

        match polarity {
            Polarity::Pos => {
                let bound = bounds.lower.clone();
                let bound = self.simplify(bound);
                let bound = self.inline_bounds(bound, infos, polarity);

                match info.occurences(-polarity) == Some(0) {
                    false => bound.union_with(Ty::var(var)),
                    true => bound,
                }
            }

            Polarity::Neg => {
                let bound = bounds.upper.clone();
                let bound = self.simplify(bound);
                let bound = self.inline_bounds(bound, infos, polarity);

                match info.occurences(-polarity) == Some(0) {
                    false => bound.inter_with(Ty::var(var)),
                    true => bound,
                }
            }
        }
    }

    fn inline_bounds_base(
        &mut self,
        base: &mut Base,
        info: &SeaHashMap<Var, VariableInfo>,
        polarity: Polarity,
    ) {
        match base {
            Base::Record(fields) => {
                for ty in fields.values_mut() {
                    *ty = self.inline_bounds(ty.clone(), info, polarity);
                }
            }

            Base::Tuple(items) => {
                for ty in items {
                    *ty = self.inline_bounds(ty.clone(), info, polarity);
                }
            }

            Base::Array(ty) => {
                *ty = self.inline_bounds(ty.clone(), info, polarity);
            }

            Base::Func(input, output) => {
                *input = self.inline_bounds(input.clone(), info, -polarity);
                *output = self.inline_bounds(output.clone(), info, polarity);
            }

            Base::Ref(ty) => {
                *ty = self.inline_bounds(ty.clone(), info, polarity);
            }
        }
    }

    pub fn find_variables(&self, ty: &Ty) -> SeaHashSet<Var> {
        let mut vars = SeaHashSet::default();
        self.find_variables_impl(ty, &mut vars);
        vars
    }

    fn find_variables_impl(&self, Ty(conjs): &Ty, vars: &mut SeaHashSet<Var>) {
        for conj in conjs {
            for &var in conj.pos.vars.union(&conj.neg.vars) {
                if !vars.insert(var) {
                    continue;
                }

                let bounds = &self.bounds[&var];

                self.find_variables_impl(&bounds.lower, vars);
                self.find_variables_impl(&bounds.upper, vars);
            }

            for app in conj.pos.apps.union(&conj.neg.apps) {
                for arg in &app.args {
                    self.find_variables_impl(arg, vars);
                }
            }

            if let Some(ref base) = conj.pos.base {
                self.find_variables_base(base, vars);
            }

            if let Some(ref base) = conj.neg.base {
                self.find_variables_base(base, vars);
            }
        }
    }

    fn find_variables_base(&self, base: &Base, vars: &mut SeaHashSet<Var>) {
        match base {
            Base::Record(fields) => {
                for ty in fields.values() {
                    self.find_variables_impl(ty, vars);
                }
            }

            Base::Tuple(items) => {
                for item in items {
                    self.find_variables_impl(item, vars);
                }
            }

            Base::Array(ty) => self.find_variables_impl(ty, vars),

            Base::Func(input, output) => {
                self.find_variables_impl(input, vars);
                self.find_variables_impl(output, vars);
            }

            Base::Ref(ty) => self.find_variables_impl(ty, vars),
        }
    }

    fn enumerate_variables(
        &self,
        Ty(conjs): &Ty,
        seen: &SeaHashSet<Var>,
        info: &mut SeaHashMap<Var, VariableInfo>,
        polarity: Polarity,
    ) {
        for conj in conjs {
            for &var in &conj.pos.vars {
                self.enumerate_variable(var, seen, info, polarity);
            }

            for &var in &conj.neg.vars {
                self.enumerate_variable(var, seen, info, -polarity);
            }

            for app in &conj.pos.apps {
                for arg in &app.args {
                    self.enumerate_variables(arg, seen, info, polarity);
                }
            }

            for app in &conj.neg.apps {
                for arg in &app.args {
                    self.enumerate_variables(arg, seen, info, -polarity);
                }
            }

            if let Some(ref base) = conj.pos.base {
                self.enumerate_variables_base(base, seen, info, polarity);
            }

            if let Some(ref base) = conj.neg.base {
                self.enumerate_variables_base(base, seen, info, -polarity);
            }
        }
    }

    fn enumerate_variables_base(
        &self,
        base: &Base,
        seen: &SeaHashSet<Var>,
        info: &mut SeaHashMap<Var, VariableInfo>,
        polarity: Polarity,
    ) {
        match base {
            Base::Record(fields) => {
                for ty in fields.values() {
                    self.enumerate_variables(ty, seen, info, polarity);
                }
            }

            Base::Tuple(items) => {
                for ty in items {
                    self.enumerate_variables(ty, seen, info, polarity);
                }
            }

            Base::Array(ty) => {
                self.enumerate_variables(ty, seen, info, polarity);
            }

            Base::Func(input, output) => {
                self.enumerate_variables(input, seen, info, -polarity);
                self.enumerate_variables(output, seen, info, polarity);
            }

            Base::Ref(ty) => {
                self.enumerate_variables(ty, seen, info, polarity);
            }
        }
    }

    fn enumerate_variable(
        &self,
        var: Var,
        seen: &SeaHashSet<Var>,
        infos: &mut SeaHashMap<Var, VariableInfo>,
        polarity: Polarity,
    ) {
        let info = infos.entry(var).or_default();

        if seen.contains(&var) {
            match polarity {
                Polarity::Pos => info.positive = None,
                Polarity::Neg => info.negative = None,
            }

            return;
        }

        let mut seen = seen.clone();
        seen.insert(var);

        let bounds = &self.bounds[&var];

        match polarity {
            Polarity::Pos => {
                if let Some(ref mut occurences) = info.positive {
                    *occurences += 1;
                }

                self.enumerate_variables(&bounds.lower, &seen, infos, polarity)
            }
            Polarity::Neg => {
                if let Some(ref mut occurences) = info.negative {
                    *occurences += 1;
                }

                self.enumerate_variables(&bounds.upper, &seen, infos, polarity)
            }
        }
    }

    fn generate_name(index: usize) -> String {
        let chars = "abcdefghijklmnopqrstuvwxyz";

        let mut name = String::new();
        let mut index = index;

        while index > 0 || name.is_empty() {
            let rem = index % chars.len();
            name.push(chars.as_bytes()[rem] as char);
            index /= chars.len();
        }

        name
    }
}
