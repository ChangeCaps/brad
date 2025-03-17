use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

use crate::diagnostic::{Diagnostic, Report, Span};

pub use dnf::*;

mod dnf;
mod format;

pub type Tag = &'static str;
pub type Field = &'static str;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct App {
    pub name: Tag,
    pub args: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    /* algebraic data types */
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    Neg(Box<Ty>),

    /* type constructors */
    Record(BTreeMap<Field, Ty>),
    Tuple(Vec<Ty>),
    Func(Box<Ty>, Box<Ty>),
    List(Box<Ty>),
    Ref(Box<Ty>),
    App(App),
    Tag(Tag),
    Top,
    Bot,

    /* type variables */
    Var(Var),
}

impl Ty {
    pub fn union(lhs: Ty, rhs: Ty) -> Self {
        Ty::Union(Box::new(lhs), Box::new(rhs))
    }

    pub fn inter(lhs: Ty, rhs: Ty) -> Self {
        Ty::Inter(Box::new(lhs), Box::new(rhs))
    }

    pub fn neg(ty: Ty) -> Self {
        Ty::Neg(Box::new(ty))
    }

    pub fn func(input: Ty, output: Ty) -> Self {
        Ty::Func(Box::new(input), Box::new(output))
    }

    pub fn list(ty: Ty) -> Self {
        Ty::List(Box::new(ty))
    }

    pub fn ref_(ty: Ty) -> Self {
        Ty::Ref(Box::new(ty))
    }

    pub fn tuple(tys: impl Into<Vec<Ty>>) -> Self {
        Ty::Tuple(tys.into())
    }

    pub fn record(fields: impl Into<BTreeMap<Field, Ty>>) -> Self {
        Ty::Record(fields.into())
    }

    pub fn app(name: Tag, args: Vec<Ty>) -> Self {
        Ty::App(App { name, args })
    }

    fn subst(&self, map: &HashMap<Ty, Ty>) -> Self {
        if let Some(ty) = map.get(self) {
            return ty.clone();
        }

        match self {
            Ty::Union(t1, t2) => Ty::union(t1.subst(map), t2.subst(map)),
            Ty::Inter(t1, t2) => Ty::inter(t1.subst(map), t2.subst(map)),
            Ty::Neg(ty) => Ty::neg(ty.subst(map)),

            Ty::Func(input, output) => Ty::func(input.subst(map), output.subst(map)),

            Ty::List(ty) => Ty::List(Box::new(ty.subst(map))),
            Ty::Ref(ty) => Ty::Ref(Box::new(ty.subst(map))),

            Ty::Record(fields) => {
                let fields = fields.iter().map(|(f, ty)| (*f, ty.subst(map))).collect();

                Ty::Record(fields)
            }

            Ty::Tuple(tys) => {
                let tys = tys.iter().map(|ty| ty.subst(map)).collect();

                Ty::Tuple(tys)
            }

            Ty::App(app) => {
                let args = app.args.iter().map(|ty| ty.subst(map)).collect();

                Ty::App(App {
                    name: app.name,
                    args,
                })
            }

            Ty::Top | Ty::Bot | Ty::Var(_) | Ty::Tag(_) => self.clone(),
        }
    }

    fn simplify(self) -> Self {
        match self {
            Ty::Union(t1, t2) => {
                let t1 = t1.simplify();
                let t2 = t2.simplify();

                if t1 == t2 {
                    return t1;
                }

                match (t1, t2) {
                    (Ty::Top, _) | (_, Ty::Top) => Ty::Top,
                    (Ty::Bot, ty) | (ty, Ty::Bot) => ty,

                    (t1, t2) => Ty::union(t1, t2),
                }
            }

            Ty::Inter(t1, t2) => {
                let t1 = t1.simplify();
                let t2 = t2.simplify();

                if t1 == t2 {
                    return t1;
                }

                match (t1, t2) {
                    (Ty::Top, ty) | (ty, Ty::Top) => ty,
                    (Ty::Bot, _) | (_, Ty::Bot) => Ty::Bot,

                    (Ty::Union(t1, t2), ty) | (ty, Ty::Union(t1, t2)) => {
                        let t1 = Ty::inter(*t1, ty.clone());
                        let t2 = Ty::inter(*t2, ty);

                        Ty::union(t1, t2).simplify()
                    }

                    (t1, t2) => Ty::inter(t1, t2),
                }
            }

            Ty::Neg(ty) => match *ty {
                Ty::Union(t1, t2) => {
                    let t1 = Ty::Neg(t1);
                    let t2 = Ty::Neg(t2);

                    Ty::inter(t1, t2).simplify()
                }

                Ty::Inter(t1, t2) => {
                    let t1 = Ty::Neg(t1);
                    let t2 = Ty::Neg(t2);

                    Ty::union(t1, t2).simplify()
                }

                Ty::Neg(ty) => *ty,
                Ty::Top => Ty::Bot,
                Ty::Bot => Ty::Top,

                Ty::Tag(_)
                | Ty::Record(_)
                | Ty::Tuple(_)
                | Ty::Func(_, _)
                | Ty::List(_)
                | Ty::Ref(_)
                | Ty::App(_)
                | Ty::Var(_) => Self::Neg(ty),
            },

            Ty::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(f, ty)| (f, ty.simplify()))
                    .collect();

                Ty::Record(fields)
            }

            Ty::Tuple(tys) => {
                let tys = tys.into_iter().map(|ty| ty.simplify()).collect();

                Ty::Tuple(tys)
            }

            Ty::Func(input, ouput) => {
                let input = input.simplify();
                let ouput = ouput.simplify();

                Ty::func(input, ouput)
            }

            Ty::List(ty) => Ty::list(ty.simplify()),
            Ty::Ref(ty) => Ty::Ref(Box::new(ty.simplify())),

            Ty::App(app) => Ty::App(App {
                name: app.name,
                args: app.args.into_iter().map(|ty| ty.simplify()).collect(),
            }),

            Ty::Tag(_) | Ty::Top | Ty::Bot | Ty::Var(_) => self,
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Union(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
            Ty::Inter(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            Ty::Neg(ty) => write!(f, "~{}", ty),
            Ty::Func(lhs, rhs) => write!(f, "({} -> {})", lhs, rhs),
            Ty::List(ty) => write!(f, "[{}]", ty),
            Ty::Ref(ty) => write!(f, "ref {}", ty),

            Ty::Top => write!(f, "⊤"),
            Ty::Bot => write!(f, "⊥"),
            Ty::Var(idx) => write!(f, "'{}", idx.index),

            Ty::Tag(tag) => write!(f, "{}", tag),

            Ty::Record(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|(ident, ty)| format!("{}: {}", ident, ty))
                    .collect();

                write!(f, "{{{}}}", fields.join(", "))
            }

            Ty::Tuple(tys) => {
                let tys: Vec<_> = tys.iter().map(|ty| ty.to_string()).collect();

                write!(f, "({})", tys.join(", "))
            }

            Ty::App(app) => {
                let args: Vec<_> = app.args.iter().map(|ty| ty.to_string()).collect();

                write!(f, "{}<{}>", app.name, args.join(", "))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Options {
    pub simplify_normal_forms: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            simplify_normal_forms: true,
        }
    }
}

#[derive(Debug)]
pub struct Solver {
    options: Options,
    variables: HashMap<usize, Variable>,
    applicables: HashMap<Tag, (Ty, Vec<Ty>)>,
    cache: HashMap<(Ty, Ty), bool>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    index: usize,
}

/// Constraints for a type variable.
#[derive(Clone, Debug)]
pub struct Variable {
    pub lbs: Vec<Ty>,
    pub ubs: Vec<Ty>,
}

impl Solver {
    pub fn new(options: Options) -> Self {
        Self {
            options,
            variables: HashMap::new(),
            applicables: HashMap::new(),
            cache: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn fresh_var(&mut self) -> Var {
        let index = self.variables.len();

        self.variables.insert(
            index,
            Variable {
                lbs: Vec::new(),
                ubs: Vec::new(),
            },
        );

        Var { index }
    }

    /// Add a type body and arguments of an applicable type.
    pub fn add_applicable(&mut self, tag: Tag, body: Ty, args: Vec<Ty>) {
        self.applicables.insert(tag, (body, args));
    }

    /// Get the arguments of an applicable type.
    pub fn applicable_args(&self, tag: Tag) -> Option<&Vec<Ty>> {
        self.applicables.get(tag).map(|(_, args)| args)
    }

    pub fn var(&mut self, var: Var) -> &mut Variable {
        self.variables.get_mut(&var.index).unwrap()
    }

    pub fn finish(self) -> Result<(), Report> {
        if self.diagnostics.is_empty() {
            Ok(())
        } else {
            Err(Report::from(self.diagnostics))
        }
    }

    pub fn instance(&mut self, ty: &Ty) -> Ty {
        self.inst(&mut HashMap::new(), ty)
    }

    fn inst(&mut self, map: &mut HashMap<Var, Var>, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(var) => {
                if let Some(&var) = map.get(var) {
                    return Ty::Var(var);
                }

                let new_var = self.fresh_var();
                map.insert(*var, new_var);

                let var = self.var(*var).clone();

                let lbs = var.lbs.iter().map(|ty| self.inst(map, ty)).collect();
                let ubs = var.ubs.iter().map(|ty| self.inst(map, ty)).collect();

                let var = self.var(new_var);

                var.lbs = lbs;
                var.ubs = ubs;

                Ty::Var(new_var)
            }

            Ty::Union(t1, t2) => Ty::union(self.inst(map, t1), self.inst(map, t2)),
            Ty::Inter(t1, t2) => Ty::inter(self.inst(map, t1), self.inst(map, t2)),
            Ty::Neg(ty) => Ty::neg(self.inst(map, ty)),

            Ty::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(f, ty)| (*f, self.inst(map, ty)))
                    .collect();

                Ty::Record(fields)
            }

            Ty::Tuple(tys) => {
                let tys = tys.iter().map(|ty| self.inst(map, ty)).collect();

                Ty::Tuple(tys)
            }

            Ty::Func(input, output) => {
                let input = self.inst(map, input);
                let output = self.inst(map, output);

                Ty::func(input, output)
            }

            Ty::List(ty) => Ty::list(self.inst(map, ty)),
            Ty::Ref(ty) => Ty::Ref(Box::new(self.inst(map, ty))),

            Ty::App(app) => {
                let args = app.args.iter().map(|ty| self.inst(map, ty)).collect();

                Ty::App(App {
                    name: app.name,
                    args,
                })
            }

            Ty::Top | Ty::Bot | Ty::Tag(_) => ty.clone(),
        }
    }

    pub fn subty(&mut self, lhs: &Ty, rhs: &Ty, span: Span) {
        let lhs = lhs.clone().simplify();
        let rhs = rhs.clone().simplify();

        let key = (lhs.clone(), rhs.clone());

        if self.cache.contains_key(&key) {
            return;
        }

        self.cache.insert(key.clone(), false);

        if let Err(err) = self.constrain(&lhs, &rhs, span) {
            self.diagnostics.push(err);
            self.cache.remove(&key);
        }

        self.cache.insert(key.clone(), true);
    }

    fn expand(&self, app: &App) -> Ty {
        let (body, args) = self.applicables.get(&app.name).unwrap();

        if args.len() != app.args.len() {
            panic!(
                "expected {} arguments, found {}",
                args.len(),
                app.args.len()
            );
        }

        let mut map = HashMap::new();

        for (arg, ty) in args.iter().zip(app.args.iter()) {
            map.insert(arg.clone(), ty.clone());
        }

        body.subst(&map)
    }

    fn constrain(&mut self, lhs: &Ty, rhs: &Ty, span: Span) -> Result<(), Diagnostic> {
        let lhs = self.simplify_dnf(self.dnf(lhs));
        let rhs = self.simplify_cnf(self.cnf(rhs));

        // (a & ~b) | (c & ~d) | .. <: (~e | f) & (~g | h) & ..
        //
        // (a & ~b) <: (~e | f) & (~g | h) & ..
        // (c & ~d) <: (~e | f) & (~g | h) & ..
        // ..
        //
        // a & ~b <: ~e | f
        // a & ~b <: ~g | h
        // c & ~d <: ~e | f
        // c & ~d <: ~g | h
        // ..
        //
        // a | e <: b & f
        // a | g <: b & h
        // c | e <: d & f
        // c | g <: d & h
        // ..

        for l in lhs.0.clone() {
            'out: for r in rhs.0.clone() {
                let Conjunct {
                    mut lnf,
                    mut vars,
                    mut rnf,
                    mut nvars,
                } = self.inter_conjunct(l.clone(), r.clone().neg());

                // lnf & vars <: rnf & nvars

                if let Some(&var) = vars.iter().next() {
                    vars.remove(&var);

                    let dis = Conjunct {
                        lnf,
                        vars,
                        rnf,
                        nvars,
                    }
                    .neg()
                    .to_ty();

                    let bounds = self.var(var);
                    bounds.ubs.push(dis.clone());

                    let bounds = self.var(var);

                    for lb in bounds.lbs.clone() {
                        self.subty(&lb, &dis, span);
                    }

                    continue;
                }

                if let Some(&var) = nvars.iter().next() {
                    nvars.remove(&var);

                    let dis = Conjunct {
                        lnf,
                        vars,
                        rnf,
                        nvars,
                    }
                    .to_ty();

                    let bounds = self.var(var);
                    bounds.lbs.push(dis.clone());

                    let var = self.var(var);

                    for ub in var.ubs.clone() {
                        self.subty(&dis, &ub, span);
                    }

                    continue;
                }

                if lnf.is_bot() || rnf.is_top() {
                    continue;
                }

                if lnf.is_top() || rnf.is_bot() {
                    let diagnostic = Diagnostic::error("type::error")
                        .message(format!(
                            "type constraint `{} <: {}` is unsatisfiable",
                            self.format_ty(&lnf.to_ty()),
                            self.format_ty(&rnf.to_ty()),
                        ))
                        .note(format!(
                            "required for `{} <: {}` to hold",
                            self.format_ty(&l.to_ty()),
                            self.format_ty(&r.to_ty()),
                        ))
                        .note(format!(
                            "required for `{} <: {}` to hold",
                            self.format_ty(&lhs.to_ty()),
                            self.format_ty(&rhs.to_ty()),
                        ))
                        .label(span, "originated here");

                    return Err(diagnostic);
                }

                let Lnf::Base {
                    tags: ref lt,
                    apps: ref mut la,
                    base: ref lb,
                } = lnf
                else {
                    unreachable!("expected base, found {:?}", lnf);
                };

                let Rnf::Base {
                    tags: ref rt,
                    apps: ref mut ra,
                    base: ref rb,
                } = rnf
                else {
                    unreachable!("expected base, found {:?}", rnf);
                };

                if let Some(app) = la.first().cloned() {
                    la.remove(&app);

                    // this should not need to be here, but for whatever reason it does
                    // frankly, it makes me want to puke
                    //  - @hjalte 2025-03-16
                    if lnf.is_bot() {
                        lnf = Lnf::Top;
                    }

                    let rhs = Ty::union(Ty::neg(lnf.to_ty()), rnf.to_ty());
                    let lhs = self.expand(&app);

                    self.subty(&lhs, &rhs, span);

                    continue;
                }

                if let Some(app) = ra.first().cloned() {
                    ra.remove(&app);

                    // this should not need to be here, but for whatever reason it does
                    // frankly, it makes me want to puke
                    //  - @hjalte 2025-03-16
                    if rnf.is_top() {
                        rnf = Rnf::Bot;
                    }

                    let lhs = Ty::inter(lnf.to_ty(), Ty::neg(rnf.to_ty()));
                    let rhs = self.expand(&app);

                    self.subty(&lhs, &rhs, span);

                    continue;
                }

                for rn in rt {
                    if lt.contains(rn) {
                        continue 'out;
                    }
                }

                match (lb, rb) {
                    (LnfBase::Func(li, lo), RnfBase::Func(ri, ro)) => {
                        self.subty(ri, li, span);
                        self.subty(lo, ro, span);

                        continue;
                    }

                    (LnfBase::Record(lf), RnfBase::Field(rf, rt)) => match lf.get(rf) {
                        Some(lt) => {
                            self.subty(lt, rt, span);
                            continue;
                        }

                        None => {
                            let fields = lf.keys().map(|f| format!("`{}`", f)).collect::<Vec<_>>();

                            let diagnostic = Diagnostic::error("type::error")
                                .message(format!("expected field `{}` in record", rf))
                                .note(format!("available fields are: {}", fields.join(", ")))
                                .note(format!(
                                    "required for `{} <: {}` to hold",
                                    self.format_ty(&lnf.to_ty()),
                                    self.format_ty(&rnf.to_ty()),
                                ))
                                .note(format!(
                                    "required for `{} <: {}` to hold",
                                    self.format_ty(&lhs.to_ty()),
                                    self.format_ty(&rhs.to_ty()),
                                ))
                                .label(span, "originated here");

                            return Err(diagnostic);
                        }
                    },

                    (LnfBase::Tuple(lt), RnfBase::Tuple(rt)) if lt.len() == rt.len() => {
                        for (l, r) in lt.iter().zip(rt.iter()) {
                            self.subty(l, r, span);
                        }

                        continue;
                    }

                    (LnfBase::Tuple(lt), RnfBase::Tuple(rt)) => {
                        let diagnostic = Diagnostic::error("type::error")
                            .message(format!(
                                "expected tuple of length `{}`, found tuple of length `{}`",
                                lt.len(),
                                rt.len()
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lnf.to_ty()),
                                self.format_ty(&rnf.to_ty()),
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lhs.to_ty()),
                                self.format_ty(&rhs.to_ty()),
                            ))
                            .label(span, "originated here");

                        return Err(diagnostic);
                    }

                    (LnfBase::List(lt), RnfBase::List(rt)) => {
                        self.subty(lt, rt, span);
                        continue;
                    }

                    (LnfBase::Ref(lt), RnfBase::Ref(rt)) => {
                        self.subty(lt, rt, span);
                        continue;
                    }

                    (lb, rb) => {
                        let lkind = match lb {
                            LnfBase::None => lnf.to_ty().to_string(),
                            LnfBase::Record(_) => String::from("record"),
                            LnfBase::Tuple(_) => String::from("tuple"),
                            LnfBase::Func(_, _) => String::from("function"),
                            LnfBase::List(_) => String::from("list"),
                            LnfBase::Ref(_) => String::from("reference"),
                        };

                        let rkind = match rb {
                            RnfBase::None => rnf.to_ty().to_string(),
                            RnfBase::Field(_, _) => String::from("record"),
                            RnfBase::Func(_, _) => String::from("function"),
                            RnfBase::Tuple(_) => String::from("tuple"),
                            RnfBase::List(_) => String::from("list"),
                            RnfBase::Ref(_) => String::from("reference"),
                        };

                        let diagnostic = Diagnostic::error("type::error")
                            .message(format!("`{}` does not subtype `{}`", lkind, rkind))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lnf.to_ty()),
                                self.format_ty(&rnf.to_ty()),
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&l.to_ty()),
                                self.format_ty(&r.to_ty()),
                            ))
                            .note(format!(
                                "required for `{} <: {}` to hold",
                                self.format_ty(&lhs.to_ty()),
                                self.format_ty(&rhs.to_ty()),
                            ))
                            .label(span, "originated here");

                        return Err(diagnostic);
                    }
                }
            }
        }

        Ok(())
    }
}
