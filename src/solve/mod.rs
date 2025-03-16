use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

use crate::diagnostic::{Diagnostic, Span};

pub use dnf::*;

mod dnf;
mod format;

pub type Name = &'static str;
pub type Field = &'static str;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct App {
    pub name: Name,
    pub args: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    /* algebraic data types */
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    Neg(Box<Ty>),

    /* type constructors */
    Name(Name),
    Record(BTreeMap<Field, Ty>),
    Tuple(Vec<Ty>),
    Func(Box<Ty>, Box<Ty>),
    App(App),
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

    pub fn tuple(tys: impl Into<Vec<Ty>>) -> Self {
        Ty::Tuple(tys.into())
    }

    pub fn record(fields: impl Into<BTreeMap<Field, Ty>>) -> Self {
        Ty::Record(fields.into())
    }

    pub fn app(name: Name, args: Vec<Ty>) -> Self {
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

            Ty::Top | Ty::Bot | Ty::Var(_) | Ty::Name(_) => self.clone(),
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

                Ty::Name(_)
                | Ty::Record(_)
                | Ty::Tuple(_)
                | Ty::Func(_, _)
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

            Ty::App(app) => Ty::App(App {
                name: app.name,
                args: app.args.into_iter().map(|ty| ty.simplify()).collect(),
            }),

            Ty::Name(_) | Ty::Top | Ty::Bot | Ty::Var(_) => self,
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

            Ty::Top => write!(f, "⊤"),
            Ty::Bot => write!(f, "⊥"),
            Ty::Var(idx) => write!(f, "'{}", idx.index),

            Ty::Name(name) => write!(f, "{}", name),

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

#[derive(Clone, Debug)]
pub struct Solver {
    variables: HashMap<usize, Variable>,
    applicables: HashMap<Name, (Ty, Vec<Ty>)>,
    cache: HashMap<(Ty, Ty), bool>,
    options: Options,
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
            variables: HashMap::new(),
            applicables: HashMap::new(),
            cache: HashMap::new(),
            options,
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
    pub fn add_applicable(&mut self, name: Name, body: Ty, args: Vec<Ty>) {
        self.applicables.insert(name, (body, args));
    }

    /// Get the arguments of an applicable type.
    pub fn applicable_args(&self, name: Name) -> Option<&Vec<Ty>> {
        self.applicables.get(name).map(|(_, args)| args)
    }

    pub fn var(&mut self, var: Var) -> &mut Variable {
        self.variables.get_mut(&var.index).unwrap()
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

            Ty::App(app) => {
                let args = app.args.iter().map(|ty| self.inst(map, ty)).collect();

                Ty::App(App {
                    name: app.name,
                    args,
                })
            }

            Ty::Top | Ty::Bot | Ty::Name(_) => ty.clone(),
        }
    }

    pub fn subty(&mut self, lhs: &Ty, rhs: &Ty, span: Span) -> Result<(), Diagnostic> {
        let lhs = lhs.clone().simplify();
        let rhs = rhs.clone().simplify();

        let key = (lhs.clone(), rhs.clone());

        if self.cache.contains_key(&key) {
            return Ok(());
        }

        self.cache.insert((lhs.clone(), rhs.clone()), false);

        self.constrain(&lhs, &rhs, span)?;

        self.cache.insert((lhs.clone(), rhs.clone()), true);

        Ok(())
    }

    fn expand(&self, app: &App) -> Result<Ty, Diagnostic> {
        let (body, args) = self.applicables.get(&app.name).unwrap();

        if args.len() != app.args.len() {
            let diagnostic = Diagnostic::error("type::error").message(format!(
                "expected {} arguments, found {}",
                args.len(),
                app.args.len()
            ));

            return Err(diagnostic);
        }

        let mut map = HashMap::new();

        for (arg, ty) in args.iter().zip(app.args.iter()) {
            map.insert(arg.clone(), ty.clone());
        }

        Ok(body.subst(&map))
    }

    fn constrain(&mut self, lhs: &Ty, rhs: &Ty, span: Span) -> Result<(), Diagnostic> {
        let lhs = self.simplify_dnf(self.dnf(lhs));
        let rhs = self.simplify_cnf(self.cnf(rhs));

        for l in lhs.0.clone() {
            'out: for r in rhs.0.clone() {
                let Conjunct {
                    mut lnf,
                    mut vars,
                    mut rnf,
                    mut nvars,
                } = self.inter_conjunct(l.clone(), r.clone().neg());

                if let Some(&name) = vars.iter().next() {
                    vars.remove(&name);

                    let dis = Conjunct {
                        lnf,
                        vars,
                        rnf,
                        nvars,
                    }
                    .neg()
                    .to_ty();

                    let var = self.var(name);
                    var.ubs.push(dis.clone());

                    let var = self.var(name).clone();

                    for lb in var.lbs.clone() {
                        self.subty(&lb, &dis, span)?;
                    }

                    continue;
                }

                if let Some(&name) = nvars.iter().next() {
                    nvars.remove(&name);

                    let dis = Conjunct {
                        lnf,
                        vars,
                        rnf,
                        nvars,
                    }
                    .to_ty();

                    let var = self.var(name);
                    var.lbs.push(dis.clone());

                    let var = self.var(name).clone();

                    for ub in var.ubs.clone() {
                        self.subty(&dis, &ub, span)?;
                    }

                    continue;
                }

                if rnf.is_top() || lnf.is_bot() {
                    continue;
                }

                let Lnf::Base {
                    names: ref ln,
                    apps: ref mut la,
                    base: ref lb,
                } = lnf
                else {
                    unreachable!();
                };

                let Rnf::Base {
                    names: ref rn,
                    apps: ref mut ra,
                    base: ref rb,
                } = rnf
                else {
                    unreachable!();
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
                    let lhs = self.expand(&app)?;

                    self.subty(&lhs, &rhs, span)?;

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
                    let rhs = self.expand(&app)?;

                    self.subty(&lhs, &rhs, span)?;

                    continue;
                }

                for rn in rn {
                    if ln.contains(rn) {
                        continue 'out;
                    }
                }

                match (lb, rb) {
                    (LnfBase::Func(li, lo), RnfBase::Func(ri, ro)) => {
                        self.subty(ri, li, span)?;
                        self.subty(lo, ro, span)?;

                        continue;
                    }

                    (LnfBase::Record(lf), RnfBase::Field(rf, rt)) => match lf.get(rf) {
                        Some(lt) => {
                            self.subty(lt, rt, span)?;
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
                            self.subty(l, r, span)?;
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

                    (LnfBase::None, _) | (_, RnfBase::None) => {}

                    (lb, rb) => {
                        let lkind = match lb {
                            LnfBase::None => lnf.to_ty().to_string(),
                            LnfBase::Record(_) => String::from("record"),
                            LnfBase::Func(_, _) => String::from("function"),
                            LnfBase::Tuple(_) => String::from("tuple"),
                        };

                        let rkind = match rb {
                            RnfBase::None => rnf.to_ty().to_string(),
                            RnfBase::Field(_, _) => String::from("record"),
                            RnfBase::Func(_, _) => String::from("function"),
                            RnfBase::Tuple(_) => String::from("tuple"),
                        };

                        let diagnostic = Diagnostic::error("type::error")
                            .message(format!(
                                "type of kind `{}` cannot be a subtype of `{}`",
                                lkind, rkind
                            ))
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

                let diagnostic = Diagnostic::error("type::error")
                    .message(format!(
                        "type constraint `{} <: {}` does not hold",
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

        Ok(())
    }
}
