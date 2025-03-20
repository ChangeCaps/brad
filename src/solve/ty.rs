use std::collections::{BTreeMap, HashMap};

use super::Var;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag {
    pub name: &'static str,
    pub data: u64,
}

impl Tag {
    pub const NONE: Self = Self::new("none", u64::MAX);
    pub const TRUE: Self = Self::new("true", u64::MAX);
    pub const FALSE: Self = Self::new("false", u64::MAX);
    pub const INT: Self = Self::new("int", u64::MAX);
    pub const FLOAT: Self = Self::new("float", u64::MAX);
    pub const STR: Self = Self::new("str", u64::MAX);

    pub const fn new(name: &'static str, data: u64) -> Self {
        Tag { name, data }
    }
}

pub type Field = &'static str;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct App {
    pub tag: Tag,
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
    pub const NONE: Self = Ty::Tag(Tag::NONE);
    pub const TRUE: Self = Ty::Tag(Tag::TRUE);
    pub const FALSE: Self = Ty::Tag(Tag::FALSE);
    pub const INT: Self = Ty::Tag(Tag::INT);
    pub const FLOAT: Self = Ty::Tag(Tag::FLOAT);
    pub const STR: Self = Ty::Tag(Tag::STR);

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

    pub fn app(tag: Tag, args: Vec<Ty>) -> Self {
        Ty::App(App { tag, args })
    }

    pub fn tag(name: &'static str, data: u64) -> Self {
        Ty::Tag(Tag::new(name, data))
    }

    pub fn subst(&self, map: &HashMap<Ty, Ty>) -> Self {
        self.clone().map(|ty| map.get(&ty).cloned().unwrap_or(ty))
    }

    pub fn visit(&self, mut f: impl FnMut(&Ty)) {
        self.visit_impl(&mut f)
    }

    fn visit_impl(&self, f: &mut dyn FnMut(&Ty)) {
        match self {
            Ty::Union(lhs, rhs) | Ty::Inter(lhs, rhs) => {
                lhs.visit_impl(f);
                rhs.visit_impl(f);
            }

            Ty::Neg(ty) => ty.visit_impl(f),

            Ty::Record(fields) => {
                for ty in fields.values() {
                    ty.visit_impl(f);
                }
            }

            Ty::Tuple(tys) => {
                for ty in tys {
                    ty.visit_impl(f);
                }
            }

            Ty::Func(input, output) => {
                input.visit_impl(f);
                output.visit_impl(f);
            }

            Ty::List(ty) | Ty::Ref(ty) => ty.visit_impl(f),

            Ty::App(app) => {
                for ty in &app.args {
                    ty.visit_impl(f);
                }
            }

            Ty::Var(_) | Ty::Tag(_) | Ty::Top | Ty::Bot => {}
        }

        f(self);
    }

    pub fn map(self, mut f: impl FnMut(Ty) -> Ty) -> Self {
        self.map_impl(&mut f)
    }

    fn map_impl(self, f: &mut dyn FnMut(Ty) -> Ty) -> Self {
        match f(self) {
            Ty::Union(lhs, rhs) => Ty::union(lhs.map_impl(f), rhs.map_impl(f)),
            Ty::Inter(lhs, rhs) => Ty::inter(lhs.map_impl(f), rhs.map_impl(f)),
            Ty::Neg(ty) => Ty::neg(ty.map_impl(f)),

            Ty::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(n, ty)| (n, ty.map_impl(f)))
                    .collect();

                Ty::Record(fields)
            }

            Ty::Tuple(tys) => Ty::Tuple(tys.into_iter().map(|ty| ty.map_impl(f)).collect()),

            Ty::Func(input, output) => Ty::func(input.map_impl(f), output.map_impl(f)),

            Ty::List(ty) => Ty::list(ty.map_impl(f)),
            Ty::Ref(ty) => Ty::ref_(ty.map_impl(f)),

            Ty::App(app) => Ty::app(
                app.tag,
                app.args.into_iter().map(|ty| ty.map_impl(f)).collect(),
            ),

            ty @ (Ty::Var(_) | Ty::Tag(_) | Ty::Top | Ty::Bot) => ty,
        }
    }
}
