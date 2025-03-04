use std::{
    collections::BTreeSet,
    ops::{Index, IndexMut},
};

use super::{Generic, Generics};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    Int,

    Float,

    Str,

    True,

    False,

    None,

    Never,

    Generic(Generic),

    Named(NamedId, Vec<Ty>),

    Ref(Box<Ty>),

    List(Box<Ty>),

    Func(Box<Ty>, Box<Ty>),

    Tuple(Vec<Ty>),

    Union(BTreeSet<Ty>),

    Record(Vec<Field>),
}

impl Ty {
    pub fn normalize(self) -> Self {
        match self {
            Self::Int
            | Self::Float
            | Self::Str
            | Self::True
            | Self::False
            | Self::None
            | Self::Never
            | Self::Generic(_) => self,

            Self::Named(id, mut tys) => {
                tys = tys.into_iter().map(|ty| ty.normalize()).collect();

                Self::Named(id, tys)
            }

            Self::Ref(ty) => Self::Ref(Box::new(ty.normalize())),

            Self::List(ty) => Self::List(Box::new(ty.normalize())),

            Self::Func(i, o) => Self::Func(Box::new(i.normalize()), Box::new(o.normalize())),

            Self::Tuple(mut tys) => {
                tys = tys.into_iter().map(|ty| ty.normalize()).collect();

                if tys.len() == 1 {
                    tys.into_iter().next().unwrap()
                } else {
                    Self::Tuple(tys)
                }
            }

            Self::Record(mut fields) => {
                for field in &mut fields {
                    field.ty = field.ty.clone().normalize();
                }

                Self::Record(fields)
            }

            Self::Union(tys) => {
                let mut new_tys = BTreeSet::new();

                for ty in tys {
                    match ty.normalize() {
                        Ty::Union(tys) => new_tys.extend(tys),
                        ty => _ = new_tys.insert(ty),
                    }
                }

                if new_tys.len() == 1 {
                    new_tys.into_iter().next().unwrap()
                } else {
                    Ty::Union(new_tys)
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub name: &'static str,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct Named {
    pub generics: Generics,
    pub ty: Option<Ty>,
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub generics: Generics,
    pub ty: Ty,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NamedId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AliasId(usize);

#[derive(Clone, Debug, Default)]
pub struct Types {
    named: Vec<Named>,
    alias: Vec<Alias>,
}

impl Types {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_named(&mut self, ty: Named) -> NamedId {
        let id = self.named.len();
        self.named.push(ty);
        NamedId(id)
    }

    pub fn insert_alias(&mut self, alias: Alias) -> AliasId {
        let id = self.alias.len();
        self.alias.push(alias);
        AliasId(id)
    }

    pub fn format(&self, ty: &Ty) -> String {
        match ty {
            Ty::Int => String::from("int"),
            Ty::Float => String::from("float"),
            Ty::Str => String::from("str"),
            Ty::True => String::from("true"),
            Ty::False => String::from("false"),
            Ty::None => String::from("none"),
            Ty::Never => String::from("!"),
            Ty::Generic(generic) => format!("{:?}", generic),
            Ty::Named(_, _) => String::from("named"),
            Ty::Ref(ty) => format!("ref {}", self.format(ty)),
            Ty::List(ty) => format!("[{}]", self.format(ty)),
            Ty::Func(i, o) => format!("{} -> {}", self.format(i), self.format(o)),
            Ty::Tuple(tys) => tys
                .iter()
                .map(|ty| self.format(ty))
                .collect::<Vec<_>>()
                .join(", "),
            Ty::Union(tys) => tys
                .iter()
                .map(|ty| self.format(ty))
                .collect::<Vec<_>>()
                .join(" | "),
            Ty::Record(fields) => fields
                .iter()
                .map(|field| format!("{}: {}", field.name, self.format(&field.ty)))
                .collect::<Vec<_>>()
                .join("; "),
        }
    }
}

impl Index<NamedId> for Types {
    type Output = Named;

    fn index(&self, NamedId(index): NamedId) -> &Self::Output {
        &self.named[index]
    }
}

impl Index<AliasId> for Types {
    type Output = Alias;

    fn index(&self, AliasId(index): AliasId) -> &Self::Output {
        &self.alias[index]
    }
}

impl IndexMut<NamedId> for Types {
    fn index_mut(&mut self, NamedId(index): NamedId) -> &mut Self::Output {
        &mut self.named[index]
    }
}

impl IndexMut<AliasId> for Types {
    fn index_mut(&mut self, AliasId(index): AliasId) -> &mut Self::Output {
        &mut self.alias[index]
    }
}
