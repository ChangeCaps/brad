use std::ops::{Index, IndexMut};

use super::{Generic, Generics};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

    Union(Vec<Ty>),

    Record(Vec<Field>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NamedId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
