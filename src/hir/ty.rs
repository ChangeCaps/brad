use std::ops::Index;

use super::{Generic, Generics};

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Field {
    pub name: &'static str,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct Named {
    pub generics: Generics,
    pub ty: Option<Ty>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NamedId(usize);

#[derive(Clone, Debug, Default)]
pub struct Types {
    named: Vec<Named>,
}

impl Types {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, ty: Named) -> NamedId {
        let id = self.named.len();
        self.named.push(ty);
        NamedId(id)
    }
}

impl Index<NamedId> for Types {
    type Output = Named;

    fn index(&self, NamedId(index): NamedId) -> &Self::Output {
        &self.named[index]
    }
}
