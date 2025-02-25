use std::ops::{Index, IndexMut};

use super::{Generic, Generics};

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NamedId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
