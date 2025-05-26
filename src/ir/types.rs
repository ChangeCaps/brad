use solve::Tags;
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Tid(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub terms: Vec<(Tags, Base)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Base {
    Unit,
    Record(Vec<(&'static str, Tid)>),
    Tuple(Vec<Tid>),
    Array(Tid),
    Func(Tid, Tid),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Types {
    types: Vec<Type>,
}

impl Types {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, ty: Type) -> Tid {
        let id = self.types.len();
        self.types.push(ty);
        Tid(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Tid, &Type)> {
        self.types.iter().enumerate().map(|(id, ty)| (Tid(id), ty))
    }
}

impl Index<Tid> for Types {
    type Output = Type;

    fn index(&self, Tid(id): Tid) -> &Self::Output {
        &self.types[id]
    }
}

impl IndexMut<Tid> for Types {
    fn index_mut(&mut self, Tid(id): Tid) -> &mut Self::Output {
        &mut self.types[id]
    }
}
