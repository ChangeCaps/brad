use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap},
    ops::{Index, IndexMut},
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    /// Represents an integer type.
    Int,

    /// Represents a floating point type.
    Float,

    /// Represents a string type.
    Str,

    /// Represents a true type.
    True,

    /// Represents a false type.
    False,

    /// Represents a none type.
    None,

    /// Represents a never type.
    Never,

    /// Represents a reference type.
    Ref(Tid),

    /// Represents a list type.
    List(Tid),

    /// Represents a function type.
    Func(Tid, Tid),

    /// Represents a tuple type.
    Tuple(Vec<Tid>),

    /// Represents a record type.
    Record(Vec<(&'static str, Tid)>),

    /// Represents a union type.
    Union(BTreeSet<Tid>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tid(pub usize);

#[derive(Clone, Debug, Default)]
pub struct Types {
    types: Vec<Ty>,
    ids: HashMap<Ty, Tid>,
}

impl Types {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            ids: HashMap::new(),
        }
    }

    pub fn push(&mut self, ty: Ty) -> Tid {
        let id = Tid(self.types.len());
        self.types.push(ty);
        id
    }

    pub fn get_or_insert(&mut self, ty: &Ty) -> Tid {
        match self.ids.entry(ty.clone()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let id = Tid(self.types.len());
                self.types.push(ty.clone());
                entry.insert(id);
                id
            }
        }
    }
}

impl Index<Tid> for Types {
    type Output = Ty;

    fn index(&self, Tid(i): Tid) -> &Self::Output {
        &self.types[i]
    }
}

impl IndexMut<Tid> for Types {
    fn index_mut(&mut self, Tid(i): Tid) -> &mut Self::Output {
        &mut self.types[i]
    }
}

impl Index<Ty> for Types {
    type Output = Tid;

    fn index(&self, ty: Ty) -> &Self::Output {
        &self.ids[&ty]
    }
}
