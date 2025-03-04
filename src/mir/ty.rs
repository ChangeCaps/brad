use std::{collections::BTreeSet, ops::Index};

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

    /// Represents a generic type.
    Generic(u16),

    /// Represents a named type.
    Named(u32, Vec<Self>),

    /// Represents a reference type.
    Ref(Box<Self>),

    /// Represents a list type.
    List(Box<Self>),

    /// Represents a function type.
    Func(Box<Self>, Box<Self>),

    /// Represents a tuple type.
    Tuple(Vec<Self>),

    /// Represents a record type.
    Record(Vec<(&'static str, Self)>),

    /// Represents a union type.
    Union(BTreeSet<Self>),
}

#[derive(Clone, Debug)]
pub struct Named {
    pub generics: u16,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct Types {
    named: Vec<Named>,
}

impl Types {
    pub fn new() -> Self {
        Self { named: Vec::new() }
    }

    pub fn push(&mut self, named: Named) -> u32 {
        let id = self.named.len() as u32;
        self.named.push(named);
        id
    }
}

impl Index<u32> for Types {
    type Output = Named;

    fn index(&self, id: u32) -> &Self::Output {
        &self.named[id as usize]
    }
}
