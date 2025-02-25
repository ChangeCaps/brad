use std::ops::Index;

#[derive(Clone, Debug)]
pub enum Ty {
    /// Represents an integer type.
    Int,

    /// Represents a floating point type.
    Float,

    /// Represents a string type.
    Str,

    /// Represents a generic type.
    Generic(u16),

    /// Represents a reference type.
    Ref(Tid),

    /// Represents a list type.
    List(Tid),

    /// Represents a function type.
    Func(Tid, Tid),

    /// Represents a record type.
    Record(Vec<Tid>),

    /// Represents a union type.
    Union(Vec<Tid>),
}

impl Ty {
    pub const ZST: Self = Self::Record(Vec::new());
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Tid(pub usize);

#[derive(Clone, Debug, Default)]
pub struct Types {
    types: Vec<Option<Ty>>,
}

impl Types {
    pub fn new() -> Self {
        Self { types: Vec::new() }
    }

    pub fn reserve(&mut self) -> Tid {
        let tid = self.types.len();
        self.types.push(None);
        Tid(tid)
    }

    pub fn push(&mut self, ty: Ty) -> Tid {
        let tid = self.types.len();
        self.types.push(Some(ty));
        Tid(tid)
    }

    pub fn insert(&mut self, Tid(i): Tid, ty: Ty) {
        self.types[i] = Some(ty);
    }
}

impl Index<Tid> for Types {
    type Output = Ty;

    fn index(&self, Tid(i): Tid) -> &Ty {
        self.types[i].as_ref().unwrap()
    }
}
