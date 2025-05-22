use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use solve::Tag;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tid {
    index: usize,
}

#[derive(Clone, Debug)]
pub struct Ty {
    pub tags: Vec<Tag>,
    pub kind: TyKind,
}

#[derive(Clone, Debug)]
pub enum TyKind {
    Any,
    Never,
    Ref { refee: Tid },
    Array { item: Tid },
    Func { input: Tid, output: Tid },
    Tuple { items: Vec<Tid> },
    Record { fields: HashMap<&'static str, Tid> },
    Union { variants: Vec<Tid> },
}

/// The type context for a program represented in mir.
#[derive(Clone, Debug)]
pub struct Tcx {
    types: Vec<Ty>,
}

impl Tcx {
    pub fn new() -> Self {
        Tcx { types: Vec::new() }
    }

    pub fn push(&mut self, ty: Ty) -> Tid {
        let index = self.types.len();
        self.types.push(ty);
        Tid { index }
    }
}

impl From<TyKind> for Ty {
    fn from(kind: TyKind) -> Self {
        Ty {
            tags: Vec::new(),
            kind,
        }
    }
}

impl Index<Tid> for Tcx {
    type Output = Ty;

    fn index(&self, id: Tid) -> &Self::Output {
        &self.types[id.index]
    }
}

impl IndexMut<Tid> for Tcx {
    fn index_mut(&mut self, id: Tid) -> &mut Self::Output {
        &mut self.types[id.index]
    }
}
