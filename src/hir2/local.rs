use std::ops::Index;

use crate::{diagnostic::Span, solve::Ty};

#[derive(Clone, Debug)]
pub struct Local {
    pub is_mutable: bool,
    pub name: &'static str,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

impl LocalId {
    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug, Default)]
pub struct Locals {
    locals: Vec<Local>,
}

impl Locals {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.locals.len()
    }

    pub fn is_empty(&self) -> bool {
        self.locals.is_empty()
    }

    pub fn insert(&mut self, local: Local) -> LocalId {
        let id = self.locals.len();
        self.locals.push(local);
        LocalId(id)
    }

    pub fn ids(&self) -> impl Iterator<Item = LocalId> {
        (0..self.locals.len()).map(LocalId)
    }
}

impl Index<LocalId> for Locals {
    type Output = Local;

    fn index(&self, LocalId(id): LocalId) -> &Self::Output {
        &self.locals[id]
    }
}
