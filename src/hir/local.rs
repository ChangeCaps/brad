use std::ops::Index;

use crate::diagnostic::Span;

use super::Ty;

#[derive(Clone, Debug)]
pub struct Local {
    pub mutable: bool,
    pub name: &'static str,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LocalId(usize);

#[derive(Clone, Debug, Default)]
pub struct Locals {
    locals: Vec<Local>,
}

impl Locals {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, local: Local) -> LocalId {
        let id = self.locals.len();
        self.locals.push(local);
        LocalId(id)
    }
}

impl Index<LocalId> for Locals {
    type Output = Local;

    fn index(&self, LocalId(id): LocalId) -> &Self::Output {
        &self.locals[id]
    }
}
