use std::ops::Index;

use diagnostic::Span;
use solve::Type;

#[derive(Clone, Debug)]
pub struct Local<T = Type> {
    pub is_mutable: bool,
    pub name: &'static str,
    pub ty: T,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

impl LocalId {
    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug)]
pub struct Locals<T = Type> {
    locals: Vec<Local<T>>,
}

impl<T> Default for Locals<T> {
    fn default() -> Self {
        Self { locals: Vec::new() }
    }
}

impl<T> Locals<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.locals.len()
    }

    pub fn is_empty(&self) -> bool {
        self.locals.is_empty()
    }

    pub fn insert(&mut self, local: Local<T>) -> LocalId {
        let id = self.locals.len();
        self.locals.push(local);
        LocalId(id)
    }

    pub fn ids(&self) -> impl Iterator<Item = LocalId> {
        (0..self.locals.len()).map(LocalId)
    }
}

impl<T> Index<LocalId> for Locals<T> {
    type Output = Local<T>;

    fn index(&self, LocalId(id): LocalId) -> &Self::Output {
        &self.locals[id]
    }
}

impl<T> IntoIterator for Locals<T> {
    type Item = Local<T>;

    type IntoIter = std::vec::IntoIter<Local<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.locals.into_iter()
    }
}
