use std::ops::{Index, IndexMut};

use crate::diagnostic::Span;

use super::{Binding, Expr, Generics, Local, LocalId, Locals, Ty};

#[derive(Clone, Debug)]
pub struct Body {
    pub name: String,
    pub generics: Generics,
    pub locals: Locals,
    pub input: Vec<Argument>,
    pub output: Ty,
    pub expr: Expr,
    pub is_extern: bool,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Argument {
    pub binding: Binding,
    pub ty: Ty,
}

impl Index<LocalId> for Body {
    type Output = Local;

    fn index(&self, index: LocalId) -> &Self::Output {
        &self.locals[index]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BodyId(pub usize);

#[derive(Clone, Debug, Default)]
pub struct Bodies {
    bodies: Vec<Body>,
}

impl Bodies {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, body: Body) -> BodyId {
        let id = self.bodies.len();
        self.bodies.push(body);
        BodyId(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (BodyId, &Body)> {
        self.bodies
            .iter()
            .enumerate()
            .map(|(id, body)| (BodyId(id), body))
    }
}

impl Index<BodyId> for Bodies {
    type Output = Body;

    fn index(&self, BodyId(id): BodyId) -> &Self::Output {
        &self.bodies[id]
    }
}

impl IndexMut<BodyId> for Bodies {
    fn index_mut(&mut self, BodyId(id): BodyId) -> &mut Self::Output {
        &mut self.bodies[id]
    }
}
