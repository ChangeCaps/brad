use std::ops::{Index, IndexMut};

use diagnostic::Span;
use solve::{Type, Var};

use crate::attribute::Attributes;

use super::{Binding, Expr, Local, LocalId, Locals};

#[derive(Clone, Debug)]
pub struct Body<T = Type> {
    pub attrs: Attributes,
    pub is_extern: bool,
    pub name: String,
    pub generics: Vec<Var>,
    pub locals: Locals<T>,
    pub input: Vec<Argument<T>>,
    pub output: T,
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Argument<T = Type> {
    pub binding: Binding,
    pub ty: T,
}

impl Body {
    pub fn ty(&self) -> Type {
        let mut ty = self.output.clone();

        for arg in self.input.iter().rev() {
            ty = arg.ty.clone().function(ty);
        }

        ty
    }
}

impl Index<LocalId> for Body {
    type Output = Local;

    fn index(&self, index: LocalId) -> &Self::Output {
        &self.locals[index]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BodyId(pub usize);

#[derive(Clone, Debug)]
pub struct Bodies<T = Type> {
    bodies: Vec<Body<T>>,
}

impl<T> Default for Bodies<T> {
    fn default() -> Self {
        Self { bodies: Vec::new() }
    }
}

impl<T> Bodies<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, body: Body<T>) -> BodyId {
        let id = self.bodies.len();
        self.bodies.push(body);
        BodyId(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (BodyId, &Body<T>)> {
        self.bodies
            .iter()
            .enumerate()
            .map(|(id, body)| (BodyId(id), body))
    }
}

impl<T> Index<BodyId> for Bodies<T> {
    type Output = Body<T>;

    fn index(&self, BodyId(id): BodyId) -> &Self::Output {
        &self.bodies[id]
    }
}

impl<T> IndexMut<BodyId> for Bodies<T> {
    fn index_mut(&mut self, BodyId(id): BodyId) -> &mut Self::Output {
        &mut self.bodies[id]
    }
}
