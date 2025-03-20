use std::ops::{Index, IndexMut};

use crate::{attribute::Attributes, diagnostic::Span, solve::Ty};

use super::{Binding, Expr, Local, LocalId, Locals};

#[derive(Clone, Debug)]
pub struct Body {
    pub attrs: Attributes,
    pub is_extern: bool,
    pub name: String,
    pub generics: Vec<Ty>,
    pub locals: Locals,
    pub input: Vec<Argument>,
    pub output: Ty,
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Argument {
    pub binding: Binding,
    pub ty: Ty,
}

impl Body {
    pub fn ty(&self) -> Ty {
        let mut ty = self.output.clone();

        for arg in self.input.iter().rev() {
            ty = Ty::func(arg.ty.clone(), ty);
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
