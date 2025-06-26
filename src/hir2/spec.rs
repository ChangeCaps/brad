use crate::anf::{Type, Types};
use crate::attribute::Attributes;
use crate::hir2::{Argument, Expr, Locals};
use diagnostic::Span;
use std::ops::{Index, IndexMut};

pub struct SpecializedBodyId(pub usize);

#[derive(Clone, Debug)]
pub struct SpecializedBody {
    pub attrs: Attributes,
    pub is_extern: bool,
    pub name: String,
    pub locals: Locals<Type>,
    pub input: Vec<Argument<Type>>,
    pub output: Type,
    pub expr: Option<Expr<Type>>,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct SpecializedBodies {
    pub bodies: Vec<SpecializedBody>,
}

impl SpecializedBodies {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, body: SpecializedBody) -> SpecializedBodyId {
        let id = self.bodies.len();
        self.bodies.push(body);
        SpecializedBodyId(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SpecializedBodyId, &SpecializedBody)> {
        self.bodies
            .iter()
            .enumerate()
            .map(|(id, body)| (SpecializedBodyId(id), body))
    }
}

impl Index<SpecializedBodyId> for SpecializedBodies {
    type Output = SpecializedBody;

    fn index(&self, SpecializedBodyId(id): SpecializedBodyId) -> &Self::Output {
        &self.bodies[id]
    }
}

impl IndexMut<SpecializedBodyId> for SpecializedBodies {
    fn index_mut(&mut self, SpecializedBodyId(id): SpecializedBodyId) -> &mut Self::Output {
        &mut self.bodies[id]
    }
}

#[derive(Clone, Debug, Default)]
pub struct SpecializedProgram {
    pub bodies: SpecializedBodies,
    pub types: Types,
}

impl Index<SpecializedBodyId> for SpecializedProgram {
    type Output = SpecializedBody;

    fn index(&self, index: SpecializedBodyId) -> &Self::Output {
        &self.bodies[index]
    }
}

impl IndexMut<SpecializedBodyId> for SpecializedProgram {
    fn index_mut(&mut self, index: SpecializedBodyId) -> &mut Self::Output {
        &mut self.bodies[index]
    }
}
