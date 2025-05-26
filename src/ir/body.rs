use crate::attribute::Attributes;
use std::ops::{Index, IndexMut};

use super::{Expr, Tid};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bid(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Local(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Body {
    pub name: Option<String>,
    pub attrs: Attributes,
    pub is_extern: bool,
    pub locals: Vec<Tid>,
    pub arguments: usize,
    pub output: Tid,
    pub expr: Option<Expr>,
}

impl Body {
    pub fn inputs(&self) -> &[Tid] {
        &self.locals[..self.arguments]
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Bodies {
    bodies: Vec<Body>,
}

impl Bodies {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, body: Body) -> Bid {
        let id = self.bodies.len();
        self.bodies.push(body);
        Bid(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Bid, &Body)> {
        self.bodies
            .iter()
            .enumerate()
            .map(|(id, body)| (Bid(id), body))
    }
}

impl Index<Bid> for Bodies {
    type Output = Body;

    fn index(&self, Bid(id): Bid) -> &Self::Output {
        &self.bodies[id]
    }
}

impl IndexMut<Bid> for Bodies {
    fn index_mut(&mut self, Bid(id): Bid) -> &mut Self::Output {
        &mut self.bodies[id]
    }
}
