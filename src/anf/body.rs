use crate::attribute::Attributes;
use diagnostic::Span;
use std::ops::{Index, IndexMut};

use super::{Expr, Tid};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bid(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Local(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub name: Option<String>,
    pub attrs: Attributes,
    pub is_extern: bool,
    pub locals: Vec<Tid>,
    pub arguments: usize,
    pub output: Tid,
    pub exprs: Vec<Expr>,
    pub span: Span,
}

impl Body {
    pub fn inputs(&self) -> &[Tid] {
        &self.locals[..self.arguments]
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Locals {
    locals: Vec<Tid>,
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct Bodies {
    bodies: Vec<Body>,
}

impl Locals {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, tid: Tid) -> Local {
        let id = self.locals.len();
        self.locals.push(tid);
        Local(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Local, &Tid)> {
        self.locals
            .iter()
            .enumerate()
            .map(|(id, body)| (Local(id), body))
    }
}

impl Index<Local> for Locals {
    type Output = Tid;

    fn index(&self, Local(id): Local) -> &Self::Output {
        &self.locals[id]
    }
}

impl IndexMut<Local> for Locals {
    fn index_mut(&mut self, Local(id): Local) -> &mut Self::Output {
        &mut self.locals[id]
    }
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
