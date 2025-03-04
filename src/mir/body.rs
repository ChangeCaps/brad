use std::ops::{Index, IndexMut};

use super::{stmt::Block, ty::Ty};

/// Represents a function body.
///
/// The locals in the body are allocated accordingly:
/// +-----------------+-----------------+-----------------+
/// | captures        | arguments       | locals          |
/// +-----------------+-----------------+-----------------+
#[derive(Clone, Debug)]
pub struct Body {
    /// The number of captures.
    pub captures: usize,

    /// The number of arguments.
    pub arguments: usize,

    /// The locals in the body.
    pub locals: Locals,

    /// The block of statements.
    pub block: Block,
}

#[derive(Clone, Debug, Default)]
pub struct Locals {
    locals: Vec<Ty>,
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

    pub fn push(&mut self, ty: Ty) -> Local {
        let local = self.locals.len();
        self.locals.push(ty);
        Local(local)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Local(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Bid(usize);

#[derive(Clone, Debug, Default)]
pub struct Bodies {
    bodies: Vec<Body>,
}

impl Bodies {
    pub fn new() -> Self {
        Self { bodies: Vec::new() }
    }

    pub fn push(&mut self, body: Body) -> Bid {
        let local = self.bodies.len();
        self.bodies.push(body);
        Bid(local)
    }

    pub fn insert(&mut self, Bid(i): Bid, body: Body) {
        self.bodies[i] = body;
    }
}

impl Index<Bid> for Bodies {
    type Output = Body;

    fn index(&self, Bid(i): Bid) -> &Self::Output {
        &self.bodies[i]
    }
}

impl Index<Local> for Locals {
    type Output = Ty;

    fn index(&self, Local(i): Local) -> &Self::Output {
        &self.locals[i]
    }
}

impl IndexMut<Bid> for Bodies {
    fn index_mut(&mut self, Bid(i): Bid) -> &mut Self::Output {
        &mut self.bodies[i]
    }
}

impl IndexMut<Local> for Locals {
    fn index_mut(&mut self, Local(i): Local) -> &mut Self::Output {
        &mut self.locals[i]
    }
}
