use std::ops::{Index, IndexMut};

use crate::attribute::Attributes;

use super::{stmt::Block, Ty};

/// Represents a function body.
///
/// The locals in the body are allocated accordingly:
/// +-----------------+-----------------+-----------------+
/// | captures        | arguments       | locals          |
/// +-----------------+-----------------+-----------------+
#[derive(Clone, Debug)]
pub struct Body<T = Ty> {
    /// The attributes of the body.
    pub attrs: Attributes,

    /// Whether the body is an extern function.
    pub is_extern: bool,

    /// The name of the function.
    pub name: Option<String>,

    /// The number of captures.
    pub captures: usize,

    /// The number of arguments.
    pub arguments: usize,

    /// The output type of the body.
    pub output: T,

    /// The locals in the body.
    pub locals: Locals<T>,

    /// The block of statements.
    pub block: Option<Block<T>>,
}

impl Default for Body<Ty> {
    fn default() -> Self {
        Self {
            attrs: Attributes::default(),
            is_extern: false,
            name: None,
            captures: 0,
            arguments: 0,
            output: Ty::None,
            locals: Locals::default(),
            block: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Locals<T = Ty> {
    locals: Vec<T>,
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

    pub fn push(&mut self, ty: T) -> Local {
        let local = self.locals.len();
        self.locals.push(ty);
        Local(local)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Local, &T)> {
        self.locals.iter().enumerate().map(|(i, ty)| (Local(i), ty))
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Local(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Bid(pub usize);

#[derive(Clone, Debug)]
pub struct Bodies<T = Ty> {
    bodies: Vec<Body<T>>,
}

impl<T> Default for Bodies<T> {
    fn default() -> Self {
        Self { bodies: Vec::new() }
    }
}

impl<T> Bodies<T> {
    pub fn new() -> Self {
        Self { bodies: Vec::new() }
    }

    pub fn push(&mut self, body: Body<T>) -> Bid {
        let local = self.bodies.len();
        self.bodies.push(body);
        Bid(local)
    }

    pub fn insert(&mut self, Bid(i): Bid, body: Body<T>) {
        self.bodies[i] = body;
    }

    pub fn iter(&self) -> impl Iterator<Item = (Bid, &Body<T>)> {
        self.bodies
            .iter()
            .enumerate()
            .map(|(i, body)| (Bid(i), body))
    }
}

impl<T> Index<Bid> for Bodies<T> {
    type Output = Body<T>;

    fn index(&self, Bid(i): Bid) -> &Self::Output {
        &self.bodies[i]
    }
}

impl<T> Index<Local> for Locals<T> {
    type Output = T;

    fn index(&self, Local(i): Local) -> &Self::Output {
        &self.locals[i]
    }
}

impl<T> IndexMut<Bid> for Bodies<T> {
    fn index_mut(&mut self, Bid(i): Bid) -> &mut Self::Output {
        &mut self.bodies[i]
    }
}

impl<T> IndexMut<Local> for Locals<T> {
    fn index_mut(&mut self, Local(i): Local) -> &mut Self::Output {
        &mut self.locals[i]
    }
}
