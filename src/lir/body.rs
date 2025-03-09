use std::ops::{Index, IndexMut};

use crate::attribute::Attributes;

use super::{stmt::Block, ty::Tid};

#[derive(Clone, Debug)]
pub struct Body {
    /// type id of function
    pub tid: Tid,
    /// list of local variable types
    pub locals: Vec<Tid>,
    /// function body
    pub block: Block,

    /// function attributes
    pub attrs: Attributes,
    /// if function is a link to extern
    pub is_extern: bool,
    /// name of function
    pub name: Option<String>,
}

#[derive(Clone, Copy, Debug)]
pub struct Bid(u32);

#[derive(Clone, Copy, Debug)]
pub struct Local(pub u32);

#[derive(Clone, Debug)]
pub struct Bodies(Vec<Body>);

impl Bodies {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, body: Body) -> Bid {
        let index = self.0.len() as u32;
        self.0.push(body);
        Bid(index)
    }
}

impl Index<Local> for Body {
    type Output = Tid;

    fn index(&self, Local(index): Local) -> &Self::Output {
        &self.locals[index as usize]
    }
}

impl Index<Bid> for Bodies {
    type Output = Body;

    fn index(&self, Bid(index): Bid) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<Bid> for Bodies {
    fn index_mut(&mut self, Bid(index): Bid) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}
