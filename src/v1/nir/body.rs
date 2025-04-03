use std::ops::Index;

use super::ty::Tid;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Local(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bid(pub u32);

#[derive(Clone, Debug)]
pub struct Body {
    pub locals: Vec<Tid>,
}

impl Body {
    pub fn new() -> Self {
        Self { locals: Vec::new() }
    }
}

impl Index<Local> for Body {
    type Output = Tid;

    fn index(&self, Local(index): Local) -> &Self::Output {
        &self.locals[index as usize]
    }
}
