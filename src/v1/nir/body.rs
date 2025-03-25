use std::ops::Index;

use super::ty::Tid;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Name(pub u32);

#[derive(Clone, Debug)]
pub struct Body {
    pub locals: Vec<Tid>,
}

impl Body {
    pub fn new() -> Self {
        Self { locals: Vec::new() }
    }
}

impl Index<Name> for Body {
    type Output = Tid;

    fn index(&self, Name(index): Name) -> &Self::Output {
        &self.locals[index as usize]
    }
}
