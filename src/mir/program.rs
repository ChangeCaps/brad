use super::{body::Bodies, ty::Types};

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }
}
