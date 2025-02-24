use super::{Bodies, Modules, Types};

#[derive(Debug, Default)]
pub struct Program {
    pub modules: Modules,
    pub bodies: Bodies,
    pub types: Types,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }
}
