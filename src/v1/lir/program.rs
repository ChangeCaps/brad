use super::{body::Bodies, ty::Types};

#[derive(Clone, Debug)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
}
