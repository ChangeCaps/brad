use super::{body::Bodies, ty::Types, Ty};

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies<Ty>,
    pub types: Types,
}
