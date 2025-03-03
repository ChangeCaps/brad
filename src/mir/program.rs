use super::{body::Bodies, ty::Types, Tid};

#[derive(Clone, Debug)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
    pub builtins: Builtins,
}

#[derive(Clone, Debug)]
pub struct Builtins {
    pub true_tid: Tid,
    pub false_tid: Tid,
}
