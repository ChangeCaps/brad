mod body;
mod build;
mod expr;
mod types;

pub use body::*;
pub use expr::*;
pub use types::*;

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
}
