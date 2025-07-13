mod body;
mod build;
mod expr;
pub mod simple_spec;
mod types;

pub use body::*;
pub use build::*;
pub use expr::*;
pub use types::*;

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies,
    pub types: Types,
}
