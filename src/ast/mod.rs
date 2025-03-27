mod decl;
mod expr;
mod formatter;
mod generate;
mod generic;
mod path;
mod ty;

pub use decl::*;
pub use expr::*;
pub use formatter::*;
pub use generate::*;
pub use generic::*;
pub use path::*;
pub use ty::*;

use crate::attribute::Attributes;

#[derive(Clone, Debug)]
pub struct Module {
    pub attrs: Attributes,
    pub decls: Vec<Decl>,
}
