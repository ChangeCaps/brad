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

#[derive(Clone, Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}
