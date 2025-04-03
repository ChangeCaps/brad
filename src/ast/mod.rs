mod decl;
mod expr;
mod formatter;
mod generate;
mod generic;
mod path;
mod spanned;
mod ty;

pub use decl::*;
pub use expr::*;
pub use formatter::*;
pub use generate::*;
pub use generic::*;
pub use path::*;
pub use spanned::*;
pub use ty::*;

use crate::attribute::Attributes;
use crate::diagnostic::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub attrs: Attributes,
    pub decls: Vec<Decl>,
}

impl Spanned for Module {
    fn span(&self) -> Span {
        self.decls.span()
    }

    fn reset_spans(&mut self) {
        self.decls.reset_spans();
        self.attrs.reset_spans();
    }
}
