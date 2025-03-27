mod decl;
mod expr;
mod formatter;
mod generate;
mod generic;
mod path;
mod ty;

use arbitrary::{Arbitrary, Unstructured};
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

impl<'a> Arbitrary<'a> for Module {
    fn arbitrary(_: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let options = GeneratorOptions::default();
        let mut generator = Generator::new(options);
        Ok(generator.generate())
    }
}
