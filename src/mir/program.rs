use super::{body::Bodies, ty::Types, Bid, Ty};
use crate::diagnostic::Diagnostic;

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies<Ty>,
    pub types: Types,
}

impl Program {
    pub fn find_body(&self, name: &str) -> Result<Bid, Diagnostic> {
        match self
            .bodies
            .iter()
            .find(|(_, body)| body.name.as_deref() == Some(name))
        {
            Some((bid, _)) => Ok(bid),
            None => Err(Diagnostic::error("function::not_found")
                .message(format!("no function found at '{}'", name))),
        }
    }
}
