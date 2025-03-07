use super::{body::Bodies, ty::Types, Bid, Ty};

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub bodies: Bodies<Ty>,
    pub types: Types,
}

impl Program {
    pub fn find_body(&self, name: &str) -> Option<Bid> {
        self.bodies
            .iter()
            .find(|(_, body)| body.name.as_deref() == Some(name))
            .map(|(id, _)| id)
    }
}
