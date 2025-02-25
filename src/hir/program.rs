use std::ops::{Index, IndexMut};

use super::{
    Alias, AliasId, Bodies, Body, BodyId, Module, ModuleId, Modules, Named, NamedId, Types,
};

#[derive(Debug, Default)]
pub struct Program {
    pub modules: Modules,
    pub bodies: Bodies,
    pub types: Types,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Index<ModuleId> for Program {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self.modules[index]
    }
}

impl Index<BodyId> for Program {
    type Output = Body;

    fn index(&self, index: BodyId) -> &Self::Output {
        &self.bodies[index]
    }
}

impl Index<NamedId> for Program {
    type Output = Named;

    fn index(&self, index: NamedId) -> &Self::Output {
        &self.types[index]
    }
}

impl Index<AliasId> for Program {
    type Output = Alias;

    fn index(&self, index: AliasId) -> &Self::Output {
        &self.types[index]
    }
}

impl IndexMut<ModuleId> for Program {
    fn index_mut(&mut self, index: ModuleId) -> &mut Self::Output {
        &mut self.modules[index]
    }
}

impl IndexMut<BodyId> for Program {
    fn index_mut(&mut self, index: BodyId) -> &mut Self::Output {
        &mut self.bodies[index]
    }
}

impl IndexMut<NamedId> for Program {
    fn index_mut(&mut self, index: NamedId) -> &mut Self::Output {
        &mut self.types[index]
    }
}

impl IndexMut<AliasId> for Program {
    fn index_mut(&mut self, index: AliasId) -> &mut Self::Output {
        &mut self.types[index]
    }
}
