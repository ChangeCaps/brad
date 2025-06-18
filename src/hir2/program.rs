use super::{Bodies, Body, BodyId, Module, ModuleId, Modules};
use solve::Type;
use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Program<T = Type> {
    pub modules: Modules,
    pub bodies: Bodies<T>,
    pub tcx: solve::Tcx,
}

impl<T> Default for Program<T> {
    fn default() -> Self {
        Self {
            modules: Modules::default(),
            bodies: Bodies::default(),
            tcx: solve::Tcx::default(),
        }
    }
}

impl<T> Program<T> {
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
