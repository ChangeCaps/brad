use std::ops::{Index, IndexMut};

use crate::solve::Solver;

use super::{Bodies, Body, BodyId, Module, ModuleId, Modules};

#[derive(Debug, Default)]
pub struct Program {
    pub modules: Modules,
    pub bodies: Bodies,
    pub solver: Solver,
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
