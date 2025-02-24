use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use super::{BodyId, NamedId, Ty};

#[derive(Clone, Debug, Default)]
pub struct Module {
    pub modules: HashMap<&'static str, ModuleId>,
    pub items: HashMap<&'static str, Item>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Func(BodyId),
    Type(NamedId),
    Alias(Ty),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

#[derive(Clone, Debug, Default)]
pub struct Modules {
    modules: Vec<Module>,
}

impl Modules {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, module: Module) -> ModuleId {
        let id = self.modules.len();
        self.modules.push(module);
        ModuleId(id)
    }
}

impl Index<ModuleId> for Modules {
    type Output = Module;

    fn index(&self, ModuleId(id): ModuleId) -> &Self::Output {
        &self.modules[id]
    }
}

impl IndexMut<ModuleId> for Modules {
    fn index_mut(&mut self, ModuleId(id): ModuleId) -> &mut Self::Output {
        &mut self.modules[id]
    }
}
