use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use super::{AliasId, BodyId, NamedId};

#[derive(Clone, Debug, Default)]
pub struct Module {
    pub name: Option<&'static str>,
    pub parent: Option<ModuleId>,
    pub modules: HashMap<&'static str, ModuleId>,
    pub items: HashMap<&'static str, Item>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_name(self, name: &'static str) -> Self {
        Self {
            name: Some(name),
            ..self
        }
    }

    pub fn with_parent(self, parent: ModuleId) -> Self {
        Self {
            parent: Some(parent),
            ..self
        }
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Func(BodyId),
    Type(NamedId),
    Alias(AliasId),
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
