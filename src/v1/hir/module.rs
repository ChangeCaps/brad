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

    pub fn iter(&self) -> impl Iterator<Item = (ModuleId, &Module)> {
        self.modules
            .iter()
            .enumerate()
            .map(|(id, module)| (ModuleId(id), module))
    }

    pub fn ids(&self) -> impl Iterator<Item = ModuleId> {
        (0..self.modules.len()).map(ModuleId)
    }

    pub fn insert_item(&mut self, mut from: ModuleId, segments: &[&'static str], item: Item) {
        let last = segments.len() - 1;

        for segment in segments.iter().take(last) {
            match self[from].modules.get(segment) {
                Some(id) => from = *id,
                None => {
                    let id = self.insert(Module::new());
                    self[from].modules.insert(segment, id);
                    from = id;
                }
            }
        }

        self[from].items.insert(segments[last], item);
    }

    pub fn get_module(&self, from: ModuleId, segments: &[&'static str]) -> Option<ModuleId> {
        let mut module = &self[from];

        for segment in segments {
            let id = module.modules.get(segment)?;
            module = &self[*id];
        }

        Some(from)
    }

    pub fn get_item(&self, from: ModuleId, segments: &[&'static str]) -> Option<&Item> {
        let mut module = &self[from];

        for segment in segments.iter().take(segments.len() - 1) {
            let id = module.modules.get(segment)?;
            module = &self[*id];
        }

        module.items.get(segments.last()?)
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
