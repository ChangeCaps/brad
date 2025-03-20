use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use crate::{diagnostic::Span, solve::Tag};

use super::BodyId;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Vis {
    Pub,
    Priv,
}

#[derive(Clone, Debug, Default)]
pub struct Module {
    pub name: Option<String>,
    pub modules: HashMap<&'static str, (ModuleId, Vis)>,
    pub bodies: HashMap<&'static str, (BodyId, Vis)>,
    pub types: HashMap<&'static str, (Tag, Vis, Span)>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_name(self, name: impl Into<String>) -> Self {
        Self {
            name: Some(name.into()),
            ..self
        }
    }
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

    pub fn insert_module(
        &mut self,
        mut from: ModuleId,
        segments: &[&'static str],
        vis: Vis,
    ) -> ModuleId {
        let mut name = self[from].name.clone().unwrap_or_default();

        for (i, segment) in segments.iter().enumerate() {
            match self[from].modules.get(segment) {
                Some((id, _)) => {
                    name = self[*id].name.clone().unwrap_or_default();
                    from = *id;
                }

                None => {
                    if !name.is_empty() {
                        name.push_str("::");
                    }

                    name.push_str(segment);

                    let vis = if i == 0 { vis } else { Vis::Priv };
                    let id = self.insert(Module::new().with_name(&name));
                    self[from].modules.insert(segment, (id, vis));
                    from = id;
                }
            }
        }

        from
    }

    pub fn insert_body(
        &mut self,
        from: ModuleId,
        segments: &[&'static str],
        body: BodyId,
        vis: Vis,
    ) -> Result<(), BodyId> {
        let module = self.insert_module(from, &segments[0..segments.len() - 1], vis);
        let last = segments.last().unwrap();

        match self[module].bodies.insert(last, (body, Vis::Pub)) {
            Some((body, _)) => Err(body),
            None => Ok(()),
        }
    }

    pub fn insert_type(
        &mut self,
        from: ModuleId,
        segments: &[&'static str],
        ty: Tag,
        vis: Vis,
        span: Span,
    ) -> Result<(), Span> {
        let module = self.insert_module(from, &segments[0..segments.len() - 1], vis);
        let last = segments.last().unwrap();

        match self[module].types.insert(last, (ty, Vis::Pub, span)) {
            Some((_, _, span)) => Err(span),
            None => Ok(()),
        }
    }

    pub fn get_module<I, S>(&self, mut from: ModuleId, segments: I) -> Option<ModuleId>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
        I::IntoIter: Clone + ExactSizeIterator,
    {
        for (i, segment) in segments.into_iter().enumerate() {
            let (id, vis) = self[from].modules.get(segment.as_ref())?;
            from = *id;

            if *vis == Vis::Priv && i > 0 {
                return None;
            }
        }

        Some(from)
    }

    pub fn get_body<I, S>(&self, from: ModuleId, segments: I) -> Option<BodyId>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
        I::IntoIter: Clone + ExactSizeIterator,
    {
        let segments = segments.into_iter();

        let module_segments = segments.clone().take(segments.len() - 1);
        let module = self.get_module(from, module_segments)?;

        let body_segment = segments.last()?;
        let (body, vis) = self[module].bodies.get(body_segment.as_ref())?;

        if *vis == Vis::Priv && module != from {
            return None;
        }

        Some(*body)
    }

    pub fn get_type<I, S>(&self, from: ModuleId, segments: I) -> Option<Tag>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
        I::IntoIter: Clone + ExactSizeIterator,
    {
        let segments = segments.into_iter();

        let module_segments = segments.clone().take(segments.len() - 1);
        let module = self.get_module(from, module_segments)?;

        let type_segment = segments.last()?;
        let (ty, vis, _) = self[module].types.get(type_segment.as_ref())?;

        if *vis == Vis::Priv && module != from {
            return None;
        }

        Some(*ty)
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
