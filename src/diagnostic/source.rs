use std::{fmt, ops::Index};

pub struct Source {
    pub content: String,
    pub file: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceId(u32);

pub struct Sources {
    sources: Vec<Source>,
}

impl Sources {
    pub fn new() -> Sources {
        Sources {
            sources: Vec::new(),
        }
    }

    pub fn push(&mut self, source: Source) -> SourceId {
        let id = self.sources.len();
        self.sources.push(source);
        SourceId(id as u32)
    }
}

impl Index<SourceId> for Sources {
    type Output = Source;

    fn index(&self, SourceId(id): SourceId) -> &Source {
        &self.sources[id as usize]
    }
}

impl fmt::Display for SourceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let SourceId(id) = *self;
        write!(f, "{}", id)
    }
}
