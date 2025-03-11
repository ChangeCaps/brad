use super::SourceId;
use std::fmt;

#[derive(Clone, Copy)]
pub struct Span {
    pub source: SourceId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(source: SourceId, start: usize, end: usize) -> Span {
        Span { source, start, end }
    }

    pub fn join(self, other: Self) -> Self {
        Self {
            source: self.source,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}..{}", self.source, self.start, self.end)
    }
}
