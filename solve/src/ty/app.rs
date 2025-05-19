use core::fmt;
use std::sync::Arc;

use crate::Tag;

use super::Type;

/// A type application.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct App {
    /// The tag of the type application.
    tag: Tag,

    /// The type arguments of the type application.
    args: Arc<[Type]>,
}

impl App {
    /// Create a new type application.
    pub fn new(tag: Tag, args: impl Into<Arc<[Type]>>) -> Self {
        let args = args.into();
        Self { tag, args }
    }

    /// Get the tag of the type application.
    pub fn tag(&self) -> Tag {
        self.tag
    }

    /// Get the type arguments of the type application.
    pub fn args(&self) -> &[Type] {
        &self.args
    }
}

impl fmt::Display for App {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        match self.args.is_empty() {
            true => write!(f, "{}", self.tag.name()),
            false => write!(f, "{}<{}>", self.tag.name(), args),
        }
    }
}
