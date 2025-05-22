use std::{
    fmt,
    sync::atomic::{self, AtomicU64},
};

use super::Type;

/// A [`Type`] variable.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    index: u64,
}

impl Var {
    /// Create a new type variable.
    pub fn fresh() -> Self {
        static NEXT_INDEX: AtomicU64 = AtomicU64::new(0);
        let index = NEXT_INDEX.fetch_add(1, atomic::Ordering::Relaxed);
        Self { index }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.index)
    }
}

/// The bounds of a [`Type`] [`Var`]iable.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bounds {
    /// The lower bound of the type variable.
    pub lower: Type,

    /// The upper bound of the type variable.
    pub upper: Type,
}

impl Default for Bounds {
    fn default() -> Self {
        Self {
            lower: Type::bottom(),
            upper: Type::top(),
        }
    }
}
