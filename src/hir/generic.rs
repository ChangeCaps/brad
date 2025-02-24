use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Clone, Debug, Default)]
pub struct Generics {
    pub params: Vec<Param>,
}

impl Generics {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: &'static str,
    pub generic: Generic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Generic {
    index: usize,
}

impl Generic {
    pub fn new() -> Self {
        static INDEX: AtomicUsize = AtomicUsize::new(0);
        let index = INDEX.fetch_add(1, Ordering::SeqCst);
        Self { index }
    }
}
