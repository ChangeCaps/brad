use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasherDefault,
};

use seahash::SeaHasher;

use crate::{Bounds, Type, Var};

type SeaHashMap<K, V> = HashMap<K, V, BuildHasherDefault<SeaHasher>>;
type SeaHashSet<K> = HashSet<K, BuildHasherDefault<SeaHasher>>;

#[derive(Clone, Default)]
pub struct Tcx {
    bounds: SeaHashMap<Var, Bounds>,
    cache: SeaHashSet<Var>,
}

impl Tcx {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn subtype(&mut self, lhs: Type, rhs: Type) {
        let l = lhs.inter(rhs.neg());
        println!("lhs: {l}");

        todo!()
    }
}
