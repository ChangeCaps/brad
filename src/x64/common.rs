use std::ops::{Index, IndexMut};

use super::{reg::Reg, types::Tid};

#[derive(Debug, Clone, Copy)]
pub enum MemScale {
    S1,
    S2,
    S4,
    S8,
}

#[derive(Debug, Clone, Copy)]
pub enum RegConstraint {
    Int,
    Float,
    Specific(Reg),
}

#[derive(Debug, Clone, Copy)]
pub enum SizeKind {
    S8,
    S16,
    S32,
    S64,
}

#[derive(Debug, Clone, Copy)]
pub enum SizeConstraint {
    Any,
    Max(SizeKind),
    Specific(SizeKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vid(pub u32);

impl Vid {
    pub fn usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct VLocal {
    elems: Vec<(Tid, VLocation)>,
}

#[derive(Debug, Clone)]
pub struct VLocals(Vec<VLocal>);

#[derive(Debug, Clone, Copy)]
pub enum VLocation {
    None,
    Stack(i32),
    Reg(Reg),
}

#[derive(Debug, Clone, Copy)]
pub struct VRegister {
    elem: u32,
    vid: Vid,
}

impl VLocal {
    pub fn new(tids: &Vec<Tid>) -> Self {
        let mut elems = Vec::new();

        for tid in tids {
            elems.push((tid.clone(), VLocation::None));
        }

        Self { elems }
    }

    pub fn locate(&self, elem_idx: usize) -> VLocation {
        self.elems[elem_idx].1
    }

    pub fn relocate(&mut self, elem_idx: usize, location: VLocation) {
        self.elems[elem_idx].1 = location;
    }
}

impl Index<usize> for VLocal {
    type Output = (Tid, VLocation);

    fn index(&self, index: usize) -> &Self::Output {
        &self.elems[index]
    }
}

impl VLocals {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

impl Index<Vid> for VLocals {
    type Output = VLocal;

    fn index(&self, index: Vid) -> &Self::Output {
        &self.0[index.usize()]
    }
}

impl IndexMut<Vid> for VLocals {
    fn index_mut(&mut self, index: Vid) -> &mut Self::Output {
        &mut self.0[index.usize()]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LocalId(u32);

#[derive(Debug, Clone, Copy)]
pub struct GlobalId(u32);

#[derive(Debug, Clone, Copy)]
pub enum ComptimeVal {
    Label(LocalId),
    TypeId(GlobalId),
    Function(GlobalId),
    DataLocation(GlobalId),
}

#[derive(Debug, Clone, Copy)]
pub enum Imm32 {
    Const(i32),
    Comptime(ComptimeVal),
}

#[derive(Debug, Clone, Copy)]
pub enum Imm64 {
    Const(i64),
    Comptime(ComptimeVal),
}
