use std::collections::BTreeSet;

use super::reg::Reg;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Tid(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,

    Function,

    Array,
    Union,
    Tuple,
    Record,

    None,
}

#[derive(Debug, Clone)]
pub enum FrontType {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BTid(pub u32);

#[derive(Debug, Clone)]
pub enum BackType {
    Bool,  // 1 byte
    Int,   // 8 bytes
    Float, // 8 bytes

    // 8 bytes (ptr)
    String,

    // 8 bytes (ptr)
    Function { args: Vec<BTid>, ret: Option<BTid> },

    // 8 bytes (ptr)
    Array { ty: BTid },
    // 4 bytes + max(sizeof(types))
    Union { types: BTreeSet<BTid> },
    // 8 bytes (ptr)
    Tuple { types: Vec<BTid> },
    // 8 bytes (ptr)
    Record { fields: Vec<(BTid, &'static str)> },

    None,
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
