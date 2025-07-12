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
    None,
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
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,
    None,
}

#[derive(Debug, Clone, Copy)]
pub struct ComptimeVal {
    id: u32,
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
