use crate::hir2 as hir;

const LUA_PRELUDE: &str = include_str!("prelude.lua");

pub fn codegen(_hir: hir::Program) -> String {
    todo!()
}
