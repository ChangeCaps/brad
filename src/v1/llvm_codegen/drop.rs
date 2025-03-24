use super::llvm_sys::prelude::*;

use super::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn copy(&mut self, value: LLVMValueRef, tid: sir::Tid) -> LLVMValueRef {
        match self.program.types[tid] {
            sir::Ty::Int
            | sir::Ty::Float
            | sir::Ty::True
            | sir::Ty::False
            | sir::Ty::None
            | sir::Ty::Never => value,

            sir::Ty::Str
            | sir::Ty::Ref(_)
            | sir::Ty::List(_)
            | sir::Ty::Func(_, _)
            | sir::Ty::Tuple(_)
            | sir::Ty::Record(_)
            | sir::Ty::Union(_) => {
                self.retain(value);
                value
            }
        }
    }

    pub unsafe fn drop(&mut self, value: LLVMValueRef, tid: sir::Tid) {
        match self.program.types[tid] {
            sir::Ty::Int
            | sir::Ty::Float
            | sir::Ty::True
            | sir::Ty::False
            | sir::Ty::None
            | sir::Ty::Never => {}

            sir::Ty::Str
            | sir::Ty::Ref(_)
            | sir::Ty::List(_)
            | sir::Ty::Func(_, _)
            | sir::Ty::Tuple(_)
            | sir::Ty::Record(_)
            | sir::Ty::Union(_) => {
                self.release(value);
            }
        }
    }
}
