use llvm_sys::prelude::*;

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn copy(&mut self, value: LLVMValueRef, tid: sir::Tid) {
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
            | sir::Ty::Tuple(_)
            | sir::Ty::Record(_)
            | sir::Ty::Union(_) => {
                self.retain(value);
            }

            sir::Ty::Func(_, _) => {
                panic!("cannot drop a function");
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
            | sir::Ty::Tuple(_)
            | sir::Ty::Record(_)
            | sir::Ty::Union(_) => {
                self.release(value);
            }

            sir::Ty::Func(_, _) => {
                panic!("cannot drop a function");
            }
        }
    }
}
