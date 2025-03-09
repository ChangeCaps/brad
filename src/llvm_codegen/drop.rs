use llvm_sys::{core::*, prelude::*};

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn drop(&mut self, value: LLVMValueRef, tid: sir::Tid) {
        match self.program.types[tid] {
            sir::Ty::Int
            | sir::Ty::Float
            | sir::Ty::Str
            | sir::Ty::True
            | sir::Ty::False
            | sir::Ty::None
            | sir::Ty::Never => {}

            sir::Ty::Ref(_)
            | sir::Ty::List(_)
            | sir::Ty::Tuple(_)
            | sir::Ty::Record(_)
            | sir::Ty::Union(_) => {
                //self.release(value);
            }

            sir::Ty::Func(_, _) => {
                //todo!();
            }
        }
    }
}
