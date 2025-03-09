use std::ffi::CString;

use llvm_sys::{core::*, prelude::*};

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn place(&self, place: &sir::Place) -> LLVMValueRef {
        let mut local = self.llvm_body().locals[place.local.0];
        let mut tid = self.body().locals[place.local];

        for (proj, result_tid) in &place.proj {
            match proj {
                sir::Proj::Field(name) => {
                    let ty = self.codegen.tid(tid);

                    let index = match self.codegen.program.types[tid] {
                        sir::Ty::Record(ref fields) => {
                            fields.iter().position(|(n, _)| n == name).unwrap() as u32
                        }
                        _ => unreachable!(),
                    };

                    let name = CString::new(*name).unwrap();
                    local = LLVMBuildLoad2(self.builder, ty, local, name.as_ptr());

                    let inner = self.codegen.types[&tid].1.expect("expected inner type");
                    local = LLVMBuildStructGEP2(self.builder, inner, local, index, name.as_ptr());
                }

                sir::Proj::Tuple(index) => {
                    let ty = self.codegen.tid(tid);

                    let name = CString::new(format!("tuple_{}", index)).unwrap();
                    local = LLVMBuildLoad2(self.builder, ty, local, name.as_ptr());

                    let inner = self.codegen.types[&tid].1.expect("expected a inner type");
                    local = LLVMBuildStructGEP2(
                        self.builder,
                        inner,
                        local,
                        *index as u32,
                        name.as_ptr(),
                    );
                }

                sir::Proj::Index(_) => todo!(),

                sir::Proj::Deref => todo!(),
            }

            tid = *result_tid;
        }

        local
    }
}
