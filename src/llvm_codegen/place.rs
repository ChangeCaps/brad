use std::ffi::CString;

use super::llvm_sys::{core::*, prelude::*};

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn place(&mut self, place: &sir::Place) -> LLVMValueRef {
        let mut local = self.llvm_body().locals[place.local.0];
        let mut tid = self.body().locals[place.local];

        for (proj, result_tid) in place.proj.clone() {
            match proj {
                sir::Proj::Field(name) => {
                    let ty = self.codegen.tid(tid);

                    let index = match self.codegen.program.types[tid] {
                        sir::Ty::Record(ref fields) => {
                            fields.iter().position(|(n, _)| *n == name).unwrap() as u32
                        }
                        _ => unreachable!(),
                    };

                    let name = CString::new(name).unwrap();
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
                        index as u32,
                        name.as_ptr(),
                    );
                }

                sir::Proj::Index(index) => {
                    let index = self.operand(&index);

                    let list_ty = LLVMStructTypeInContext(
                        self.codegen.context,
                        [
                            self.i64_type(),       // size
                            self.i64_type(),       // capacity
                            self.zero_size_type(), // data
                        ]
                        .as_mut_ptr(),
                        3,
                        0,
                    );

                    let ty = self.codegen.tid(tid);

                    local = LLVMBuildLoad2(self.builder, ty, local, c"array_ptr".as_ptr());
                    local = LLVMBuildStructGEP2(self.builder, list_ty, local, 2, c"data".as_ptr());
                    local = LLVMBuildGEP2(
                        self.builder,
                        ty,
                        local,
                        [index].as_mut_ptr(),
                        1,
                        c"index".as_ptr(),
                    );
                }

                sir::Proj::Deref => {
                    let ty = self.codegen.tid(tid);
                    local = LLVMBuildLoad2(self.builder, ty, local, c"deref".as_ptr());
                }
            }

            tid = result_tid;
        }

        local
    }
}
