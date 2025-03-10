use llvm_sys::{core::*, prelude::*};

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn operand(&mut self, operand: &sir::Operand) -> LLVMValueRef {
        match operand {
            sir::Operand::Copy(place) => {
                let &tid = place.ty(&self.body().locals);
                let ty = self.codegen.tid(tid);

                let place = self.place(place);

                let operand = LLVMBuildLoad2(self.builder, ty, place, c"load".as_ptr());
                self.copy(operand, tid)
            }

            sir::Operand::Move(place) => {
                let place = self.place(place);

                LLVMBuildLoad2(self.builder, LLVMTypeOf(place), place, c"load".as_ptr())
            }

            sir::Operand::Const(r#const, _) => match r#const {
                sir::Const::None => self.codegen.zero_size_value(),

                sir::Const::Int(value) => {
                    LLVMConstInt(LLVMInt64TypeInContext(self.context), *value as u64, 0)
                }

                sir::Const::Float(value) => {
                    LLVMConstReal(LLVMFloatTypeInContext(self.context), *value)
                }

                sir::Const::String(value) => {
                    let i64_size = LLVMSizeOf(self.i64_type());
                    let len = LLVMConstInt(self.i64_type(), value.len() as u64, 0);

                    let total_len = LLVMBuildAdd(self.builder, i64_size, len, c"len".as_ptr());

                    let marker = self.str_marker();
                    let string = self.alloc(total_len, marker);

                    LLVMBuildStore(self.builder, len, string);

                    let contents = LLVMConstStringInContext2(
                        self.context,
                        value.as_ptr() as *const i8,
                        value.len(),
                        0,
                    );

                    let bytes = LLVMAddGlobal(
                        self.module,
                        LLVMTypeOf(contents), // type
                        c"string_bytes".as_ptr(),
                    );

                    LLVMSetInitializer(bytes, contents);

                    let pointer = LLVMBuildGEP2(
                        self.builder,
                        LLVMInt8TypeInContext(self.context),
                        string,
                        [i64_size].as_mut_ptr(),
                        1,
                        c"str_ptr".as_ptr(),
                    );

                    LLVMBuildMemCpy(self.builder, pointer, 1, bytes, 1, len);

                    string
                }
            },
        }
    }
}
