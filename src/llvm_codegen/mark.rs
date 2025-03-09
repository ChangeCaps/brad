use llvm_sys::{core::*, prelude::*};

use super::Codegen;

impl Codegen {
    pub unsafe fn str_marker(&mut self) -> LLVMValueRef {
        let current = LLVMGetInsertBlock(self.builder);

        let func = LLVMAddFunction(self.module, c"str_marker".as_ptr(), self.gc.marker_ty);

        let entry = LLVMAppendBasicBlock(func, c"entry".as_ptr());
        LLVMPositionBuilderAtEnd(self.builder, entry);
        LLVMBuildRetVoid(self.builder);

        LLVMPositionBuilderAtEnd(self.builder, current);

        func
    }
}
