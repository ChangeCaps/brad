use llvm_sys::{core::*, prelude::*};

use crate::sir;

use super::Codegen;

impl Codegen {
    pub unsafe fn marker(&mut self, tid: sir::Tid) -> LLVMValueRef {
        if let Some(marker) = self.markers.get(&tid) {
            return *marker;
        }

        let marker = match &self.program.types[tid] {
            sir::Ty::Int => todo!(),
            sir::Ty::Float => todo!(),
            sir::Ty::Str => self.build_str_marker(),
            sir::Ty::True => todo!(),
            sir::Ty::False => todo!(),
            sir::Ty::None => todo!(),
            sir::Ty::Never => todo!(),
            sir::Ty::Ref(_) => todo!(),
            sir::Ty::List(_) => todo!(),
            sir::Ty::Func(_, _) => todo!(),
            sir::Ty::Tuple(_) => todo!(),
            sir::Ty::Record(_) => todo!(),
            sir::Ty::Union(_) => todo!(),
        };

        self.markers.insert(tid, marker);

        marker
    }

    pub unsafe fn str_marker(&mut self) -> LLVMValueRef {
        let tid = self.program.types.get_or_insert(&sir::Ty::Str);
        self.marker(tid)
    }

    unsafe fn build_str_marker(&mut self) -> LLVMValueRef {
        let current = LLVMGetInsertBlock(self.builder);

        let func = LLVMAddFunction(self.module, c"str_marker".as_ptr(), self.gc.marker_ty);

        let entry = LLVMAppendBasicBlock(func, c"entry".as_ptr());
        LLVMPositionBuilderAtEnd(self.builder, entry);
        LLVMBuildRetVoid(self.builder);

        LLVMPositionBuilderAtEnd(self.builder, current);

        func
    }
}
