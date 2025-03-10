use std::collections::BTreeSet;

use llvm_sys::{core::*, prelude::*};

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn alloc_record(&mut self, fields: &[sir::Tid]) -> LLVMValueRef {
        let mut tys: Vec<_> = fields.iter().map(|t| self.tid(*t)).collect();

        let record_ty = LLVMStructTypeInContext(
            self.context,
            tys.as_mut_ptr(),
            tys.len() as u32, //
            0,
        );

        let marker = self.record_marker(fields);

        self.alloc(LLVMSizeOf(record_ty), marker)
    }

    pub unsafe fn alloc_union(
        &mut self,
        variant: sir::Tid,
        _variants: &BTreeSet<sir::Tid>,
    ) -> LLVMValueRef {
        let union_ty = LLVMStructTypeInContext(
            self.context,
            [
                self.i64_type(),   // variant
                self.tid(variant), // value
            ]
            .as_mut_ptr(),
            2,
            0,
        );

        let marker = self.union_marker(variant);

        self.alloc(LLVMSizeOf(union_ty), marker)
    }

    pub unsafe fn alloc_closure(&mut self, body: sir::Bid) -> LLVMValueRef {
        let closure_ty = LLVMStructTypeInContext(
            self.context,
            [
                self.void_pointer_type(),       // function pointer
                self.i64_type(),                // missing
                self.bodies[&body].captures_ty, // captures
            ]
            .as_mut_ptr(),
            3,
            0,
        );

        let marker = self.closure_marker(body);

        self.alloc(LLVMSizeOf(closure_ty), marker)
    }

    pub unsafe fn record_marker(&mut self, field_tids: &[sir::Tid]) -> LLVMValueRef {
        let current = LLVMGetInsertBlock(self.builder);

        let func = LLVMAddFunction(self.module, c"record_marker".as_ptr(), self.gc.marker_ty);

        let entry = LLVMAppendBasicBlock(func, c"entry".as_ptr());

        LLVMPositionBuilderAtEnd(self.builder, entry);

        let input = LLVMGetParam(func, 0);

        let mut field_tys: Vec<_> = field_tids.iter().map(|t| self.tid(*t)).collect();

        let record = LLVMStructTypeInContext(
            self.context,
            field_tys.as_mut_ptr(),
            field_tys.len() as u32, //
            0,
        );

        for (i, tid) in field_tids.iter().enumerate() {
            let value = LLVMBuildStructGEP2(
                self.builder,
                record,
                input,
                i as u32, // field
                c"mark_field".as_ptr(),
            );

            let value = LLVMBuildLoad2(
                self.builder,
                LLVMTypeOf(value), //
                value,
                c"value".as_ptr(),
            );

            self.mark_tid(value, *tid);
        }

        LLVMBuildRetVoid(self.builder);

        LLVMPositionBuilderAtEnd(self.builder, current);

        func
    }

    pub unsafe fn union_marker(&mut self, variant: sir::Tid) -> LLVMValueRef {
        let current = LLVMGetInsertBlock(self.builder);

        let func = LLVMAddFunction(self.module, c"union_marker".as_ptr(), self.gc.marker_ty);

        let entry = LLVMAppendBasicBlock(func, c"entry".as_ptr());

        LLVMPositionBuilderAtEnd(self.builder, entry);

        let union_ty = LLVMStructTypeInContext(
            self.context,
            [
                self.i64_type(), // variant
                self.tid(variant),
            ]
            .as_mut_ptr(),
            2,
            0,
        );

        let value = LLVMGetParam(func, 0);

        let value = LLVMBuildStructGEP2(
            self.builder,
            union_ty,
            value,
            1, // variant
            c"value".as_ptr(),
        );

        self.mark_tid(value, variant);

        LLVMBuildRetVoid(self.builder);

        LLVMPositionBuilderAtEnd(self.builder, current);

        func
    }

    pub unsafe fn closure_marker(&mut self, body: sir::Bid) -> LLVMValueRef {
        let current = LLVMGetInsertBlock(self.builder);

        let func = LLVMAddFunction(self.module, c"closure_marker".as_ptr(), self.gc.marker_ty);

        let entry = LLVMAppendBasicBlock(func, c"entry".as_ptr());

        LLVMPositionBuilderAtEnd(self.builder, entry);

        LLVMBuildRetVoid(self.builder);

        LLVMPositionBuilderAtEnd(self.builder, current);

        func
    }

    pub unsafe fn str_marker(&mut self) -> LLVMValueRef {
        self.build_str_marker()
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

    pub unsafe fn mark_tid(&mut self, value: LLVMValueRef, tid: sir::Tid) {
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
                self.mark(value);
            }
        }
    }

    pub unsafe fn alloc(&mut self, count: LLVMValueRef, marker: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildCall2(
            self.builder,
            self.gc.alloc_ty,
            self.gc.alloc,
            [count, marker].as_mut_ptr(),
            2,
            c"alloc".as_ptr(),
        )
    }

    pub unsafe fn retain(&mut self, value: LLVMValueRef) {
        LLVMBuildCall2(
            self.builder,
            self.gc.retain_ty,
            self.gc.retain,
            [value].as_mut_ptr(),
            1,
            c"".as_ptr(),
        );
    }

    pub unsafe fn release(&mut self, value: LLVMValueRef) {
        LLVMBuildCall2(
            self.builder,
            self.gc.release_ty,
            self.gc.release,
            [value].as_mut_ptr(),
            1,
            c"".as_ptr(),
        );
    }

    pub unsafe fn collect(&mut self) {
        LLVMBuildCall2(
            self.builder,
            self.gc.collect_ty,
            self.gc.collect,
            [].as_mut_ptr(),
            0,
            c"".as_ptr(),
        );
    }

    pub unsafe fn mark(&mut self, value: LLVMValueRef) {
        LLVMBuildCall2(
            self.builder,
            self.gc.mark_ty,
            self.gc.mark,
            [value].as_mut_ptr(),
            1,
            c"".as_ptr(),
        );
    }
}
