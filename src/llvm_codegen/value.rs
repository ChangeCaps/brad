use std::collections::BTreeSet;

use llvm_sys::{core::*, prelude::*, LLVMIntPredicate, LLVMRealPredicate, LLVMTypeKind};

use crate::sir;

use super::BodyCodegen;

impl BodyCodegen<'_> {
    pub unsafe fn value(&mut self, value: &sir::Value) -> LLVMValueRef {
        match value {
            sir::Value::Use(operand) => self.operand(operand),

            sir::Value::Tuple(items) => {
                let mut values = Vec::new();
                let mut types = Vec::new();

                for operand in items {
                    values.push(self.operand(operand));
                    types.push(self.tid(*operand.ty(&self.body().locals)));
                }

                let ty = LLVMStructTypeInContext(
                    self.context,
                    types.as_mut_ptr(),
                    types.len() as u32,
                    0,
                );

                let tids = items
                    .iter()
                    .map(|item| *item.ty(&self.body().locals))
                    .collect::<Vec<_>>();

                let ptr = self.alloc_record(&tids);

                for (i, value) in values.iter().enumerate() {
                    let field = LLVMBuildStructGEP2(
                        self.builder,
                        ty,
                        ptr,      // struct memory
                        i as u32, // field index
                        c"field".as_ptr(),
                    );

                    LLVMBuildStore(self.builder, *value, field);

                    let tid = items[i].ty(&self.body().locals);
                    self.drop(*value, *tid);
                }

                ptr
            }

            sir::Value::Record(fields) => {
                if fields.is_empty() {
                    return self.zero_size_value();
                }

                let mut values = Vec::new();
                let mut types = Vec::new();

                for (_, operand) in fields {
                    values.push(self.operand(operand));
                    types.push(self.tid(*operand.ty(&self.body().locals)));
                }

                let ty = LLVMStructTypeInContext(
                    self.context,
                    types.as_mut_ptr(),
                    types.len() as u32,
                    0,
                );

                let tids = fields
                    .iter()
                    .map(|(_, ty)| *ty.ty(&self.body().locals))
                    .collect::<Vec<_>>();

                let ptr = self.alloc_record(&tids);

                for (i, value) in values.iter().enumerate() {
                    let field = LLVMBuildStructGEP2(
                        self.builder,
                        ty,
                        ptr,      // struct memory
                        i as u32, // field index
                        c"field".as_ptr(),
                    );

                    LLVMBuildStore(self.builder, *value, field);

                    let tid = fields[i].1.ty(&self.body().locals);
                    self.drop(*value, *tid);
                }

                ptr
            }

            sir::Value::Promote {
                variant,
                variants,
                operand,
            } => {
                let operand = self.operand(operand);

                let union = LLVMStructTypeInContext(
                    self.context,
                    [self.i64_type(), LLVMTypeOf(operand)].as_mut_ptr(),
                    2,
                    0,
                );

                let ptr = self.alloc_union(*variant, variants);

                let tag_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    union,
                    ptr,
                    0, // tag
                    c"tag".as_ptr(),
                );

                let tag = LLVMConstInt(
                    self.i64_type(),
                    variant.0 as u64,
                    0, // sign extend
                );

                LLVMBuildStore(self.builder, tag, tag_ptr);

                let value_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    union,
                    ptr,
                    1, // value
                    c"value".as_ptr(),
                );

                LLVMBuildStore(self.builder, operand, value_ptr);

                LLVMBuildPointerCast(
                    self.builder,
                    ptr, // value
                    self.void_pointer_type(),
                    c"promote".as_ptr(),
                )
            }

            sir::Value::Coerce { .. } => todo!(),

            sir::Value::Call(func, input) => {
                let func_tid = *func.ty(&self.body().locals);

                let output_ty = match self.program.types[func_tid] {
                    sir::Ty::Func(_, output) => self.codegen.tid(output),
                    _ => unreachable!(),
                };

                let func_ty = LLVMStructTypeInContext(
                    self.context,
                    [
                        self.i64_type(),          // allocation size
                        self.i64_type(),          // missing
                        self.void_pointer_type(), // function pointer
                        self.zero_size_type(),    // captures
                    ]
                    .as_mut_ptr(),
                    4,
                    0,
                );

                let old_func = self.place(func);
                let old_func = LLVMBuildLoad2(
                    self.builder,
                    LLVMTypeOf(old_func),
                    old_func,
                    c"func".as_ptr(),
                );

                let func_size_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    func_ty,
                    old_func,
                    0, // size
                    c"func_size".as_ptr(),
                );
                let func_size = LLVMBuildLoad2(
                    self.builder,
                    self.i64_type(),
                    func_size_ptr,
                    c"func_size".as_ptr(),
                );

                let marker = self.get_marker(old_func);
                let func = self.alloc(func_size, marker, "call");

                LLVMBuildMemCpy(self.builder, func, 1, old_func, 1, func_size);

                let input_tid = *input.ty(&self.body().locals);
                let input_ty = self.codegen.tid(input_tid);
                let input = self.operand(input);

                let missing_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    func_ty,
                    func,
                    1, // missing
                    c"func_missing_ptr".as_ptr(),
                );

                let new_missing = LLVMBuildSub(
                    self.builder,
                    LLVMBuildLoad2(
                        self.builder,
                        LLVMInt64TypeInContext(self.context),
                        missing_ptr,
                        c"func_missing".as_ptr(),
                    ),
                    LLVMSizeOf(input_ty),
                    c"new_missing".as_ptr(),
                );

                let captures_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    func_ty,
                    func,
                    3, // captures
                    c"func_captures_ptr".as_ptr(),
                );

                let input_ptr = LLVMBuildGEP2(
                    self.builder,
                    LLVMInt8TypeInContext(self.context),
                    captures_ptr,
                    [new_missing].as_mut_ptr(),
                    1,
                    c"input_location".as_ptr(),
                );

                LLVMBuildStore(self.builder, input, input_ptr);
                LLVMBuildStore(self.builder, new_missing, missing_ptr);

                self.drop(input, input_tid);

                let call_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body().function,
                    c"call".as_ptr(),
                );

                let append_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body().function,
                    c"append".as_ptr(),
                );

                let end_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body().function,
                    c"end".as_ptr(),
                );

                let call = LLVMBuildICmp(
                    self.builder,
                    LLVMIntPredicate::LLVMIntEQ,
                    new_missing,
                    LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0),
                    c"cmp".as_ptr(),
                );

                let output = LLVMBuildAlloca(self.builder, output_ty, c"call_output".as_ptr());

                LLVMBuildCondBr(self.builder, call, call_block, append_block);

                /* call block */

                LLVMPositionBuilderAtEnd(self.builder, call_block);

                let func_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    func_ty,
                    func,
                    2, // function pointer
                    c"func_ptr".as_ptr(),
                );

                let func_ptr = LLVMBuildLoad2(
                    self.builder,
                    self.codegen.void_pointer_type(),
                    func_ptr,
                    c"load".as_ptr(),
                );

                let result = LLVMBuildCall2(
                    self.builder,
                    LLVMFunctionType(output_ty, [self.void_pointer_type()].as_mut_ptr(), 1, 0),
                    func_ptr,
                    [captures_ptr].as_mut_ptr(),
                    1,
                    c"call".as_ptr(),
                );

                if LLVMGetTypeKind(output_ty) != LLVMTypeKind::LLVMVoidTypeKind {
                    LLVMBuildStore(self.builder, result, output);
                }

                self.drop(func, func_tid);

                LLVMBuildBr(self.builder, end_block);

                /* append block */

                LLVMPositionBuilderAtEnd(self.builder, append_block);

                LLVMBuildStore(self.builder, func, output);
                LLVMBuildBr(self.builder, end_block);

                /* end block */

                LLVMPositionBuilderAtEnd(self.builder, end_block);

                LLVMBuildLoad2(self.builder, output_ty, output, c"load".as_ptr())
            }

            sir::Value::Binary(op, lhs, rhs) => {
                let lhs_ty = *lhs.ty(&self.body().locals);
                let rhs_ty = *rhs.ty(&self.body().locals);

                assert_eq!(lhs_ty, rhs_ty);

                let lhs = self.operand(lhs);
                let rhs = self.operand(rhs);

                match op {
                    sir::BinaryOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, c"add".as_ptr()),
                    sir::BinaryOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, c"sub".as_ptr()),
                    sir::BinaryOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, c"mul".as_ptr()),
                    sir::BinaryOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, c"div".as_ptr()),
                    sir::BinaryOp::Mod => LLVMBuildSRem(self.builder, lhs, rhs, c"mod".as_ptr()),
                    sir::BinaryOp::BAnd => LLVMBuildAnd(self.builder, lhs, rhs, c"and".as_ptr()),
                    sir::BinaryOp::BOr => LLVMBuildOr(self.builder, lhs, rhs, c"or".as_ptr()),
                    sir::BinaryOp::BXor => LLVMBuildXor(self.builder, lhs, rhs, c"xor".as_ptr()),
                    sir::BinaryOp::Eq => {
                        let eq = self.eq(lhs, rhs, lhs_ty);

                        self.drop(lhs, lhs_ty);
                        self.drop(rhs, rhs_ty);

                        self.int_to_bool(eq)
                    }
                    sir::BinaryOp::Ne => {
                        let eq = self.eq(lhs, rhs, lhs_ty);

                        self.drop(lhs, lhs_ty);
                        self.drop(rhs, rhs_ty);

                        self.int_to_bool(LLVMBuildNot(self.builder, eq, c"ne".as_ptr()))
                    }
                    sir::BinaryOp::Lt => self.int_to_bool(LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        lhs,
                        rhs,
                        c"lt".as_ptr(),
                    )),
                    sir::BinaryOp::Le => self.int_to_bool(LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLE,
                        lhs,
                        rhs,
                        c"le".as_ptr(),
                    )),
                    sir::BinaryOp::Gt => self.int_to_bool(LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGT,
                        lhs,
                        rhs,
                        c"gt".as_ptr(),
                    )),
                    sir::BinaryOp::Ge => self.int_to_bool(LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGE,
                        lhs,
                        rhs,
                        c"ge".as_ptr(),
                    )),
                    sir::BinaryOp::LShr => LLVMBuildAShr(self.builder, lhs, rhs, c"shr".as_ptr()),
                    sir::BinaryOp::LShl => LLVMBuildShl(self.builder, lhs, rhs, c"shr".as_ptr()),
                    sir::BinaryOp::FAdd => LLVMBuildFAdd(self.builder, lhs, rhs, c"fadd".as_ptr()),
                    sir::BinaryOp::FSub => LLVMBuildFSub(self.builder, lhs, rhs, c"fsub".as_ptr()),
                    sir::BinaryOp::FMul => LLVMBuildFMul(self.builder, lhs, rhs, c"fmul".as_ptr()),
                    sir::BinaryOp::FDiv => LLVMBuildFDiv(self.builder, lhs, rhs, c"fdiv".as_ptr()),
                    sir::BinaryOp::FMod => LLVMBuildFRem(self.builder, lhs, rhs, c"fmod".as_ptr()),
                    sir::BinaryOp::FLt => self.int_to_bool(LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOLT,
                        lhs,
                        rhs,
                        c"flt".as_ptr(),
                    )),
                    sir::BinaryOp::FLe => self.int_to_bool(LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOLE,
                        lhs,
                        rhs,
                        c"fle".as_ptr(),
                    )),
                    sir::BinaryOp::FGt => self.int_to_bool(LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOGT,
                        lhs,
                        rhs,
                        c"fgt".as_ptr(),
                    )),
                    sir::BinaryOp::FGe => self.int_to_bool(LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOGE,
                        lhs,
                        rhs,
                        c"fge".as_ptr(),
                    )),
                    sir::BinaryOp::And => todo!("implement runtime booleans"),
                    sir::BinaryOp::Or => todo!("implement runtime booleans"),
                }
            }

            sir::Value::Unary(op, operand) => {
                let operand = self.operand(operand);
                match op {
                    sir::UnaryOp::Neg => LLVMBuildNeg(self.builder, operand, c"neg".as_ptr()),
                    sir::UnaryOp::FNeg => LLVMBuildFNeg(self.builder, operand, c"fneg".as_ptr()),
                    sir::UnaryOp::BNot => LLVMBuildNot(self.builder, operand, c"not".as_ptr()),
                    sir::UnaryOp::Not => todo!("implement runtime booleans"),
                    sir::UnaryOp::Deref => todo!(),
                }
            }

            sir::Value::Closure { body, .. } => {
                let closure_ty = LLVMStructTypeInContext(
                    self.context,
                    [
                        self.i64_type(),               // allocation size
                        self.i64_type(),               // missing captures
                        self.void_pointer_type(),      // function pointer
                        self.bodies[body].captures_ty, // captures
                    ]
                    .as_mut_ptr(),
                    4,
                    0,
                );

                let closure_ptr = self.alloc_closure(*body);

                let size_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure_ptr,
                    0, // size
                    c"size_ptr".as_ptr(),
                );

                LLVMBuildStore(
                    self.builder,
                    LLVMSizeOf(closure_ty), // allocation size
                    size_ptr,
                );

                let missing_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure_ptr,
                    1, // missing
                    c"missing_ptr".as_ptr(),
                );

                LLVMBuildStore(
                    self.builder,
                    LLVMSizeOf(self.bodies[body].captures_ty), // missing captures
                    missing_ptr,
                );

                let function_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure_ptr,
                    2, // function pointer
                    c"function_ptr".as_ptr(),
                );

                LLVMBuildStore(
                    self.builder,
                    self.bodies[body].function, // function pointer
                    function_ptr,
                );

                closure_ptr
            }
        }
    }

    unsafe fn eq(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, tid: sir::Tid) -> LLVMValueRef {
        match self.program.types[tid] {
            sir::Ty::Int => LLVMBuildICmp(
                self.builder,
                LLVMIntPredicate::LLVMIntEQ,
                lhs,
                rhs,
                c"eq".as_ptr(),
            ),

            sir::Ty::Float => LLVMBuildFCmp(
                self.builder,
                LLVMRealPredicate::LLVMRealOEQ,
                lhs,
                rhs,
                c"eq".as_ptr(),
            ),

            sir::Ty::True | sir::Ty::False | sir::Ty::None | sir::Ty::Never => {
                // zero-sized types are always equal

                LLVMConstInt(self.i64_type(), 1, 0)
            }

            sir::Ty::Str => {
                todo!("string equality")
            }

            sir::Ty::Ref(_) => todo!("reference equality"),
            sir::Ty::List(_) => todo!("list equality"),

            sir::Ty::Func(_, _) => self.func_eq(lhs, rhs),

            sir::Ty::Tuple(ref items) => {
                let mut eq = LLVMConstInt(self.i64_type(), 1, 0);

                for (i, item) in items.clone().into_iter().enumerate() {
                    let lhs_field = LLVMBuildStructGEP2(
                        self.builder,
                        LLVMTypeOf(lhs),
                        lhs,
                        i as u32,
                        c"lhs_field".as_ptr(),
                    );

                    let rhs_field = LLVMBuildStructGEP2(
                        self.builder,
                        LLVMTypeOf(rhs),
                        rhs,
                        i as u32,
                        c"rhs_field".as_ptr(),
                    );

                    let field_eq = self.eq(lhs_field, rhs_field, item);

                    eq = LLVMBuildAnd(self.builder, eq, field_eq, c"eq".as_ptr());
                }

                eq
            }

            sir::Ty::Record(ref fields) => {
                let mut eq = LLVMConstInt(self.i64_type(), 1, 0);

                for (i, (_, ty)) in fields.clone().into_iter().enumerate() {
                    let lhs_field = LLVMBuildStructGEP2(
                        self.builder,
                        LLVMTypeOf(lhs),
                        lhs,
                        i as u32,
                        c"lhs_field".as_ptr(),
                    );

                    let rhs_field = LLVMBuildStructGEP2(
                        self.builder,
                        LLVMTypeOf(rhs),
                        rhs,
                        i as u32,
                        c"rhs_field".as_ptr(),
                    );

                    let field_eq = self.eq(lhs_field, rhs_field, ty);

                    eq = LLVMBuildAnd(self.builder, eq, field_eq, c"eq".as_ptr());
                }

                eq
            }

            sir::Ty::Union(_) => todo!("union equality"),
        }
    }

    unsafe fn func_eq(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        let func_ty = LLVMStructTypeInContext(
            self.context,
            [
                self.i64_type(),          // allocation size
                self.i64_type(),          // missing
                self.void_pointer_type(), // function pointer
                self.zero_size_type(),    // captures
            ]
            .as_mut_ptr(),
            4,
            0,
        );

        let lhs_missing_ptr = LLVMBuildStructGEP2(
            self.builder,
            func_ty,
            lhs,
            1, // missing
            c"lhs_missing_ptr".as_ptr(),
        );

        let lhs_missing = LLVMBuildLoad2(
            self.builder,
            self.i64_type(),
            lhs_missing_ptr,
            c"lhs_missing".as_ptr(),
        );

        let rhs_missing_ptr = LLVMBuildStructGEP2(
            self.builder,
            func_ty,
            rhs,
            1, // missing
            c"rhs_missing_ptr".as_ptr(),
        );

        let rhs_missing = LLVMBuildLoad2(
            self.builder,
            self.i64_type(),
            rhs_missing_ptr,
            c"rhs_missing".as_ptr(),
        );

        let missing_eq = LLVMBuildICmp(
            self.builder,
            LLVMIntPredicate::LLVMIntEQ,
            lhs_missing,
            rhs_missing,
            c"missing_eq".as_ptr(),
        );

        let lhs_ptr = LLVMBuildStructGEP2(
            self.builder,
            func_ty,
            lhs,
            2, // function pointer
            c"lhs_ptr".as_ptr(),
        );

        let lhs_ptr = LLVMBuildLoad2(
            self.builder,
            self.void_pointer_type(),
            lhs_ptr,
            c"lhs_ptr".as_ptr(),
        );

        let rhs_ptr = LLVMBuildStructGEP2(
            self.builder,
            func_ty,
            rhs,
            2, // function pointer
            c"rhs_ptr".as_ptr(),
        );

        let rhs_ptr = LLVMBuildLoad2(
            self.builder,
            self.void_pointer_type(),
            rhs_ptr,
            c"rhs_ptr".as_ptr(),
        );

        let function_eq = LLVMBuildICmp(
            self.builder,
            LLVMIntPredicate::LLVMIntEQ,
            lhs_ptr,
            rhs_ptr,
            c"function_eq".as_ptr(),
        );

        LLVMBuildAnd(self.builder, missing_eq, function_eq, c"eq".as_ptr())
    }

    unsafe fn int_to_bool(&mut self, value: LLVMValueRef) -> LLVMValueRef {
        let true_tid = self.program.types[sir::Ty::True];
        let false_tid = self.program.types[sir::Ty::False];

        let bool_ptr = self.alloc_union(true_tid, &BTreeSet::from([true_tid, false_tid]));

        let true_block = LLVMAppendBasicBlockInContext(
            self.context,
            self.llvm_body().function,
            c"true".as_ptr(),
        );

        let false_block = LLVMAppendBasicBlockInContext(
            self.context,
            self.llvm_body().function,
            c"false".as_ptr(),
        );

        let end_block = LLVMAppendBasicBlockInContext(
            self.context,
            self.llvm_body().function,
            c"end".as_ptr(), //
        );

        LLVMBuildCondBr(self.builder, value, true_block, false_block);

        LLVMPositionBuilderAtEnd(self.builder, true_block);

        let true_tag = LLVMConstInt(self.i64_type(), true_tid.0 as u64, 0);
        LLVMBuildStore(self.builder, true_tag, bool_ptr);
        LLVMBuildBr(self.builder, end_block);

        LLVMPositionBuilderAtEnd(self.builder, false_block);

        let false_tag = LLVMConstInt(self.i64_type(), false_tid.0 as u64, 0);
        LLVMBuildStore(self.builder, false_tag, bool_ptr);
        LLVMBuildBr(self.builder, end_block);

        LLVMPositionBuilderAtEnd(self.builder, end_block);

        bool_ptr
    }
}
