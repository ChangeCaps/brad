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

                let ptr = self.alloc_single(ty);

                for (i, value) in values.iter().enumerate() {
                    let field = LLVMBuildStructGEP2(
                        self.builder,
                        ty,
                        ptr,      // struct memory
                        i as u32, // field index
                        c"field".as_ptr(),
                    );

                    LLVMBuildStore(self.builder, *value, field);
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

                let ptr = self.alloc_single(ty);

                for (i, value) in values.iter().enumerate() {
                    let field = LLVMBuildStructGEP2(
                        self.builder,
                        ty,
                        ptr,      // struct memory
                        i as u32, // field index
                        c"field".as_ptr(),
                    );

                    LLVMBuildStore(self.builder, *value, field);
                }

                ptr
            }

            sir::Value::Promote {
                variant, operand, ..
            } => {
                let operand = self.operand(operand);

                let union = LLVMStructTypeInContext(
                    self.context,
                    [self.i64_type(), LLVMTypeOf(operand)].as_mut_ptr(),
                    2,
                    0,
                );

                let ptr = self.alloc_single(union);

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

                let func = self.place(func);
                let func = LLVMBuildLoad2(self.builder, LLVMTypeOf(func), func, c"func".as_ptr());

                let input_tid = *input.ty(&self.body().locals);
                let input_ty = self.codegen.tid(input_tid);
                let input = self.operand(input);

                let func_ty = LLVMStructTypeInContext(
                    self.context,
                    [
                        self.void_pointer_type(), // function pointer
                        self.i64_type(),          // missing
                        self.zero_size_type(),    // captures
                    ]
                    .as_mut_ptr(),
                    3,
                    0,
                );

                let captures = LLVMBuildStructGEP2(
                    self.builder,
                    func_ty,
                    func,
                    2, // captures
                    c"func_captures_ptr".as_ptr(),
                );

                let missing = LLVMBuildStructGEP2(
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
                        missing,
                        c"func_missing".as_ptr(),
                    ),
                    LLVMSizeOf(input_ty),
                    c"new_missing".as_ptr(),
                );

                let input_location = LLVMBuildGEP2(
                    self.builder,
                    LLVMInt8TypeInContext(self.context),
                    captures,
                    [new_missing].as_mut_ptr(),
                    1,
                    c"input_location".as_ptr(),
                );

                LLVMBuildStore(self.builder, input, input_location);
                LLVMBuildStore(self.builder, new_missing, missing);

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
                    0, // function pointer
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
                    [captures].as_mut_ptr(),
                    1,
                    c"call".as_ptr(),
                );

                if LLVMGetTypeKind(output_ty) != LLVMTypeKind::LLVMVoidTypeKind {
                    LLVMBuildStore(self.builder, result, output);
                }

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
                let lhs = self.operand(lhs);
                let rhs = self.operand(rhs);

                match op {
                    sir::BinaryOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, c"add".as_ptr()),
                    sir::BinaryOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, c"sub".as_ptr()),
                    sir::BinaryOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, c"mul".as_ptr()),
                    sir::BinaryOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, c"div".as_ptr()),
                    sir::BinaryOp::Mod => LLVMBuildSRem(self.builder, lhs, rhs, c"mod".as_ptr()),
                    sir::BinaryOp::BAnd => todo!(),
                    sir::BinaryOp::BOr => todo!(),
                    sir::BinaryOp::BXor => todo!(),
                    sir::BinaryOp::Eq => LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        lhs,
                        rhs,
                        c"eq".as_ptr(),
                    ),
                    sir::BinaryOp::Ne => LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntNE,
                        lhs,
                        rhs,
                        c"ne".as_ptr(),
                    ),
                    sir::BinaryOp::Lt => LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        lhs,
                        rhs,
                        c"lt".as_ptr(),
                    ),
                    sir::BinaryOp::Le => LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLE,
                        lhs,
                        rhs,
                        c"le".as_ptr(),
                    ),
                    sir::BinaryOp::Gt => LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGT,
                        lhs,
                        rhs,
                        c"gt".as_ptr(),
                    ),
                    sir::BinaryOp::Ge => LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGE,
                        lhs,
                        rhs,
                        c"ge".as_ptr(),
                    ),
                    sir::BinaryOp::LShr => LLVMBuildAShr(self.builder, lhs, rhs, c"shr".as_ptr()),
                    sir::BinaryOp::LShl => LLVMBuildShl(self.builder, lhs, rhs, c"shr".as_ptr()),
                    sir::BinaryOp::FAdd => LLVMBuildFAdd(self.builder, lhs, rhs, c"fadd".as_ptr()),
                    sir::BinaryOp::FSub => LLVMBuildFSub(self.builder, lhs, rhs, c"fsub".as_ptr()),
                    sir::BinaryOp::FMul => LLVMBuildFMul(self.builder, lhs, rhs, c"fmul".as_ptr()),
                    sir::BinaryOp::FDiv => LLVMBuildFDiv(self.builder, lhs, rhs, c"fdiv".as_ptr()),
                    sir::BinaryOp::FMod => LLVMBuildFRem(self.builder, lhs, rhs, c"fmod".as_ptr()),
                    sir::BinaryOp::FEq => LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOEQ,
                        lhs,
                        rhs,
                        c"feq".as_ptr(),
                    ),
                    sir::BinaryOp::FNe => LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealONE,
                        lhs,
                        rhs,
                        c"fne".as_ptr(),
                    ),
                    sir::BinaryOp::FLt => LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOLT,
                        lhs,
                        rhs,
                        c"flt".as_ptr(),
                    ),
                    sir::BinaryOp::FLe => LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOLE,
                        lhs,
                        rhs,
                        c"fle".as_ptr(),
                    ),
                    sir::BinaryOp::FGt => LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOGT,
                        lhs,
                        rhs,
                        c"fgt".as_ptr(),
                    ),
                    sir::BinaryOp::FGe => LLVMBuildFCmp(
                        self.builder,
                        LLVMRealPredicate::LLVMRealOGE,
                        lhs,
                        rhs,
                        c"fge".as_ptr(),
                    ),
                    sir::BinaryOp::And => LLVMBuildAnd(self.builder, lhs, rhs, c"and".as_ptr()),
                    sir::BinaryOp::Or => LLVMBuildOr(self.builder, lhs, rhs, c"or".as_ptr()),
                }
            }

            sir::Value::Unary(op, operand) => {
                let operand = self.operand(operand);
                match op {
                    sir::UnaryOp::Neg => LLVMBuildNeg(self.builder, operand, c"neg".as_ptr()),
                    sir::UnaryOp::FNeg => LLVMBuildFNeg(self.builder, operand, c"fneg".as_ptr()),
                    sir::UnaryOp::BNot => todo!(),
                    sir::UnaryOp::Not => LLVMBuildNot(self.builder, operand, c"not".as_ptr()),
                    sir::UnaryOp::Deref => todo!(),
                }
            }

            sir::Value::Closure { body, .. } => {
                let closure_ty = LLVMStructTypeInContext(
                    self.context,
                    [
                        self.void_pointer_type(),             // function pointer
                        LLVMInt64TypeInContext(self.context), // missing
                        self.bodies[body].captures,           // captures
                    ]
                    .as_mut_ptr(),
                    3,
                    0,
                );

                let closure_ptr = self.alloc_single(closure_ty);

                let function = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure_ptr,
                    0, // function pointer
                    c"function_ptr".as_ptr(),
                );

                LLVMBuildStore(self.builder, self.bodies[body].function, function);

                let missing = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure_ptr,
                    1, // missing
                    c"missing_ptr".as_ptr(),
                );

                LLVMBuildStore(
                    self.builder,
                    LLVMSizeOf(self.bodies[body].captures),
                    missing,
                );

                closure_ptr
            }
        }
    }
}
