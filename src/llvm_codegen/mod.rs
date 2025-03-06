use std::{collections::HashMap, ffi::CString};

use crate::sir;
use crate::sir::Operand;

use llvm_sys::{
    core::*, prelude::*, target::*, target_machine::*, LLVMIntPredicate, LLVMRealPredicate,
};

/// void* macro (takes llvm context)
macro_rules! llvm_void_ptr {
    ($context:expr) => {
        LLVMPointerType(LLVMVoidTypeInContext($context), 0)
    };
}

macro_rules! llvm_null {
    ($context:expr) => {
        LLVMConstNull(LLVMInt64TypeInContext($context))
    };
}

pub fn codegen(program: sir::Program) {
    unsafe {
        let mut codegen = Codegen::new(program);
        codegen.build();
    }
}

struct Codegen {
    /// LLVM
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    program: sir::Program,
    types: HashMap<sir::Tid, (LLVMTypeRef, Option<LLVMTypeRef>)>,
}

impl Codegen {
    unsafe fn new(program: sir::Program) -> Self {
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(c"main".as_ptr(), context);
        let builder = LLVMCreateBuilderInContext(context);

        Self {
            program,
            context,
            module,
            builder,

            types: HashMap::new(),
        }
    }

    unsafe fn build(&mut self) {
        for (tid, _) in self.program.types.iter() {
            self.types.insert(tid, self.initialize_tys(tid));
        }

        for (tid, _) in self.program.types.iter() {
            self.build_tys(tid);
        }

        // Initialize bodies
        let mut bodies = self
            .program
            .bodies
            .iter()
            .map(|(id, _)| BodyCodegen::new(self, id))
            .collect::<Vec<_>>();

        // Build bodies
        for mut body in bodies {
            body.build();
        }

        println!(
            "{}",
            CString::from_raw(LLVMPrintModuleToString(self.module))
                .to_str()
                .unwrap()
        );
    }

    fn tid(&self, tid: sir::Tid) -> LLVMTypeRef {
        self.types[&tid].0
    }

    /// Build an LLVM type from a SIR type.
    unsafe fn initialize_tys(&self, tid: sir::Tid) -> (LLVMTypeRef, Option<LLVMTypeRef>) {
        match &self.program.types[tid] {
            sir::Ty::Int => (LLVMInt64TypeInContext(self.context), None),
            sir::Ty::Float => (LLVMFloatTypeInContext(self.context), None),
            sir::Ty::Str => {
                let inner = LLVMInt8TypeInContext(self.context);
                let ptr = LLVMPointerType(inner, 0);
                (ptr, Some(inner))
            }
            sir::Ty::List(_) => {
                let void = LLVMVoidTypeInContext(self.context);
                let ptr = LLVMPointerType(void, 0);
                (ptr, Some(void))
            }
            sir::Ty::True | sir::Ty::False | sir::Ty::None | sir::Ty::Never => {
                let zst = LLVMStructTypeInContext(self.context, std::ptr::null_mut(), 0, 0);
                (zst, None)
            }
            ty => {
                let name = match ty {
                    sir::Ty::Ref(_) => "ref",
                    sir::Ty::Func(_, _) => "fn",
                    sir::Ty::Tuple(_) => "tuple",
                    sir::Ty::Record(_) => "record",
                    sir::Ty::Union(_) => "union",
                    _ => unreachable!(),
                };

                let name = format!("ty_{}_{}", name, tid.0);
                let name = CString::new(name).unwrap();
                let inner = LLVMStructCreateNamed(self.context, name.as_ptr());
                let ptr = LLVMPointerType(inner, 0);
                (ptr, Some(inner))
            }
        }
    }

    unsafe fn build_tys(&self, tid: sir::Tid) {
        match self.program.types[tid] {
            sir::Ty::Int
            | sir::Ty::Float
            | sir::Ty::Str
            | sir::Ty::True
            | sir::Ty::False
            | sir::Ty::None
            | sir::Ty::Never
            | sir::Ty::List(_) => {}

            sir::Ty::Ref(inner) => {
                let mut inner = self.tid(inner);
                let s = self.types[&tid].1.unwrap();
                LLVMStructSetBody(s, &mut inner, 1, 0);
            }

            sir::Ty::Func(input, output) => {
                let input = self.tid(input);
                let output = self.tid(output);

                let void = LLVMVoidTypeInContext(self.context);
                let captures = LLVMPointerType(void, 0);

                let mut inputs = [captures, input];

                let func = LLVMFunctionType(output, inputs.as_mut_ptr(), 1, 0);

                let mut elements = [captures, func];

                let s = self.types[&tid].1.unwrap();
                LLVMStructSetBody(s, elements.as_mut_ptr(), 2, 0);
            }

            sir::Ty::Tuple(ref items) => {
                let mut elements = Vec::new();
                for item in items {
                    elements.push(self.tid(*item));
                }

                let s = self.types[&tid].1.unwrap();
                LLVMStructSetBody(s, elements.as_mut_ptr(), elements.len() as u32, 0);
            }

            sir::Ty::Record(ref fields) => {
                let mut elements = Vec::new();
                for (_, ty) in fields {
                    elements.push(self.tid(*ty));
                }

                let s = self.types[&tid].1.unwrap();
                LLVMStructSetBody(s, elements.as_mut_ptr(), elements.len() as u32, 0);
            }
            sir::Ty::Union(ref btree_set) => {
                // { u64, void* }
                let void = LLVMVoidTypeInContext(self.context);
                let void_ptr = LLVMPointerType(void, 0);
                let u64 = LLVMInt64TypeInContext(self.context);
                let mut elements = [u64, void_ptr];
                let s = self.types[&tid].1.unwrap();
                LLVMStructSetBody(s, elements.as_mut_ptr(), 2, 0);
            }
        }
    }
}

impl Drop for Codegen {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

struct BodyCodegen<'a> {
    /// LLVM
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    codegen: &'a Codegen,
    body: sir::Body,
    function: LLVMValueRef,
    captures: LLVMTypeRef,
    locals: Vec<LLVMValueRef>,
}

impl<'a> BodyCodegen<'a> {
    unsafe fn new(codegen: &'a Codegen, id: sir::Bid) -> Self {
        let body = &codegen.program.bodies[id];

        let context = codegen.context;
        let module = codegen.module;
        let builder = codegen.builder;

        let mut captures = Vec::new();

        for i in 0..body.captures {
            let local = body.locals[crate::mir::Local(i)];
            let ty = codegen.tid(local);
            captures.push(ty);
        }

        let captures =
            LLVMStructTypeInContext(context, captures.as_mut_ptr(), captures.len() as u32, 0);

        let void = LLVMVoidTypeInContext(context);
        let void_ptr = LLVMPointerType(void, 0);

        let input = if body.arguments > 0 {
            let input = body.locals[crate::mir::Local(body.captures)];
            codegen.tid(input)
        } else {
            LLVMStructTypeInContext(context, std::ptr::null_mut(), 0, 0)
        };

        let mut inputs = [void_ptr, input];

        let output = codegen.tid(body.output);

        let func = LLVMFunctionType(output, inputs.as_mut_ptr(), 2, 0);

        let name = CString::new(format!("body_{}", id.0)).unwrap();
        let function = LLVMAddFunction(module, name.as_ptr(), func);

        Self {
            context,
            module,
            builder,

            codegen,
            body: body.clone(),
            function,
            captures,
            locals: Vec::new(),
        }
    }

    unsafe fn build(&mut self) {
        let block = LLVMAppendBasicBlockInContext(self.context, self.function, c"entry".as_ptr());
        LLVMPositionBuilderAtEnd(self.builder, block);

        let mut locals = Vec::new();

        for i in 0..self.body.locals.len() {
            let local = self.body.locals[crate::mir::Local(i)];
            let ty = self.codegen.tid(local);
            let alloca = LLVMBuildAlloca(self.builder, ty, c"local".as_ptr());
            locals.push(alloca);
        }

        self.block(&self.body.block);
    }

    unsafe fn block(&self, block: &sir::Block) {
        for stmt in &block.stmts {
            self.stmt(stmt);
        }

        match &block.term {
            sir::Term::Return(value) => {}
            sir::Term::Break => {}
            sir::Term::Exit => {}
        }
    }

    unsafe fn stmt(&self, stmt: &sir::Stmt) {
        match stmt {
            sir::Stmt::Assign(place, value) => {
                let place = self.place(place);
                let value = self.value(value);
                LLVMBuildStore(self.builder, value, place);
            }
            sir::Stmt::Loop(block) => {
                // Create loop that jumps to the top of the block
                let llvm_block =
                    LLVMAppendBasicBlockInContext(self.context, self.function, c"loop".as_ptr());
                LLVMPositionBuilderAtEnd(self.builder, llvm_block);
                self.block(block);
                LLVMBuildBr(self.builder, llvm_block);
            }
            sir::Stmt::Match { .. } => {}
        }
    }

    unsafe fn operand(&self, operand: &sir::Operand) -> LLVMValueRef {
        match operand {
            Operand::Place(place) => self.place(place),
            Operand::Const(r#const) => match r#const {
                sir::Const::None => LLVMConstNull(LLVMInt64TypeInContext(self.context)),
                sir::Const::Int(value) => {
                    LLVMConstInt(LLVMInt64TypeInContext(self.context), *value as u64, 0)
                }
                sir::Const::Float(value) => {
                    LLVMConstReal(LLVMFloatTypeInContext(self.context), *value)
                }
                sir::Const::String(value) => {
                    todo!("needs allocation");
                }
            },
        }
    }

    unsafe fn place(&self, place: &sir::Place) -> LLVMValueRef {
        LLVMConstNull(LLVMInt64TypeInContext(self.context))
    }

    unsafe fn value(&self, value: &sir::Value) -> LLVMValueRef {
        match value {
            sir::Value::Use(_) => {
                llvm_null!(self.context)
            }
            sir::Value::Tuple(_) => {
                // Allocate a tuple
                llvm_null!(self.context)
            }
            sir::Value::Record(_) => {
                // Allocate a record
                llvm_null!(self.context)
            }
            sir::Value::Promote { .. } => llvm_null!(self.context),
            sir::Value::Coerce { .. } => llvm_null!(self.context),
            sir::Value::Call(_, _) => llvm_null!(self.context),
            sir::Value::Binary(op, lhs, rhs) => {
                let lhs = self.operand(lhs);
                let rhs = self.operand(rhs);

                match op {
                    sir::BinaryOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, c"add".as_ptr()),
                    sir::BinaryOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, c"sub".as_ptr()),
                    sir::BinaryOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, c"mul".as_ptr()),
                    sir::BinaryOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, c"div".as_ptr()),
                    sir::BinaryOp::Mod => LLVMBuildSRem(self.builder, lhs, rhs, c"mod".as_ptr()),
                    sir::BinaryOp::BAnd => llvm_null!(self.context),
                    sir::BinaryOp::BOr => llvm_null!(self.context),
                    sir::BinaryOp::BXor => llvm_null!(self.context),
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
                    sir::UnaryOp::BNot => llvm_null!(self.context),
                    sir::UnaryOp::Not => LLVMBuildNot(self.builder, operand, c"not".as_ptr()),
                    sir::UnaryOp::Deref => llvm_null!(self.context),
                }
            }
            sir::Value::Closure { body, .. } => {
                llvm_null!(self.context)
            }
        }
    }
}
