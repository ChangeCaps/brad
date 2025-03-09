mod jit;

use std::{collections::HashMap, ffi::CString, ops::Deref, ptr};

use crate::sir;
use crate::sir::Operand;

use crate::mir::Proj;
use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use llvm_sys::{core::*, prelude::*, target::*, LLVMIntPredicate, LLVMRealPredicate, LLVMTypeKind};

pub fn codegen(program: sir::Program) -> String {
    unsafe {
        let mut codegen = Codegen::new(program);
        codegen.build()
    }
}

pub fn jit(module: &str, entry: &str) {
    unsafe {
        let jit = jit::Jit::new();
        let module = jit.load_module(module);
        jit.run(module, entry);
    }
}

struct Codegen {
    // LLVM
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    program: sir::Program,
    bodies: HashMap<sir::Bid, LLVMBody>,
    types: HashMap<sir::Tid, (LLVMTypeRef, Option<LLVMTypeRef>)>,
    externs: HashMap<String, (LLVMValueRef, LLVMTypeRef)>,
}

impl Drop for Codegen {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.context);
        }
    }
}

impl Codegen {
    unsafe fn new(program: sir::Program) -> Self {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();

        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(c"main".as_ptr(), context);
        let builder = LLVMCreateBuilderInContext(context);

        Self {
            context,
            module,
            builder,

            program,
            bodies: HashMap::new(),
            types: HashMap::new(),
            externs: HashMap::new(),
        }
    }

    /// Build LLVM IR from the SIR program.
    unsafe fn build(&mut self) -> String {
        // Add external brad_alloc and brad_free functions
        let brad_alloc_ty = LLVMFunctionType(
            self.void_pointer_type(),
            [self.i64_type(), self.i64_type()].as_mut_ptr(),
            2,
            0,
        );

        let brad_alloc_fn = LLVMAddFunction(self.module, c"brad_alloc".as_ptr(), brad_alloc_ty);

        let brad_free_ty = LLVMFunctionType(
            self.void_type(),
            [LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(),
            1,
            0,
        );

        let brad_free_fn = LLVMAddFunction(self.module, c"brad_free".as_ptr(), brad_free_ty);

        self.externs.insert(
            "brad_alloc".to_string(),
            (brad_alloc_fn, brad_alloc_ty), //
        );
        self.externs.insert(
            "brad_free".to_string(),
            (brad_free_fn, brad_free_ty), //
        );

        // Build types
        for (tid, _) in self.program.types.iter() {
            self.types.insert(tid, self.initialize_ty(tid));
        }

        for (tid, _) in self.program.types.iter() {
            self.build_tys(tid);
        }

        for (id, _) in self.program.bodies.iter() {
            self.bodies.insert(id, LLVMBody::new(self, id));
        }

        // build bodies
        for (id, _) in self.program.bodies.iter() {
            let mut body_codegen = BodyCodegen::new(self, id);
            body_codegen.build();
        }

        println!("Verifying module...");

        let verified = LLVMVerifyModule(
            self.module,
            LLVMVerifierFailureAction::LLVMPrintMessageAction,
            ptr::null_mut(),
        );

        if verified.is_negative() {
            // panic!("Module verification failed");
        }

        CString::from_raw(LLVMPrintModuleToString(self.module))
            .to_str()
            .unwrap()
            .to_string()
    }

    fn tid(&self, tid: sir::Tid) -> LLVMTypeRef {
        self.types[&tid].0
    }

    unsafe fn void_type(&self) -> LLVMTypeRef {
        LLVMVoidTypeInContext(self.context)
    }

    unsafe fn void_pointer_type(&self) -> LLVMTypeRef {
        LLVMPointerType(self.void_type(), 0)
    }

    unsafe fn zero_size_type(&self) -> LLVMTypeRef {
        LLVMStructTypeInContext(self.context, ptr::null_mut(), 0, 0)
    }

    unsafe fn zero_size_value(&self) -> LLVMValueRef {
        LLVMConstNamedStruct(self.zero_size_type(), ptr::null_mut(), 0)
    }

    unsafe fn string_type(&self) -> LLVMTypeRef {
        LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
    }

    unsafe fn list_type(&self) -> LLVMTypeRef {
        LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
    }

    unsafe fn i64_type(&self) -> LLVMTypeRef {
        LLVMInt64TypeInContext(self.context)
    }

    unsafe fn alloc_single(&self, ty: LLVMTypeRef) -> LLVMValueRef {
        let (alloc_fn, alloc_ty) = self.externs["brad_alloc"];

        let size = LLVMSizeOf(ty);

        let mut args = [
            size,
            LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, 0),
        ];

        LLVMBuildCall2(
            self.builder,
            alloc_ty,
            alloc_fn,
            args.as_mut_ptr(),
            2,
            c"alloc".as_ptr(),
        )
    }

    unsafe fn alloc_bytes(&self, count: LLVMValueRef) -> LLVMValueRef {
        let (alloc_fn, alloc_ty) = self.externs["brad_alloc"];

        let size = LLVMSizeOf(LLVMInt8TypeInContext(self.context));

        let mut args = [size, count];

        LLVMBuildCall2(
            self.builder,
            alloc_ty,
            alloc_fn,
            args.as_mut_ptr(),
            2,
            c"alloc".as_ptr(),
        )
    }

    /// Build an LLVM type from a SIR type.
    unsafe fn initialize_ty(&self, tid: sir::Tid) -> (LLVMTypeRef, Option<LLVMTypeRef>) {
        match &self.program.types[tid] {
            sir::Ty::Int => (LLVMInt64TypeInContext(self.context), None),
            sir::Ty::Float => (LLVMFloatTypeInContext(self.context), None),

            sir::Ty::True | sir::Ty::False | sir::Ty::None | sir::Ty::Never => {
                (self.zero_size_type(), None)
            }

            sir::Ty::Str | sir::Ty::List(_) | sir::Ty::Union(_) => {
                let ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                (ptr, None)
            }

            sir::Ty::Func(_, _) => {
                let ty = LLVMStructCreateNamed(self.context, c"ty_fn".as_ptr());
                (ty, None)
            }

            sir::Ty::Record(fields) if fields.is_empty() => (self.zero_size_type(), None),

            ty => {
                let name = match ty {
                    sir::Ty::Ref(_) => "ref",
                    sir::Ty::Func(_, _) => "fn",
                    sir::Ty::Tuple(_) => "tuple",
                    sir::Ty::Record(_) => "record",
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
            | sir::Ty::List(_)
            | sir::Ty::Union(_) => {}

            sir::Ty::Ref(inner) => {
                let mut inner = self.tid(inner);
                let s = self.types[&tid].1.unwrap();
                LLVMStructSetBody(s, &mut inner, 1, 0);
            }

            sir::Ty::Func(_, _) => {
                let void = LLVMVoidTypeInContext(self.context);
                let captures = LLVMPointerType(void, 0);
                let missing = LLVMInt64TypeInContext(self.context);

                let mut elements = [captures, missing, self.void_pointer_type()];

                let s = self.types[&tid].0;
                LLVMStructSetBody(s, elements.as_mut_ptr(), 3, 0);
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
        }
    }
}

struct LLVMBody {
    function: LLVMValueRef,
    captures: LLVMTypeRef,
    entry: LLVMBasicBlockRef,
    locals: Vec<LLVMValueRef>,
}

impl LLVMBody {
    unsafe fn new(codegen: &Codegen, id: sir::Bid) -> Self {
        let body = &codegen.program.bodies[id];

        if let Some(ref name) = body.name {
            println!("Compiling function: {}", name);
            // output comments
        }

        /* create the captures struct */

        let mut captures = Vec::new();

        for i in (0..body.captures + body.arguments).rev() {
            let local = body.locals[crate::mir::Local(i)];
            let ty = codegen.tid(local);
            captures.push(ty);
        }

        let captures = LLVMStructTypeInContext(
            codegen.context,
            captures.as_mut_ptr(), // elements
            captures.len() as u32, // element count
            0,
        );

        let mut input = LLVMPointerType(captures, 0);
        let output = codegen.tid(body.output);

        /* create the function type */

        let func = LLVMFunctionType(output, &mut input, 1, 0);

        /* create the function */

        let name = match body.name {
            Some(ref name) => name.clone(),
            None => format!("anon_body_[{}]", id.0),
        };

        let name = CString::new(name).unwrap();
        let function = LLVMAddFunction(codegen.module, name.as_ptr(), func);

        /* build the local variables */

        let entry = LLVMAppendBasicBlockInContext(codegen.context, function, c"entry".as_ptr());
        LLVMPositionBuilderAtEnd(codegen.builder, entry);

        let mut locals = Vec::new();

        for i in 0..body.locals.len() {
            let local = body.locals[crate::mir::Local(i)];
            let ty = codegen.tid(local);
            let alloca = LLVMBuildAlloca(codegen.builder, ty, c"local".as_ptr());
            locals.push(alloca);
        }

        let input = LLVMGetParam(function, 0);

        for (i, local) in (0..body.captures + body.arguments).rev().enumerate() {
            let arg = LLVMBuildStructGEP2(
                codegen.builder,
                captures,
                input,
                i as u32, // index
                c"arg".as_ptr(),
            );

            let arg = LLVMBuildLoad2(
                codegen.builder,
                codegen.tid(body.locals[crate::mir::Local(local)]),
                arg,
                c"load".as_ptr(),
            );

            LLVMBuildStore(codegen.builder, arg, locals[local]);
        }

        Self {
            function,
            captures,
            entry,
            locals,
        }
    }
}

struct BodyCodegen<'a> {
    codegen: &'a Codegen,
    body: &'a sir::Body,
    llvm_body: &'a LLVMBody,
}

impl Deref for BodyCodegen<'_> {
    type Target = Codegen;

    fn deref(&self) -> &Self::Target {
        self.codegen
    }
}

impl<'a> BodyCodegen<'a> {
    unsafe fn new(codegen: &'a Codegen, id: sir::Bid) -> Self {
        Self {
            codegen,
            body: &codegen.program.bodies[id],
            llvm_body: &codegen.bodies[&id],
        }
    }

    unsafe fn build(&mut self) {
        LLVMPositionBuilderAtEnd(self.builder, self.llvm_body.entry);

        match self.body.block {
            Some(ref block) => {
                self.block(block);
            }

            None => {
                self.r#extern();
            }
        }
    }

    unsafe fn r#extern(&self) {
        assert!(self.body.is_extern);

        let mut inputs = Vec::new();

        for (_, &tid) in self.body.locals.iter().take(self.body.arguments) {
            inputs.push(self.tid(tid));
        }

        let output = self.tid(self.body.output);

        let func_ty = LLVMFunctionType(output, inputs.as_mut_ptr(), inputs.len() as u32, 0);

        let name = match self.body.attrs.find_value("link") {
            Some(name) => name.to_string(),
            None => panic!("missing link attribute"),
        };

        let name = CString::new(name).unwrap();

        let func = LLVMAddFunction(self.module, name.as_ptr(), func_ty);

        let mut args = Vec::new();

        for &local in self.llvm_body.locals.iter().take(self.body.arguments) {
            let local = LLVMBuildLoad2(
                self.builder,
                LLVMTypeOf(local),
                local,
                c"load_extern_call".as_ptr(),
            );
            args.push(local);
        }

        let output = LLVMBuildCall2(
            self.builder,
            func_ty,
            func,
            args.as_mut_ptr(),
            args.len() as u32,
            c"call".as_ptr(),
        );

        LLVMBuildRet(self.builder, output);
    }

    unsafe fn block(&self, block: &sir::Block) {
        for stmt in &block.stmts {
            self.stmt(stmt);
        }

        if let Some(ref term) = block.term {
            match term {
                sir::Term::Return(value) => {
                    let value = self.value(value);
                    LLVMBuildRet(self.builder, value);
                }

                sir::Term::Break => {}
            }
        }
    }

    unsafe fn stmt(&self, stmt: &sir::Stmt) {
        match stmt {
            sir::Stmt::Drop(value) => {
                let _ = self.value(value);
            }

            sir::Stmt::Assign(place, value) => {
                let place = self.place(place);
                let value = self.value(value);
                LLVMBuildStore(self.builder, value, place);
            }

            sir::Stmt::Loop(block) => {
                // Create loop that jumps to the top of the block
                let llvm_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body.function,
                    c"loop".as_ptr(),
                );

                LLVMBuildBr(self.builder, llvm_block);

                LLVMPositionBuilderAtEnd(self.builder, llvm_block);
                self.block(block);
                LLVMBuildBr(self.builder, llvm_block);
            }

            sir::Stmt::Match {
                target,
                cases,
                default,
            } => {
                // structure only, missing control flow.
                let target = self.place(target);
                let target = LLVMBuildLoad2(
                    self.builder,
                    self.void_pointer_type(),
                    target, // the target
                    c"load".as_ptr(),
                );

                // load the tag from the target
                let tag = LLVMBuildLoad2(self.builder, self.i64_type(), target, c"load".as_ptr());

                let default_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body.function,
                    c"default".as_ptr(),
                );

                let switch = LLVMBuildSwitch(
                    self.builder,
                    tag,
                    default_block,
                    cases.len() as u32, // estimated number of cases
                );

                let end_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body.function,
                    c"end".as_ptr(),
                );

                for case in cases {
                    let case_block = LLVMAppendBasicBlockInContext(
                        self.context,
                        self.llvm_body.function,
                        c"case".as_ptr(),
                    );

                    LLVMPositionBuilderAtEnd(self.builder, case_block);

                    let variant = LLVMConstInt(self.i64_type(), case.ty.0 as u64, 0);
                    LLVMAddCase(switch, variant, case_block);

                    let ty = self.tid(case.ty);

                    // create a the type for the specific variant
                    let union = LLVMStructTypeInContext(
                        self.context,
                        [self.i64_type(), ty].as_mut_ptr(),
                        2,
                        0,
                    );

                    // retrieve the value from the union
                    let value = LLVMBuildStructGEP2(
                        self.builder,
                        union,
                        target,
                        1, // value
                        c"union_value".as_ptr(),
                    );

                    let value = LLVMBuildLoad2(self.builder, ty, value, c"load".as_ptr());

                    // store the value in the case local
                    let local = self.llvm_body.locals[case.local.0];
                    LLVMBuildStore(self.builder, value, local);

                    // build the case block
                    self.block(&case.block);

                    LLVMBuildBr(self.builder, end_block);
                }

                /* default block */

                LLVMPositionBuilderAtEnd(self.builder, default_block);

                self.block(default);
                LLVMBuildBr(self.builder, end_block);

                /* end block */

                LLVMPositionBuilderAtEnd(self.builder, end_block);
            }
        }
    }

    unsafe fn value(&self, value: &sir::Value) -> LLVMValueRef {
        match value {
            sir::Value::Use(operand) => self.operand(operand),

            sir::Value::Tuple(items) => {
                let mut values = Vec::new();
                let mut types = Vec::new();

                for operand in items {
                    values.push(self.operand(operand));
                    types.push(self.tid(*operand.ty(&self.body.locals)));
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
                    types.push(self.tid(*operand.ty(&self.body.locals)));
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
                let func_tid = *func.ty(&self.body.locals);

                let output_ty = match self.program.types[func_tid] {
                    sir::Ty::Func(_, output) => self.codegen.tid(output),
                    _ => unreachable!(),
                };

                let func_ty = self.codegen.tid(func_tid);
                let func = self.place(func);

                let input_tid = *input.ty(&self.body.locals);
                let input_ty = self.codegen.tid(input_tid);
                let input = self.operand(input);

                let captures = LLVMBuildStructGEP2(
                    self.builder,
                    func_ty,
                    func,
                    0, // captures
                    c"func_captures_ptr".as_ptr(),
                );

                let captures = LLVMBuildLoad2(
                    self.builder,
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                    captures,
                    c"func_captures".as_ptr(),
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
                    self.llvm_body.function,
                    c"call".as_ptr(),
                );

                let append_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body.function,
                    c"append".as_ptr(),
                );

                let end_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body.function,
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

                let func = LLVMBuildLoad2(self.builder, func_ty, func, c"load".as_ptr());
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
                let captures = self.alloc_single(self.bodies[body].captures);

                let closure_ty = LLVMStructTypeInContext(
                    self.context,
                    [
                        self.void_pointer_type(),             // captures
                        LLVMInt64TypeInContext(self.context), // missing
                        self.void_pointer_type(),             // function pointer
                    ]
                    .as_mut_ptr(),
                    3,
                    0,
                );

                let closure = LLVMBuildAlloca(self.builder, closure_ty, c"closure".as_ptr());

                let captures_ptr = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure,
                    0, // captures
                    c"captures_ptr".as_ptr(),
                );

                LLVMBuildStore(self.builder, captures, captures_ptr);

                let missing = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure,
                    1, // missing
                    c"missing_ptr".as_ptr(),
                );

                LLVMBuildStore(
                    self.builder,
                    LLVMSizeOf(self.bodies[body].captures),
                    missing,
                );

                let function = LLVMBuildStructGEP2(
                    self.builder,
                    closure_ty,
                    closure,
                    2, // function pointer
                    c"function_ptr".as_ptr(),
                );

                LLVMBuildStore(self.builder, self.bodies[body].function, function);
                LLVMBuildLoad2(self.builder, closure_ty, closure, c"load".as_ptr())
            }
        }
    }

    unsafe fn operand(&self, operand: &sir::Operand) -> LLVMValueRef {
        match operand {
            Operand::Copy(place) => {
                let &tid = place.ty(&self.body.locals);
                let ty = self.codegen.tid(tid);

                let place = self.place(place);

                LLVMBuildLoad2(self.builder, ty, place, c"load".as_ptr())
            }

            Operand::Const(r#const, _) => match r#const {
                sir::Const::None => self.codegen.zero_size_value(),

                sir::Const::Int(value) => {
                    LLVMConstInt(LLVMInt64TypeInContext(self.context), *value as u64, 0)
                }

                sir::Const::Float(value) => {
                    LLVMConstReal(LLVMFloatTypeInContext(self.context), *value)
                }

                sir::Const::String(value) => {
                    let i64_size = LLVMSizeOf(self.i64_type());
                    let len = LLVMConstInt(
                        LLVMInt64TypeInContext(self.context),
                        value.len() as u64, // length in bytes
                        0,
                    );

                    let total_len = LLVMBuildAdd(self.builder, i64_size, len, c"len".as_ptr());

                    let string = self.codegen.alloc_bytes(total_len);

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

    unsafe fn place(&self, place: &sir::Place) -> LLVMValueRef {
        let mut local = self.llvm_body.locals[place.local.0];
        let mut tid = self.body.locals[place.local];

        for (proj, result_tid) in &place.proj {
            match proj {
                Proj::Field(name) => {
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

                Proj::Tuple(index) => {
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

                Proj::Index(_) => todo!(),

                Proj::Deref => todo!(),
            }

            tid = *result_tid;
        }

        local
    }
}
