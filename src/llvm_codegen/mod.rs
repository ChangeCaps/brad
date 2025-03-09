mod drop;
mod jit;
mod mark;
mod operand;
mod place;
mod value;

use std::ops::DerefMut;
use std::{collections::HashMap, ffi::CString, ops::Deref, ptr};

use crate::sir;

use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use llvm_sys::{core::*, prelude::*, target::*};

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
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub gc: GarbageContext,

    pub program: sir::Program,
    pub bodies: HashMap<sir::Bid, LLVMBody>,
    pub types: HashMap<sir::Tid, (LLVMTypeRef, Option<LLVMTypeRef>)>,
    pub markers: HashMap<sir::Tid, LLVMValueRef>,
}

struct GarbageContext {
    pub alloc_ty: LLVMTypeRef,
    pub alloc: LLVMValueRef,

    pub retain_ty: LLVMTypeRef,
    pub retain: LLVMValueRef,

    pub release_ty: LLVMTypeRef,
    pub release: LLVMValueRef,

    pub collect_ty: LLVMTypeRef,
    pub collect: LLVMValueRef,

    pub marker_ty: LLVMTypeRef,
    pub marker_ptr_ty: LLVMTypeRef,
}

impl GarbageContext {
    unsafe fn new(context: LLVMContextRef, module: LLVMModuleRef) -> Self {
        let byte_ptr = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
        let void = LLVMVoidTypeInContext(context);
        let int64 = LLVMInt64TypeInContext(context);

        let marker_ty = LLVMFunctionType(void, [byte_ptr].as_mut_ptr(), 1, 0);
        let marker_ptr_ty = LLVMPointerType(marker_ty, 0);

        let alloc_ty = LLVMFunctionType(byte_ptr, [int64, marker_ptr_ty].as_mut_ptr(), 2, 0);
        let alloc = LLVMAddFunction(module, c"brad_alloc".as_ptr(), alloc_ty);

        let retain_ty = LLVMFunctionType(void, [byte_ptr].as_mut_ptr(), 1, 0);
        let retain = LLVMAddFunction(module, c"brad_retain".as_ptr(), retain_ty);

        let release_ty = LLVMFunctionType(void, [byte_ptr].as_mut_ptr(), 1, 0);
        let release = LLVMAddFunction(module, c"brad_release".as_ptr(), release_ty);

        let collect_ty = LLVMFunctionType(void, [].as_mut_ptr(), 0, 0);
        let collect = LLVMAddFunction(module, c"brad_collect".as_ptr(), collect_ty);

        Self {
            alloc_ty,
            alloc,

            retain_ty,
            retain,

            release_ty,
            release,

            collect_ty,
            collect,

            marker_ty,
            marker_ptr_ty,
        }
    }
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
        let garbage = GarbageContext::new(context, module);

        Self {
            context,
            module,
            builder,
            gc: garbage,

            program,
            bodies: HashMap::new(),
            types: HashMap::new(),
            markers: HashMap::new(),
        }
    }

    /// Build LLVM IR from the SIR program.
    unsafe fn build(&mut self) -> String {
        // Build types
        for (tid, _) in self.program.types.iter() {
            self.types.insert(tid, self.initialize_ty(tid));
        }

        for (tid, _) in self.program.types.iter() {
            self.build_tys(tid);
        }

        let body_ids = self
            .program
            .bodies
            .iter()
            .map(|(id, _)| id)
            .collect::<Vec<_>>();

        for id in body_ids.iter().copied() {
            self.bodies.insert(id, LLVMBody::new(self, id));
        }

        // build bodies
        for id in body_ids {
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
                let missing = LLVMInt64TypeInContext(self.context);

                let mut elements = [missing, self.void_pointer_type(), self.zero_size_type()];

                let s = self.types[&tid].1.unwrap();
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
    codegen: &'a mut Codegen,
    id: sir::Bid,

    /// The number of local allocations retained in the function.
    retained: usize,
}

impl Deref for BodyCodegen<'_> {
    type Target = Codegen;

    fn deref(&self) -> &Self::Target {
        self.codegen
    }
}

impl DerefMut for BodyCodegen<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.codegen
    }
}

impl<'a> BodyCodegen<'a> {
    unsafe fn new(codegen: &'a mut Codegen, id: sir::Bid) -> Self {
        Self {
            codegen,
            id,
            retained: 0,
        }
    }

    unsafe fn body(&self) -> &sir::Body {
        &self.program.bodies[self.id]
    }

    unsafe fn llvm_body(&self) -> &LLVMBody {
        &self.bodies[&self.id]
    }

    unsafe fn alloc_single(&mut self, ty: LLVMTypeRef) -> LLVMValueRef {
        let size = LLVMSizeOf(ty);
        let marker = self.str_marker();

        self.alloc(size, marker)
    }

    unsafe fn alloc(&mut self, count: LLVMValueRef, marker: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildCall2(
            self.builder,
            self.gc.alloc_ty,
            self.gc.alloc,
            [count, marker].as_mut_ptr(),
            2,
            c"alloc".as_ptr(),
        )
    }

    unsafe fn retain(&mut self, value: LLVMValueRef) {
        LLVMBuildCall2(
            self.builder,
            self.gc.retain_ty,
            self.gc.retain,
            [value].as_mut_ptr(),
            1,
            c"retain".as_ptr(),
        );
    }

    unsafe fn release(&mut self, value: LLVMValueRef) {
        LLVMBuildCall2(
            self.builder,
            self.gc.release_ty,
            self.gc.release,
            [value].as_mut_ptr(),
            1,
            c"release".as_ptr(),
        );
    }

    unsafe fn collect(&mut self) {
        LLVMBuildCall2(
            self.builder,
            self.gc.collect_ty,
            self.gc.collect,
            [].as_mut_ptr(),
            0,
            c"collect".as_ptr(),
        );
    }

    unsafe fn build(&mut self) {
        LLVMPositionBuilderAtEnd(self.builder, self.llvm_body().entry);

        match self.body().block.clone() {
            Some(block) => {
                self.block(&block);
            }

            None => {
                self.r#extern();
            }
        }
    }

    unsafe fn r#extern(&self) {
        assert!(self.body().is_extern);

        let mut inputs = Vec::new();

        for (_, &tid) in self.body().locals.iter().take(self.body().arguments) {
            inputs.push(self.tid(tid));
        }

        let output = self.tid(self.body().output);

        let func_ty = LLVMFunctionType(output, inputs.as_mut_ptr(), inputs.len() as u32, 0);

        let name = match self.body().attrs.find_value("link") {
            Some(name) => name.to_string(),
            None => panic!("missing link attribute"),
        };

        let name = CString::new(name).unwrap();

        let func = LLVMAddFunction(self.module, name.as_ptr(), func_ty);

        let mut args = Vec::new();

        for &local in self.llvm_body().locals.iter().take(self.body().arguments) {
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

    unsafe fn block(&mut self, block: &sir::Block) {
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

    unsafe fn stmt(&mut self, stmt: &sir::Stmt) {
        match stmt {
            sir::Stmt::Drop(value, tid) => {
                let value = self.value(value);
                self.drop(value, *tid);
            }

            sir::Stmt::Assign(place, value) => {
                let place = self.place(place);
                let value = self.value(value);
                LLVMBuildStore(self.builder, value, place);
            }

            sir::Stmt::Loop(block) => {
                // create loop that jumps to the top of the block
                let llvm_block = LLVMAppendBasicBlockInContext(
                    self.context,
                    self.llvm_body().function,
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
                    self.llvm_body().function,
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
                    self.llvm_body().function,
                    c"end".as_ptr(),
                );

                for case in cases {
                    let case_block = LLVMAppendBasicBlockInContext(
                        self.context,
                        self.llvm_body().function,
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
                    let local = self.llvm_body().locals[case.local.0];
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
}
