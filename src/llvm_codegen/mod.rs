use std::{collections::HashMap, ffi::CString};

use crate::mir::BodyId;
use crate::sir;
use llvm_sys::{core::*, prelude::*, target::*, target_machine::*};

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
            _ => {
                let name = CString::new(format!("ty{}", tid.0)).unwrap();
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
                // return void ptr

                let void = LLVMVoidTypeInContext(self.context);
                let mut ptr = LLVMPointerType(void, 0);
                LLVMStructSetBody(self.types[&tid].1.unwrap(), &mut ptr, 1, 0);
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
    unsafe fn new(codegen: &'a Codegen, id: BodyId) -> Self {
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

        let name = CString::new(format!("{:?}", id)).unwrap();
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
            }
            sir::Stmt::Match { .. } => {}
        }
    }

    unsafe fn place(&self, place: &sir::Place) -> LLVMValueRef {
        LLVMConstNull(LLVMInt64TypeInContext(self.context))
    }

    unsafe fn value(&self, value: &sir::Value) -> LLVMValueRef {
        match value {
            sir::Value::Use(_) => {}
            sir::Value::Tuple(_) => {}
            sir::Value::Record(_) => {}
            sir::Value::Promote { .. } => {}
            sir::Value::Coerce { .. } => {}
            sir::Value::Call(_, _) => {}
            sir::Value::Binary(_, _, _) => {}
            sir::Value::Unary(_, _) => {}
            sir::Value::Closure { .. } => {}
        };

        LLVMConstNull(LLVMInt64TypeInContext(self.context))
    }
}
