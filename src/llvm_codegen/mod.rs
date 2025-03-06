use std::{collections::HashMap, ffi::CString};

use llvm_sys::{core::*, prelude::*, target::*, target_machine::*};

use crate::sir;

pub fn codegen(program: sir::Program) {
    unsafe {
        let mut codegen = Codegen::new(program);
        codegen.build();
    }
}

struct Codegen {
    program: sir::Program,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    bodies: HashMap<sir::Bid, Body>,
    types: HashMap<sir::Tid, (LLVMTypeRef, Option<LLVMTypeRef>)>,
}

struct Body {
    captures: LLVMTypeRef,
    function: LLVMValueRef,
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

            bodies: HashMap::new(),
            types: HashMap::new(),
        }
    }

    unsafe fn build(&mut self) {
        for (tid, _) in self.program.types.iter() {
            self.types.insert(tid, self.ty_step1(tid));
        }

        for (tid, _) in self.program.types.iter() {
            self.ty_step2(tid);
        }

        for (id, body) in self.program.bodies.iter() {
            let mut captures = Vec::new();

            for i in 0..body.captures {
                let local = body.locals[crate::mir::Local(i)];
                let ty = self.tid(local);
                captures.push(ty);
            }

            let captures = LLVMStructTypeInContext(
                self.context,
                captures.as_mut_ptr(),
                captures.len() as u32,
                0,
            );

            let void = LLVMVoidTypeInContext(self.context);
            let void_ptr = LLVMPointerType(void, 0);

            let input = if body.arguments > 0 {
                let input = body.locals[crate::mir::Local(body.captures)];
                self.tid(input)
            } else {
                LLVMStructTypeInContext(self.context, std::ptr::null_mut(), 0, 0)
            };

            let mut inputs = [void_ptr, input];

            let output = self.tid(body.output);

            let func = LLVMFunctionType(output, inputs.as_mut_ptr(), 2, 0);

            let name = CString::new(format!("{:?}", id)).unwrap();
            let function = LLVMAddFunction(self.module, name.as_ptr(), func);

            self.bodies.insert(id, Body { captures, function });
        }

        for (id, body) in self.program.bodies.iter() {
            self.body(id, body);
        }

        println!(
            "{}",
            CString::from_raw(LLVMPrintModuleToString(self.module))
                .to_str()
                .unwrap()
        );
    }

    unsafe fn body(&self, id: sir::Bid, body: &sir::Body) {
        let llvm_body = &self.bodies[&id];

        let block =
            LLVMAppendBasicBlockInContext(self.context, llvm_body.function, c"entry".as_ptr());
        LLVMPositionBuilderAtEnd(self.builder, block);

        let mut locals = Vec::new();

        for (_, local) in body.locals.iter() {
            let ty = self.tid(*local);
            let alloca = LLVMBuildAlloca(self.builder, ty, c"local".as_ptr());
            locals.push(alloca);
        }
    }

    fn tid(&self, tid: sir::Tid) -> LLVMTypeRef {
        self.types[&tid].0
    }

    /// Build an LLVM type from a SIR type.
    unsafe fn ty_step1(&self, tid: sir::Tid) -> (LLVMTypeRef, Option<LLVMTypeRef>) {
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

    unsafe fn ty_step2(&self, tid: sir::Tid) {
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
