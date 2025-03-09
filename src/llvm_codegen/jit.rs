use crate::sir;
use llvm_sys::ir_reader::LLVMParseIRInContext;
use llvm_sys::{
    core::*,
    error::LLVMGetErrorMessage,
    orc2::{lljit::*, *},
    prelude::*,
};
use std::ffi::CString;
use std::ptr;

pub struct JIT {
    context: LLVMContextRef,
    jit: LLVMOrcLLJITRef,
}

impl Drop for JIT {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.context);
            LLVMOrcDisposeLLJIT(self.jit);
        }
    }
}

impl JIT {
    pub unsafe fn new() -> Self {
        let context = LLVMContextCreate();
        let mut jit = ptr::null_mut();
        let err = LLVMOrcCreateLLJIT(&mut jit, ptr::null_mut());

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        Self { context, jit }
    }

    pub unsafe fn load_module(&self, module: String) -> LLVMModuleRef {
        let module = CString::new(module).unwrap();

        let membuf = LLVMCreateMemoryBufferWithMemoryRange(
            module.as_ptr(),
            module.as_bytes().len(),
            c"module".as_ptr(),
            0,
        );

        let mut module = ptr::null_mut();
        let mut msg = ptr::null_mut();

        if LLVMParseIRInContext(self.context, membuf, &mut module, &mut msg).is_negative() {
            let err = CString::from_raw(msg);
            panic!("{}", err.to_str().unwrap());
        };

        module
    }

    pub unsafe fn run(&self, module: LLVMModuleRef, entry: sir::Bid) {
        let dylib = LLVMOrcLLJITGetMainJITDylib(self.jit);

        // load `gc.o` object file
        let gc = CString::new("gc.o").unwrap();

        let mut membuf = ptr::null_mut();
        let mut err = ptr::null_mut();

        if LLVMCreateMemoryBufferWithContentsOfFile(gc.as_ptr(), &mut membuf, &mut err) != 0 {
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        let err = LLVMOrcLLJITAddObjectFile(self.jit, dylib, membuf);

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        let ctx = LLVMOrcCreateNewThreadSafeContext();
        let tsm = LLVMOrcCreateNewThreadSafeModule(module, ctx);

        let err = LLVMOrcLLJITAddLLVMIRModule(self.jit, dylib, tsm);

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        let entry = CString::new(format!("body_{}", entry.0)).unwrap();

        let mut func_addr = 0;
        let err = LLVMOrcLLJITLookup(self.jit, &mut func_addr, entry.as_ptr());

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        let func: extern "C" fn() -> i64 = std::mem::transmute(func_addr);

        println!("Calling entry function: {}", func());
    }
}
