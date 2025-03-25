use super::llvm_sys::{
    core::*,
    error::LLVMGetErrorMessage,
    ir_reader::LLVMParseIRInContext,
    orc2::{lljit::*, *},
    prelude::*,
};
use std::ffi::CString;
use std::path::PathBuf;
use std::ptr;

pub struct Jit {
    context: LLVMContextRef,
    jit: LLVMOrcLLJITRef,
}

impl Drop for Jit {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.context);
            LLVMOrcDisposeLLJIT(self.jit);
        }
    }
}

impl Jit {
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

    pub unsafe fn load_module(&self, module: &str) -> LLVMModuleRef {
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

    unsafe fn load_object_file(&self, dylib: LLVMOrcJITDylibRef, path: PathBuf) {
        assert!(path.exists(), "object file not found");
        let obj_path_str = CString::new(path.to_str().unwrap()).unwrap();
        let mut membuf = ptr::null_mut();
        let mut msg = ptr::null_mut();

        if LLVMCreateMemoryBufferWithContentsOfFile(obj_path_str.as_ptr(), &mut membuf, &mut msg)
            .is_negative()
        {
            let err = CString::from_raw(msg);
            panic!("{}", err.to_str().unwrap());
        }

        let err = LLVMOrcLLJITAddObjectFile(self.jit, dylib, membuf);

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }
    }

    pub unsafe fn run(&self, module: LLVMModuleRef, entry: &str) {
        let dylib = LLVMOrcLLJITGetMainJITDylib(self.jit);

        self.load_object_file(dylib, "obj/release/runtime.o".into());
        self.load_object_file(dylib, "obj/release/std_io.o".into());
        self.load_object_file(dylib, "obj/release/std_os.o".into());
        self.load_object_file(dylib, "obj/release/std_string.o".into());
        self.load_object_file(dylib, "obj/release/std_list.o".into());

        let ctx = LLVMOrcCreateNewThreadSafeContext();
        let tsm = LLVMOrcCreateNewThreadSafeModule(module, ctx);

        let err = LLVMOrcLLJITAddLLVMIRModule(self.jit, dylib, tsm);

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        let entry = CString::new(entry).unwrap();

        let mut func_addr = 0;
        let err = LLVMOrcLLJITLookup(self.jit, &mut func_addr, entry.as_ptr());

        if !err.is_null() {
            let err = LLVMGetErrorMessage(err);
            let err = CString::from_raw(err);
            panic!("{}", err.to_str().unwrap());
        }

        let func: unsafe extern "C" fn() = std::mem::transmute(func_addr);

        func();
    }
}
