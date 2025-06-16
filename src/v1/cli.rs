use crate::cli::ModuleArgs;
use crate::compiler::Compiler;
use crate::v1;
use clap::Subcommand;
use diagnostic::{Report, Sources};
use std::fs;
use std::path::Path;

#[derive(Subcommand)]
pub enum PipelineV1Cmd {
    Mir(ModuleArgs),
    Interpret(ModuleArgs),
    #[clap(name = "llvm-ir")]
    LLVMIr(ModuleArgs),
    #[clap(name = "llvm-jit")]
    LLVMJit(ModuleArgs),
}

pub fn execute_v1(cmd: &PipelineV1Cmd, sources: &mut Sources) -> Result<(), Report> {
    let f = match &cmd {
        PipelineV1Cmd::Mir(f) => f,
        PipelineV1Cmd::Interpret(f) => f,
        PipelineV1Cmd::LLVMIr(f) => f,
        PipelineV1Cmd::LLVMJit(f) => f,
    };

    let packages = f.packages.clone();
    let mut compiler = Compiler::new(sources);

    for package in packages.iter() {
        let name = Path::new(package).file_stem().unwrap().to_str().unwrap();
        compiler.add_package(name, package).unwrap();
    }

    let main_package = Path::new(packages.last().unwrap())
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();

    let entrypoint = format!("{}::main", main_package);

    match cmd {
        PipelineV1Cmd::Mir(_) => {
            compiler.tokenize()?;
            compiler.parse()?;
            let hir = compiler.lower()?;
            let mir = compiler.mir(hir)?;
            let mut formatter = v1::mir::Formatter::new(std::io::stdout(), &mir);
            formatter.format_program().unwrap();
            Ok(())
        }
        PipelineV1Cmd::Interpret(_) => compiler.interpret(entrypoint.as_str()),
        PipelineV1Cmd::LLVMIr(_) => {
            let llvm_ir = compiler.compile(entrypoint.clone().as_str())?;

            if let Some(output) = &f.output {
                fs::write(output, llvm_ir.clone()).unwrap();
            } else {
                println!("{}", llvm_ir);
            }

            Ok(())
        }
        PipelineV1Cmd::LLVMJit(_) => {
            let llvm_ir = compiler.compile(entrypoint.clone().as_str())?;
            compiler.jit(entrypoint.as_str(), llvm_ir)
        }
    }
}
