#![allow(dead_code)]

use ast::Formatter;
use clap::{Args, Parser, Subcommand};
use diagnostic::{Source, Sources};
use parse::{Interner, Tokens};
use std::path::{Path, PathBuf};

use compiler::Compiler;

mod ast;
mod attribute;
mod compiler;
mod diagnostic;
mod hir;
mod interpret;
mod lir;
mod llvm_codegen;
mod lower;
mod mir;
mod parse;
mod sir;

#[derive(Parser)]
pub struct Cli {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(Args, Clone)]
pub struct FileArgs {
    /// File path to a source file, as a positional argument
    file: PathBuf,
}

#[derive(Args, Clone)]
pub struct ModuleArgs {
    /// Module name to interpret, as a positional argument
    #[arg(required=true)]
    packages: Vec<String>,
    /// Output IR file, as an optional argument
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Dry-run, only compile do not run.
    #[arg(short, long, default_value = "false")]
    dry_run: bool,
}

#[derive(Subcommand)]
pub enum Cmd {
    Lex(FileArgs),
    Ast(FileArgs),
    Fmt {
        #[command(subcommand)]
        command: FmtCmd,
    },
    // Hir(ModuleArgs),
    // Mir(ModuleArgs),
    // Lir(ModuleArgs),
    Interpret(ModuleArgs),
    Compile(ModuleArgs),
}

#[derive(Subcommand)]
pub enum FmtCmd {
    Ast(FileArgs),
    // Hir(FileArgs),
    // Mir(FileArgs),
    // Lir(FileArgs),
}

fn main2(sources: &mut Sources) -> Result<(), diagnostic::Diagnostic> {
    let args = Cli::parse();

    match &args.command {
        Cmd::Lex(f)
        | Cmd::Ast(f)
        | Cmd::Fmt {
            command: FmtCmd::Ast(f),
        } => {
            let mut interner = Interner::new();
            let content = std::fs::read_to_string(f.file.clone()).unwrap();

            let source = Source {
                content,
                file: f.file.clone(),
            };

            let source_id = sources.push(source);

            let mut tokens =
                Tokens::tokenize(&mut interner, source_id, &sources[source_id].content)?;

            match &args.command {
                Cmd::Lex(_) => println!("{:#?}", tokens),
                Cmd::Ast(_) => {
                    let ast = parse::module(&mut tokens)?;
                    println!("{:#?}", ast);
                }
                Cmd::Fmt {
                    command: FmtCmd::Ast(_),
                } => {
                    let ast = parse::module(&mut tokens)?;
                    let mut formatter = Formatter::new(std::io::stdout());
                    formatter.format_module(&ast).unwrap();
                }
                _ => unreachable!(),
            };

            Ok(())
        }

        Cmd::Interpret(f) | Cmd::Compile(f) => {
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

            match &args.command {
                Cmd::Interpret(_) => compiler.interpret(main_package),
                Cmd::Compile(_) => {
                    let llvm_ir = compiler.compile(main_package)?;

                    if let Some(output) = &f.output {
                        std::fs::write(output, llvm_ir.clone()).unwrap();
                    } else {
                        println!("{}", llvm_ir);
                    }

                    if !f.dry_run {
                        compiler.jit(main_package, llvm_ir)?;
                    };

                    Ok(())
                }
                _ => unreachable!(),
            }
        }
    }
}

fn main() {
    let mut sources = Sources::new();

    if let Err(diagnostic) = main2(&mut sources) {
        let mut writer = std::io::stdout();
        let mut formatter = diagnostic::Formatter::new(&mut writer, &sources);
        formatter.write(&diagnostic).unwrap();
    }
}
