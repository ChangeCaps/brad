#![allow(dead_code)]

use ast::Formatter;
use clap::{Args, Parser, Subcommand};
use diagnostic::{Source, Sources};
use parse::{Interner, Tokens};
use std::path::PathBuf;

use compiler::Compiler;

mod ast;
mod compiler;
mod diagnostic;
mod hir;
mod interpret;
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
    module: String,
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
            let module_args = f.clone();
            let mut compiler = Compiler::new(sources);

            compiler
                .add_package(module_args.module.clone().as_str(), module_args.module)
                .unwrap();

            match &args.command {
                Cmd::Interpret(_) => compiler.interpret(),
                Cmd::Compile(_) => compiler.compile(),
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
