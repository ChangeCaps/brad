#![allow(dead_code)]

use ast::Formatter;
use clap::{Args, Parser, Subcommand};
use diagnostic::SourceId;
use parse::{Interner, Tokens};
use std::path::PathBuf;

use compiler::Compiler;

mod ast;
mod compiler;
mod diagnostic;
mod hir;
mod interpret;
mod lower;
mod mir;
mod parse;

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
}

#[derive(Subcommand)]
pub enum FmtCmd {
    Ast(FileArgs),
    // Hir(FileArgs),
    // Mir(FileArgs),
    // Lir(FileArgs),
}

fn create_compiler(module_args: ModuleArgs) -> Compiler {
    let mut compiler = Compiler::new();

    compiler
        .add_package(module_args.module.clone().as_str(), module_args.module)
        .unwrap();

    compiler
}

fn main() {
    let args = Cli::parse();

    match &args.command {
        Cmd::Lex(f)
        | Cmd::Ast(f)
        | Cmd::Fmt {
            command: FmtCmd::Ast(f),
        } => {
            let mut interner = Interner::new();
            let input = std::fs::read_to_string(f.file.clone()).unwrap();
            let mut tokens = Tokens::tokenize(&mut interner, SourceId(0_u32), &input).unwrap();

            match &args.command {
                Cmd::Lex(_) => println!("{:#?}", tokens),
                Cmd::Ast(_) => {
                    let ast = parse::module(&mut tokens).unwrap();
                    println!("{:#?}", ast);
                }
                Cmd::Fmt {
                    command: FmtCmd::Ast(_),
                } => {
                    let ast = parse::module(&mut tokens).unwrap();
                    let mut formatter = Formatter::new(std::io::stdout());
                    formatter.format_module(&ast).unwrap();
                }
                _ => unreachable!(),
            }
        }

        Cmd::Interpret(f) => {
            let mut compiler = create_compiler(f.clone());

            if let Err(diagnostic) = compiler.compile() {
                let mut writer = std::io::stdout();
                let mut formatter = diagnostic::Formatter::new(&mut writer, compiler.sources());
                formatter.write(&diagnostic).unwrap();
            }
        }
    }
}
