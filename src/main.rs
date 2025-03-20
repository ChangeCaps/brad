#![allow(dead_code)]

use clap::{Args, Parser, Subcommand};
use diagnostic::{Source, Sources};
use parse::{Interner, Tokens};
use std::path::{Path, PathBuf};

use crate::ast::GeneratorOptions;
use compiler::Compiler;

mod ast;
mod attribute;
mod compiler;
mod diagnostic;
mod hir;
mod hir2;
mod interpret;
mod lir;
mod llvm_codegen;
mod lower;
mod lower2;
mod lua;
mod mir;
mod parse;
mod sir;
mod solve;

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
    #[arg(required = true)]
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
    Hir2(ModuleArgs),
    Lua(ModuleArgs),
    RandomAst {
        #[command(flatten)]
        options: GeneratorOptions,
        #[command(flatten)]
        formatter_options: ast::FormatterOptions,
    },
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
    Ast {
        #[command(flatten)]
        file: FileArgs,
        #[command(flatten)]
        options: ast::FormatterOptions,
    },
    // Hir(FileArgs),
    Mir(ModuleArgs),
    // Lir(FileArgs),
}

fn main2(sources: &mut Sources) -> Result<(), diagnostic::Report> {
    let args = Cli::parse();

    match &args.command {
        Cmd::RandomAst {
            options,
            formatter_options,
        } => {
            let mut generator = ast::Generator::new(options.clone());
            let module = generator.generate();
            let mut formatter = ast::Formatter::new(std::io::stdout(), formatter_options.clone());
            formatter.format_module(&module).unwrap();
            Ok(())
        }

        Cmd::Lex(f)
        | Cmd::Ast(f)
        | Cmd::Fmt {
            command: FmtCmd::Ast { file: f, .. },
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
                    command: FmtCmd::Ast { .. },
                } => {
                    let ast = parse::module(&mut tokens)?;
                    let mut formatter =
                        ast::Formatter::new(std::io::stdout(), ast::FormatterOptions::default());
                    formatter.format_module(&ast).unwrap();
                }
                _ => unreachable!(),
            };

            Ok(())
        }

        Cmd::Hir2(f) => {
            let mut rep = diagnostic::Report::new();

            let mut compiler = Compiler::new(sources);

            for package in f.packages.iter() {
                let name = Path::new(package).file_stem().unwrap().to_str().unwrap();
                compiler.add_package(name, package).unwrap();
            }

            compiler.tokenize2(&mut rep).map_err(|_| rep.clone())?;
            compiler.parse2(&mut rep).map_err(|_| rep.clone())?;

            let _ = compiler.lower2(&mut rep).map_err(|_| rep.clone())?;

            Ok(())
        }

        Cmd::Lua(f) => {
            let mut rep = diagnostic::Report::new();

            let mut compiler = Compiler::new(sources);

            for package in f.packages.iter() {
                let name = Path::new(package).file_stem().unwrap().to_str().unwrap();
                compiler.add_package(name, package).unwrap();
            }

            compiler.tokenize2(&mut rep).map_err(|_| rep.clone())?;
            compiler.parse2(&mut rep).map_err(|_| rep.clone())?;

            let lua = compiler.lua(&mut rep).map_err(|_| rep)?;

            std::fs::write("out.lua", lua).unwrap();

            let output = std::process::Command::new("lua")
                .arg("out.lua")
                .output()
                .expect("failed to execute process");

            print!("{}", String::from_utf8_lossy(&output.stdout));

            Ok(())
        }

        Cmd::Interpret(f)
        | Cmd::Compile(f)
        | Cmd::Fmt {
            command: FmtCmd::Mir(f),
        } => {
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

            match &args.command {
                Cmd::Interpret(_) => {
                    compiler.interpret(entrypoint.as_str())?;
                    Ok(())
                }
                Cmd::Compile(_) => {
                    let llvm_ir = compiler.compile(entrypoint.clone().as_str())?;

                    if let Some(output) = &f.output {
                        std::fs::write(output, llvm_ir.clone()).unwrap();
                    } else {
                        //println!("{}", llvm_ir);
                    }

                    if !f.dry_run {
                        compiler.jit(entrypoint.as_str(), llvm_ir)?;
                    };

                    Ok(())
                }
                Cmd::Fmt {
                    command: FmtCmd::Mir(_),
                } => {
                    compiler.tokenize()?;
                    compiler.parse()?;
                    let hir = compiler.lower()?;
                    let mir = compiler.mir(hir)?;
                    let mut formatter = mir::Formatter::new(std::io::stdout(), &mir);
                    formatter.format_program().unwrap();
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
        formatter.write_report(&diagnostic).unwrap();
    }
}
