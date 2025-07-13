use crate::ast::GeneratorOptions;
use crate::compiler::Compiler;
use crate::parse::{Interner, Tokens};
use crate::v1::cli::{execute_v1, PipelineV1Cmd};
use crate::{ast, parse};
use clap::{Args, Parser, Subcommand};
use diagnostic::{Report, Source, Sources};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::fs;

#[derive(Args, Clone)]
pub struct FileArgs {
    /// File path to a source file, as a positional argument
    pub(crate) file: PathBuf,
}

#[derive(Args, Clone)]
pub struct ModuleArgs {
    /// Module name to interpret, as a positional argument
    #[arg(required = true)]
    pub(crate) packages: Vec<String>,
    /// Output results to a specific file, as an optional argument
    #[arg(short, long)]
    pub(crate) output: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum PipelineV2Cmd {
    Hir(ModuleArgs),
    Lua(ModuleArgs),
    SimpleSpecPipeline(ModuleArgs),
}

#[derive(Subcommand)]
pub enum Cmd {
    Lex(FileArgs),
    RandomAst {
        #[command(flatten)]
        options: GeneratorOptions,
        #[command(flatten)]
        formatter_options: ast::FormatterOptions,
    },
    Fmt {
        #[command(flatten)]
        file: FileArgs,
        #[command(flatten)]
        options: ast::FormatterOptions,
    },
    V1 {
        #[command(subcommand)]
        command: PipelineV1Cmd,
    },
    V2 {
        #[command(subcommand)]
        command: PipelineV2Cmd,
    },
}

#[derive(Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub(crate) command: Cmd,
}

fn execute_v2(cmd: &PipelineV2Cmd, sources: &mut Sources) -> Result<(), Report> {
    let f = match cmd {
        PipelineV2Cmd::Hir(f) => f,
        PipelineV2Cmd::Lua(f) => f,
        PipelineV2Cmd::SimpleSpecPipeline(f) => f,
    };

    let mut rep = Report::new();

    let mut compiler = Compiler::new(sources);

    for package in f.packages.iter() {
        let name = Path::new(package).file_stem().unwrap().to_str().unwrap();
        compiler.add_package(name, package).unwrap();
    }

    compiler.tokenize2(&mut rep).map_err(|_| rep.clone())?;
    compiler.parse2(&mut rep).map_err(|_| rep.clone())?;

    match cmd {
        PipelineV2Cmd::Hir(_) => {
            let _ = compiler.lower2(&mut rep).map_err(|_| rep.clone())?;
            Ok(())
        }
        PipelineV2Cmd::Lua(_) => {
            let file_path = f.output.as_ref().map_or("out.lua", |p| p.to_str().unwrap());
            let mut file = fs::File::create(file_path).unwrap();

            compiler.lua(&mut rep, &mut file).map_err(|_| rep.clone())?;

            let main_package = Path::new(f.packages.last().unwrap())
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap();

            let entrypoint = format!("{}::main", main_package);

            file.write_all(format!("M['{}']()", entrypoint).as_bytes())
                .unwrap();

            file.flush().unwrap();

            drop(file);

            Ok(())
        }
        PipelineV2Cmd::SimpleSpecPipeline(_) => {
            let hir = compiler.lower2(&mut rep).map_err(|_| rep.clone())?;
            let spec = crate::anf::simple_spec::simple_spec(&hir);
            let anf = crate::anf::BuildContext::build(&spec);
            println!("ANF: {:#?}", anf);
            let builder = crate::x86::build::Builder::new(anf);
            builder.build();
            Ok(())
        }
    }
}

pub fn execute_cli(sources: &mut Sources) -> Result<(), Report> {
    let args = Cli::parse();

    match &args.command {
        Cmd::Lex(f) | Cmd::Fmt { file: f, .. } => {
            let mut interner = Interner::new();
            let content = fs::read_to_string(f.file.clone()).unwrap();

            let source = Source {
                content,
                file: f.file.clone(),
            };

            let source_id = sources.push(source);

            let mut tokens =
                Tokens::tokenize(&mut interner, source_id, &sources[source_id].content)?;

            match &args.command {
                Cmd::Lex(_) => println!("{:#?}", tokens),
                Cmd::Fmt { options, .. } => {
                    let module = parse::module(&mut tokens)?;

                    println!("Module: {:#?}", module);

                    let mut formatter = ast::Formatter::new(std::io::stdout(), options.clone());
                    formatter.format_module(&module).unwrap();
                }
                _ => unreachable!(),
            };

            Ok(())
        }
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
        Cmd::V1 { command } => execute_v1(command, sources),
        Cmd::V2 { command } => execute_v2(command, sources),
    }
}
