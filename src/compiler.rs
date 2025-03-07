use crate::{
    ast,
    diagnostic::{Diagnostic, Source, SourceId, Sources},
    hir,
    interpret::Interpreter,
    llvm_codegen::codegen,
    lower::Lowerer,
    mir,
    parse::{self, Interner, Tokens},
};
use std::{fs, io, path::Path};

pub struct Compiler<'a> {
    interner: Interner,
    sources: &'a mut Sources,
    files: Vec<File>,
}

struct File {
    path: Vec<&'static str>,
    source: SourceId,
    tokens: Option<Tokens>,
    ast: Option<ast::Module>,
}

impl<'a> Compiler<'a> {
    pub fn new(sources: &'a mut Sources) -> Self {
        Self {
            interner: Interner::new(),
            sources,
            files: Vec::new(),
        }
    }

    pub fn add_package(&mut self, name: &str, path: impl AsRef<Path>) -> io::Result<()> {
        let name = self.interner.intern(name);
        self.add_directory(vec![name], path.as_ref())
    }

    pub fn sources(&self) -> &Sources {
        self.sources
    }

    fn add_directory(&mut self, modules: Vec<&'static str>, path: &Path) -> io::Result<()> {
        for entry in path.read_dir()? {
            let entry = entry?;

            let path = entry.path();
            if path.is_dir() {
                let name = entry.file_name().to_string_lossy().into_owned();

                let mut modules = modules.clone();
                modules.push(self.interner.intern(&name));

                self.add_directory(modules, &path)?;
            } else {
                if path.extension() != Some("bd".as_ref()) {
                    continue;
                }

                let content = fs::read_to_string(&path)?;

                let source = Source {
                    content,
                    file: path,
                };

                self.files.push(File {
                    path: modules.clone(),
                    source: self.sources.push(source),
                    tokens: None,
                    ast: None,
                });
            }
        }

        Ok(())
    }

    pub fn tokenize(&mut self) -> Result<(), Diagnostic> {
        for file in &mut self.files {
            let source = &self.sources[file.source];
            let tokens = Tokens::tokenize(&mut self.interner, file.source, &source.content)?;
            file.tokens = Some(tokens);
        }

        Ok(())
    }

    pub fn parse(&mut self) -> Result<(), Diagnostic> {
        for file in &mut self.files {
            let tokens = &mut file.tokens.take().unwrap();
            let ast = parse::module(tokens)?;
            file.ast = Some(ast);
        }

        Ok(())
    }

    pub fn lower(&mut self) -> Result<hir::Program, Diagnostic> {
        let mut lowerer = Lowerer::new();

        for file in &mut self.files {
            lowerer.add_module(&file.path, file.ast.take().unwrap());
        }

        lowerer.lower()
    }

    pub fn mir(&self, hir: hir::Program) -> Result<(mir::Program, mir::Bid), Diagnostic> {
        mir::build(&hir)
    }

    pub fn interpret(&mut self) -> Result<(), Diagnostic> {
        self.tokenize()?;
        self.parse()?;
        let hir = self.lower()?;
        let (mir, main) = self.mir(hir)?;
        let (sir, main) = mir::specialize(mir, main);
        let interpreter = Interpreter::new(sir);
        interpreter.run(main);
        Ok(())
    }

    pub fn compile(&mut self) -> Result<(), Diagnostic> {
        self.tokenize()?;
        self.parse()?;
        let hir = self.lower()?;
        let (mir, main) = self.mir(hir)?;
        let (sir, main) = mir::specialize(mir, main);

        codegen(sir, main);

        Ok(())
    }
}
