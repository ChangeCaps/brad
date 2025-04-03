use crate::v1::interpret::Interpreter;
use crate::{
    ast,
    diagnostic::{Diagnostic, Report, Reporter, Source, SourceId, Sources},
    hir2, lower2, lua,
    parse::{self, Interner, Tokens},
    v1::hir,
    v1::lower,
    v1::mir,
};
use std::{
    fs,
    io::{self, Write},
    path::Path,
};

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

        if path.as_ref().is_dir() {
            self.add_directory(vec![name], path.as_ref())
        } else {
            self.add_file(&[name], path.as_ref())
        }
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
                self.add_file(&modules, &path)?;
            }
        }

        Ok(())
    }

    fn add_file(&mut self, modules: &[&'static str], path: &Path) -> io::Result<()> {
        if path.extension() != Some("bd".as_ref()) {
            return Ok(());
        }

        let content = fs::read_to_string(path)?;

        let source = Source {
            content,
            file: path.to_path_buf(),
        };

        self.files.push(File {
            path: modules.to_vec(),
            source: self.sources.push(source),
            tokens: None,
            ast: None,
        });

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

    pub fn tokenize2(&mut self, reporter: &mut dyn Reporter) -> Result<(), ()> {
        let mut is_error = false;

        for file in &mut self.files {
            let source = &self.sources[file.source];

            match Tokens::tokenize(&mut self.interner, file.source, &source.content) {
                Ok(tokens) => file.tokens = Some(tokens),
                Err(diagnostic) => {
                    reporter.emit(diagnostic);
                    is_error = true;
                }
            }
        }

        match is_error {
            true => Err(()),
            false => Ok(()),
        }
    }

    pub fn parse(&mut self) -> Result<(), Diagnostic> {
        for file in &mut self.files {
            let tokens = &mut file.tokens.take().unwrap();
            let ast = parse::module(tokens)?;
            file.ast = Some(ast);
        }

        Ok(())
    }

    pub fn parse2(&mut self, reporter: &mut dyn Reporter) -> Result<(), ()> {
        let mut is_error = false;

        for file in &mut self.files {
            let tokens = &mut file.tokens.take().unwrap();

            match parse::module(tokens) {
                Ok(ast) => file.ast = Some(ast),
                Err(diagnostic) => {
                    reporter.emit(diagnostic);
                    is_error = true;
                }
            }
        }

        match is_error {
            true => Err(()),
            false => Ok(()),
        }
    }

    pub fn lower(&mut self) -> Result<hir::Program, Report> {
        let mut lowerer = lower::Lowerer::new();

        for file in &mut self.files {
            lowerer.add_module(&file.path, file.ast.take().unwrap());
        }

        lowerer.finish().map_err(Into::into)
    }

    pub fn lower2(&mut self, reporter: &mut dyn Reporter) -> Result<hir2::Program, ()> {
        let mut lowerer = lower2::Lowerer::new(reporter, &mut self.interner);

        for file in &mut self.files {
            lowerer.add_module(&file.path, file.ast.take().unwrap());
        }

        lowerer.finish()
    }

    pub fn lua(
        &mut self,
        reporter: &mut dyn Reporter,
        writer: &mut impl Write,
    ) -> Result<String, ()> {
        let hir = self.lower2(reporter)?;
        lua::codegen(writer, &hir).unwrap();
        Ok(String::new())
    }

    pub fn mir(&self, hir: hir::Program) -> Result<mir::Program, Diagnostic> {
        mir::build(&hir)
    }

    pub fn interpret(&mut self, entrypoint: &str) -> Result<(), Report> {
        self.tokenize()?;
        self.parse()?;
        let hir = self.lower()?;
        let mir = self.mir(hir)?;
        let main = mir.find_body(entrypoint)?;
        let (sir, main) = mir::specialize(mir, main);
        let interpreter = Interpreter::new(sir);
        interpreter.run(main);
        Ok(())
    }

    pub fn compile(&mut self, entrypoint: &str) -> Result<String, Report> {
        self.tokenize()?;
        self.parse()?;
        let hir = self.lower()?;
        let mir = self.mir(hir)?;

        let bid = mir.find_body(entrypoint)?;
        let (sir, _main) = mir::specialize(mir, bid);
        //let (lir, _) = lir::build(&sir, main)?;
        //let mut formatter = lir::Formatter::new(std::io::stdout());
        //formatter.format(&lir).unwrap();

        Ok(crate::v1::llvm_codegen::codegen(sir))
    }

    pub fn jit(&self, entrypoint: &str, llvm_ir: String) -> Result<(), Report> {
        crate::v1::llvm_codegen::jit(llvm_ir.as_str(), entrypoint);
        Ok(())
    }
}
