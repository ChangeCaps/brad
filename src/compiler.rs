use std::{fs, io, path::Path};

use crate::{
    diagnostic::{Diagnostic, Source, SourceId, Sources},
    lower::Lowerer,
    parse::{self, Interner, Tokens},
};

pub struct Compiler {
    interner: Interner,
    sources: Sources,
    files: Vec<File>,
}

struct File {
    path: Vec<&'static str>,
    source: SourceId,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            interner: Interner::new(),
            sources: Sources::new(),
            files: Vec::new(),
        }
    }

    pub fn add_package(&mut self, name: &str, path: impl AsRef<Path>) -> io::Result<()> {
        let name = self.interner.intern(name);
        self.add_directory(vec![name], path.as_ref())
    }

    pub fn sources(&self) -> &Sources {
        &self.sources
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
                });
            }
        }

        Ok(())
    }

    pub fn compile(&mut self) -> Result<(), Diagnostic> {
        let mut lowerer = Lowerer::new();

        for file in &self.files {
            let source = &self.sources[file.source];
            let mut tokens = Tokens::tokenize(&mut self.interner, file.source, &source.content)?;
            let ast = parse::module(&mut tokens)?;

            lowerer.add_module(&file.path, ast);
        }

        let _hir = lowerer.lower()?;

        Ok(())
    }
}
