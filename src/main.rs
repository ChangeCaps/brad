#![allow(dead_code)]

use std::fs;

use diagnostic::{Diagnostic, Source, SourceId, Sources};
use parse::Interner;

mod ast;
mod diagnostic;
mod hir;
mod parse;

fn main() {
    let input = fs::read_to_string("test.brad").unwrap();
    let mut interner = Interner::new();
    let mut sources = Sources::new();
    let source = sources.push(Source {
        content: input.clone(),
        file: String::from("test.brad"),
    });

    match compile(&mut interner, source, &input) {
        Ok(ast) => {
            println!("{:#?}", ast);
        }
        Err(diagnostic) => {
            let mut writer = std::io::stdout();
            let mut formatter = diagnostic::Formatter::new(&mut writer, &sources);
            formatter.write(&diagnostic).unwrap();
        }
    }
}

fn compile(
    interner: &mut Interner,
    source: SourceId,
    input: &str,
) -> Result<ast::Module, Diagnostic> {
    let mut tokens = parse::Tokens::tokenize(interner, source, input)?;
    let ast = parse::module(&mut tokens)?;

    Ok(ast)
}
