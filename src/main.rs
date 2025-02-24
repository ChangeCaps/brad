#![allow(dead_code)]

use compiler::Compiler;

mod ast;
mod compiler;
mod diagnostic;
mod hir;
mod lower;
mod parse;

fn main() {
    let mut compiler = Compiler::new();

    compiler.add_package("test", "test").unwrap();

    if let Err(diagnostic) = compiler.compile() {
        let mut writer = std::io::stdout();
        let mut formatter = diagnostic::Formatter::new(&mut writer, compiler.sources());
        formatter.write(&diagnostic).unwrap();
    }
}
