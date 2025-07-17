#![allow(dead_code)]
mod anf;
pub mod ast;
mod attribute;
mod cli;
mod compiler;
mod hir2;
mod lower2;
mod lua;
pub mod parse;
mod v1;
mod x64;
mod x86;

use diagnostic::Sources;

use crate::cli::execute_cli;

fn main() {
    let mut sources = Sources::new();

    if let Err(diagnostic) = execute_cli(&mut sources) {
        let mut writer = std::io::stdout();
        let mut formatter = diagnostic::Formatter::new(&mut writer, &sources);
        formatter.write_report(&diagnostic).unwrap();
        std::process::exit(1);
    }
}
