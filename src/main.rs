#![allow(dead_code)]
mod ast;
mod attribute;
mod cli;
mod compiler;
mod diagnostic;
mod hir2;
mod lower2;
mod lua;
mod parse;
mod solve;
mod v1;

use diagnostic::Sources;

use crate::cli::execute_cli;

fn main() {
    let mut sources = Sources::new();

    if let Err(diagnostic) = execute_cli(&mut sources) {
        let mut writer = std::io::stdout();
        let mut formatter = diagnostic::Formatter::new(&mut writer, &sources);
        formatter.write_report(&diagnostic).unwrap();
    }
}
