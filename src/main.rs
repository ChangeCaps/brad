#![allow(dead_code)]
pub mod ast;
mod attribute;
mod cli;
mod compiler;
mod hir2;
mod ir;
mod lower2;
mod lua;
pub mod parse;
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
