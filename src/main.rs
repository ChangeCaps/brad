use std::fs;

mod diagnostic;
mod parse;

fn main() {
    let input = fs::read_to_string("test.brad").unwrap();
    let mut interner = parse::Interner::new();
    let mut sources = diagnostic::Sources::new();
    let source = sources.push(diagnostic::Source {
        content: input.clone(),
        file: String::from("test.brad"),
    });
    let tokens = parse::Tokens::tokenize(&mut interner, source, &input).unwrap();

    println!("{:?}", tokens);
}
