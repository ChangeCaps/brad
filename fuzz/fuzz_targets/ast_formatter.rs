#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: brad::ast::Module| {
    // fuzzed code goes here
    let mut data1 = Vec::new();
    let cursor = std::io::Cursor::new(&mut data1);

    let mut formatter = brad::ast::Formatter::new(cursor, Default::default());

    formatter.format_module(&data).unwrap();

    let mut sources = brad::diagnostic::Sources::new();
    let mut interner = brad::parse::Interner::new();

    let source = brad::diagnostic::Source {
        content: String::from_utf8(data1.clone()).unwrap(),
        file: "/dev/null".into(),
    };

    let source_id = sources.push(source);
    let mut tokens =
        brad::parse::Tokens::tokenize(&mut interner, source_id, &sources[source_id].content)
            .unwrap();

    let parsed = brad::parse::module(&mut tokens).unwrap();

    let mut data2 = Vec::new();
    let cursor = std::io::Cursor::new(&mut data2);
    let mut formatter = brad::ast::Formatter::new(cursor, Default::default());

    formatter.format_module(&parsed).unwrap();

    // Assert data1 and data2 are equal
    assert_eq!(data1, data2);
});
