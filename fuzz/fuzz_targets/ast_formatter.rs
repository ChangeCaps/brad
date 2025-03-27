#![no_main]

use brad::ast::{Generator, GeneratorOptions, Module};
use libfuzzer_sys::arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::{arbitrary, fuzz_target};

#[derive(Debug)]
struct ModuleWithGenerator {
    module: Module,
    generator: Generator,
}

impl<'a> Arbitrary<'a> for ModuleWithGenerator {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let options = GeneratorOptions {
            enable_match: u.arbitrary()?,
            enable_loop: u.arbitrary()?,
            enable_ref_ty: u.arbitrary()?,
            enable_list_ty: u.arbitrary()?,
            enable_unary: u.arbitrary()?,
            enable_floats: u.arbitrary()?,
            max_functions: u.int_in_range(1..=10)?,
            max_function_args: u.int_in_range(0..=10)?,
            min_exprs: u.int_in_range(0..=9)?,
            max_exprs: u.int_in_range(10..=10)?,
            max_match_arms: u.int_in_range(0..=10)?,
            max_union_size: u.int_in_range(2..=10)?,
            max_record_size: u.int_in_range(0..=10)?,
            max_tuple_size: u.int_in_range(2..=10)?,
            max_tys_rounds: u.int_in_range(0..=10)?,
            max_depth: u.int_in_range(0..=10)?,
            max_complexity: u.int_in_range(0..=100)?,
            max_bruteforce_attempts: u.int_in_range(10..=100)?,
        };

        let mut generator = Generator::new(options);
        let module = generator.generate();

        Ok(ModuleWithGenerator { module, generator })
    }
}

fuzz_target!(|data: ModuleWithGenerator| {
    // fuzzed code goes here
    let mut data1 = Vec::new();
    let cursor = std::io::Cursor::new(&mut data1);

    let mut formatter = brad::ast::Formatter::new(cursor, Default::default());

    formatter.format_module(&data.module).unwrap();

    let mut sources = brad::diagnostic::Sources::new();
    let mut interner = brad::parse::Interner::new();

    let content = String::from_utf8(data1).unwrap();

    let source = brad::diagnostic::Source {
        content,
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

    // So cursed.
    unsafe {
        data.generator.interner.clone().clear();
        interner.clear();
    }

    // Assert data1 and data2 are equal
    assert_eq!(
        sources[source_id].content,
        String::from_utf8(data2).unwrap()
    );
});
