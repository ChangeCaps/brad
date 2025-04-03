#![no_main]

use brad::ast::{Generator, GeneratorOptions, Module, Spanned};
use libfuzzer_sys::arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::{arbitrary, fuzz_target};

#[derive(Debug, Clone, PartialEq)]
struct RandomModule(Module);

impl<'a> Arbitrary<'a> for RandomModule {
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
            min_exprs: 0,
            max_exprs: 2,
            max_match_arms: u.int_in_range(0..=10)?,
            max_union_size: u.int_in_range(2..=10)?,
            max_record_size: u.int_in_range(0..=10)?,
            max_tuple_size: u.int_in_range(2..=10)?,
            max_tys_rounds: u.int_in_range(0..=1)?,
            max_depth: u.int_in_range(0..=10)?,
            max_complexity: u.int_in_range(0..=100)?,
            max_bruteforce_attempts: u.int_in_range(10..=100)?,
            seed: Some(u.int_in_range(0..=u64::MAX)?),
        };

        let mut generator = Generator::new(options);
        let module = generator.generate();

        Ok(Self(module))
    }
}

fuzz_target!(|data: RandomModule| {
    // fuzzed code goes here
    let mut data1 = Vec::new();
    let cursor = std::io::Cursor::new(&mut data1);

    let mut formatter = brad::ast::Formatter::new(cursor, Default::default());

    formatter.format_module(&data.0).unwrap();

    let mut sources = brad::diagnostic::Sources::new();
    let mut interner = brad::ast::INTERNER.lock().unwrap();

    let content = String::from_utf8(data1).unwrap();

    let source = brad::diagnostic::Source {
        content,
        file: "/dev/null".into(),
    };

    let source_id = sources.push(source);
    let mut tokens =
        brad::parse::Tokens::tokenize(&mut interner, source_id, &sources[source_id].content)
            .unwrap();

    // Recursively visit all modules and replace all spans with (0, 0, 0)
    let mut parsed = brad::parse::module(&mut tokens).unwrap();
    parsed.reset_spans();

    // Assert the two ASTs are equal
    assert_eq!(parsed, data.0, "Parsed AST does not match generated AST");

    let mut data2 = Vec::new();
    let cursor = std::io::Cursor::new(&mut data2);
    let mut formatter = brad::ast::Formatter::new(cursor, Default::default());

    formatter.format_module(&parsed).unwrap();

    // So cursed.
    unsafe {
        interner.clear();
    }

    // Assert data1 and data2 are equal
    assert_eq!(
        sources[source_id].content,
        String::from_utf8(data2).unwrap()
    );
});
