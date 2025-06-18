// TODO :: javad things

use crate::{anf, hir2 as hir};

pub fn build(program: &hir::SpecializedProgram) -> anf::Program {
    let mut ir = anf::Program::default();

    for (_, body) in program.bodies.iter() {
        println!("Building body: {}", body.name);

        let locals = anf::Locals::default();

        let ctx = BuildContext {
            hir: program,
            ir: &mut ir,
            locals,
        };

        _ = ctx;
    }

    todo!()
}

struct BuildContext<'a> {
    hir: &'a hir::SpecializedProgram,
    ir: &'a mut anf::Program,
    locals: anf::Locals,
}
