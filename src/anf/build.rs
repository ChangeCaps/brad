// TODO :: javad things

use crate::{anf, hir2 as hir};

pub fn build(program: &hir::Program<anf::Type>) -> anf::Program {
    let mut ir = anf::Program::default();

    for (bid, body) in program.bodies.iter() {
        println!("Building body: {}", body.name);

        let mut locals = anf::Locals::default();

        let mut ctx = BuildContext {
            hir: program,
            ir: &mut ir,
        };
    }

    todo!()
}

struct BuildContext<'a> {
    hir: &'a hir::Program<anf::Type>,
    ir: &'a mut anf::Program,
}
