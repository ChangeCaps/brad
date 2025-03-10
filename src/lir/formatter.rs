use super::{Bid, Body, Program, Stmt, Tid, Ty};

pub fn format(program: &Program) -> &'static str {
    for i in 0..program.types.len() {
        let str_vec = format_type(program, Tid(i as u32));
    }

    for i in 0..program.bodies.len() {
        let str_vec = format_body(program, Bid(i as u32));
    }

    todo!()
}

fn format_type(program: &Program, tid: Tid) -> Vec<&'static str> {
    todo!()
}

fn format_body(program: &Program, bid: Bid) -> Vec<&'static str> {
    todo!()
}
