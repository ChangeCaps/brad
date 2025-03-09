use std::collections::HashMap;

use crate::{diagnostic::Diagnostic, lir, sir};

pub fn build(
    source: &sir::Program,
    entry: sir::Bid,
) -> Result<(lir::Program, lir::Bid), Diagnostic> {
    let mut program = lir::Program {
        bodies: lir::Bodies::new(),
        types: lir::Types::new(),
    };

    let mut body_map: HashMap<sir::Bid, lir::Bid> = HashMap::new();
    let mut type_map: HashMap<sir::Ty, lir::Tid> = HashMap::new();

    let mut main = Builder::new(&mut body_map, &mut type_map, source, &mut program, entry);
    main.build();

    todo!()
}

struct Builder<'a> {
    body_map: &'a mut HashMap<sir::Bid, lir::Bid>,
    type_map: &'a mut HashMap<sir::Ty, lir::Tid>,

    sir: &'a sir::Program,
    lir: &'a mut lir::Program,

    source: sir::Bid,
    body: lir::Body,

    local_map: HashMap<sir::Local, lir::Local>,
}

impl<'a> Builder<'a> {
    fn new(
        body_map: &'a mut HashMap<sir::Bid, lir::Bid>,
        type_map: &'a mut HashMap<sir::Ty, lir::Tid>,
        sir: &'a sir::Program,
        lir: &'a mut lir::Program,
        source: sir::Bid,
    ) -> Self {
        Self {
            body_map,
            type_map,
            sir,
            lir,
            source,
            body: lir::Body {
                tid: lir::Tid(0),
                locals: Vec::new(),
                block: lir::Block::new(),
            },
            local_map: HashMap::new(),
        }
    }

    fn build(&mut self) {}

    fn build_body(&mut self, bid: sir::Bid) -> lir::Bid {
        if let Some(id) = self.body_map.get(&bid) { return *id };

        let argc = self.sir.bodies[bid].arguments;

        let id = {
            let body = lir::Body {
                tid: {
                    // get correct function type based on sir body
                    let body = &self.sir.bodies[bid];

                    let ty = lir::Ty::Func(
                        (0..argc)
                            .rev()
                            .fold(self.lir.types.get_id(lir::Ty::Empty), |acc, i| {
                                if i == body.arguments - 1 {
                                    self.map_type(body.locals[sir::Local { 0: i }])
                                } else {
                                    let input = self.map_type(body.locals[sir::Local { 0: i }]);
                                    self.lir.types.get_id(lir::Ty::Func(input, acc))
                                }
                            }),
                        self.map_type(body.output),
                    );

                    self.lir.types.get_id(ty)
                },
                locals: Vec::new(),
                block: lir::Block::new(),
            };
            self.lir.bodies.push(body)
        };

        self.body_map.insert(bid, id);

        // build lir body
        // TODO

        // push args into locals
        for i in 0..argc {
            let sid = self.sir.bodies[bid].locals[sir::Local { 0: i }];
            let lid = self.map_type(sid);
            self.lir.bodies[id].locals.push(lid);
        }

        id
    }

    fn map_type(&mut self, sid: sir::Tid) -> lir::Tid {
        if let Some(id) = self.type_map.get(&self.sir.types[sid]) { return *id };

        let ty = self.sir.types[sid].clone();

        let lt = match ty {
            sir::Ty::Int => lir::Ty::Int,
            sir::Ty::Float => lir::Ty::Float,
            sir::Ty::Str => lir::Ty::Str,
            sir::Ty::None => lir::Ty::Named(0xffffffff, self.lir.types.get_id(lir::Ty::Empty)),
            sir::Ty::True => lir::Ty::True,
            sir::Ty::False => lir::Ty::False,
            sir::Ty::Never => lir::Ty::Never,
            sir::Ty::Ref(tid) => lir::Ty::Ref(self.map_type(tid)),
            sir::Ty::List(tid) => lir::Ty::List(self.map_type(tid)),
            sir::Ty::Func(tid, tid1) => lir::Ty::Func(self.map_type(tid), self.map_type(tid1)),
            sir::Ty::Tuple(ref tids) => lir::Ty::Record(tids.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_type(*tid));
                    vec
                })),
            sir::Ty::Record(ref items) => lir::Ty::Record(items.iter().fold(Vec::new(), |mut vec, (_, tid)| {
                    vec.push(self.map_type(*tid));
                    vec
                })),
            sir::Ty::Union(ref btree_set) => lir::Ty::Union(btree_set.iter().fold(Vec::new(), |mut vec, tid| {
                    vec.push(self.map_type(*tid));
                    vec
                })),
        };

        let id = self.lir.types.get_id(lt);
        self.type_map.insert(ty, id);

        id
    }
}
