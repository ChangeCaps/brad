use std::{
    collections::{BTreeSet, HashMap},
    hash::Hash,
    ops::Index,
};

use solve::Tag;

use crate::anf;

use super::types::{Tid, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bid(pub u32);

impl Bid {
    pub fn usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub enum Body {
    Extern(&'static str),
    Local { ty: Tid, text: Option<u64> },
}

#[derive(Debug, Clone)]
pub struct ProgramBuilder {
    pub types: Vec<Type>,
    pub type_map: HashMap<Type, Tid>,
    pub tid_map: HashMap<anf::Tid, Tid>,

    pub bodies: Vec<Body>,
}

impl ProgramBuilder {
    pub fn new(program: &anf::Program) -> Self {
        let mut ret = Self {
            types: Vec::new(),
            type_map: HashMap::new(),
            tid_map: HashMap::new(),
            bodies: Vec::new(),
        };

        for (a_tid, _) in program.types.iter() {
            ret.build_type_from_anf(program, &a_tid);
        }

        ret
    }
}

impl Index<Bid> for ProgramBuilder {
    type Output = Body;

    fn index(&self, index: Bid) -> &Self::Output {
        &self.bodies[index.usize()]
    }
}

impl Index<Tid> for ProgramBuilder {
    type Output = Type;

    fn index(&self, index: Tid) -> &Self::Output {
        &self.types[index.usize()]
    }
}

impl ProgramBuilder {
    pub fn tid_from_tid(&self, a_tid: &anf::Tid) -> Tid {
        todo!()
    }

    pub fn tid_from_type(&self, a_tid: &anf::Tid, a_ty: &anf::Type) -> Tid {
        todo!()
    }

    fn build_type_from_term(&mut self, program: &anf::Program, term: &anf::Term) -> Tid {
        let (tmin, ty) = match &term.base {
            anf::Base::Any => {
                if term.tags.contains(Tag::INT) {
                    (1, Type::Int)
                } else if term.tags.contains(Tag::FLOAT) {
                    (1, Type::Float)
                } else if term.tags.contains(Tag::STR) {
                    (1, Type::String)
                } else if term.tags.contains(Tag::TRUE) {
                    (1, Type::True)
                } else if term.tags.contains(Tag::FALSE) {
                    (1, Type::False)
                } else {
                    (1, Type::None)
                }
            }
            anf::Base::Record(fields) => {
                let mut tids = Vec::new();
                for (name, f_tid) in fields {
                    tids.push((*name, self.build_type_from_anf(program, f_tid)));
                }
                (0, Type::Record { fields: tids })
            }
            anf::Base::Tuple(elem_tids) => {
                let mut tids = Vec::new();
                for elem_tid in elem_tids {
                    tids.push(self.build_type_from_anf(program, elem_tid));
                }
                (0, Type::Tuple { types: tids })
            }
            anf::Base::Array(elem_tid) => (
                0,
                Type::Array {
                    ty: self.build_type_from_anf(program, elem_tid),
                },
            ),
            anf::Base::Function(input_tid, output_tid) => {
                let arg = self.build_type_from_anf(program, input_tid);
                let ret = self.build_type_from_anf(program, output_tid);
                (0, Type::Function { arg, ret })
            }
        };

        let res = self.type_map.get(&ty);
        let tid = match res {
            Some(tid) => tid.clone(),
            None => {
                self.types.push(ty.clone());
                let tid = Tid(self.types.len() as u32);
                self.type_map.insert(ty, tid);
                tid
            }
        };

        if term.tags.len() > tmin {
            // tagged
            let ty = Type::Tagged {
                ty: tid,
                tags: term.tags.clone(),
            };
            let res = self.type_map.get(&ty);
            match res {
                Some(tid) => tid.clone(),
                None => {
                    self.types.push(ty.clone());
                    let tid = Tid(self.types.len() as u32);
                    self.type_map.insert(ty, tid);
                    tid
                }
            }
        } else {
            // not tagged
            tid
        }
    }

    fn build_type_from_anf(&mut self, program: &anf::Program, a_tid: &anf::Tid) -> Tid {
        match self.tid_map.get(&a_tid) {
            Some(tid) => tid.clone(),
            None => {
                let a_ty = &program.types[a_tid.clone()];
                let tid: Tid = if a_ty.terms.len() == 0 {
                    panic!("empty type")
                } else if a_ty.terms.len() == 1 {
                    // union
                    let mut types = BTreeSet::new();
                    for term in &a_ty.terms {
                        let tid = self.build_type_from_term(program, &term);
                        types.insert(tid);
                    }

                    let ty = Type::Union { types };
                    let res = self.type_map.get(&ty);

                    match res {
                        Some(tid) => tid.clone(),
                        None => {
                            self.types.push(ty.clone());
                            let tid = Tid(self.types.len() as u32);
                            self.type_map.insert(ty, tid);
                            tid
                        }
                    }
                } else {
                    let term = &a_ty.terms[0];
                    self.build_type_from_term(program, term)
                };

                self.tid_map.insert(a_tid.clone(), tid);

                tid
            }
        }
    }
}
