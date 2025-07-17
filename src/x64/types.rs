use std::{collections::BTreeSet, ops::Index};

use crate::anf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeType {
    I8,
    I16,
    I32,
    I64,

    F8,
    F16,
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpType {
    Bool,
    Int,
    Float,
    String,

    Function,

    Array,
    Union,
    Tuple,
    Record,

    Tag,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tid(pub u32);

impl Tid {
    pub fn usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    True,
    False,
    Bool,  // 1 byte
    Int,   // 8 bytes
    Float, // 8 bytes

    // 8 bytes (ptr)
    String,

    // 8 bytes (ptr)
    Function { arg: Tid, ret: Tid },

    // 8 bytes (ptr)
    Array { ty: Tid },
    // real shit (i die)
    Union { types: BTreeSet<Tid> },
    // 8 bytes (ptr)
    Tuple { types: Vec<Tid> },
    // 8 bytes (ptr)
    Record { fields: Vec<(&'static str, Tid)> },

    // varies based on type
    Tagged { ty: Tid, tags: solve::Tags },

    // 2 bytes
    None,
}

#[derive(Debug, Clone)]
pub struct Types {
    types: Vec<Type>,
}

impl Types {
    pub fn new() -> Self {
        Self { types: Vec::new() }
    }
}

impl Index<Tid> for Types {
    type Output = Type;

    fn index(&self, tid: Tid) -> &Self::Output {
        &self.types[tid.usize()]
    }
}

impl OpType {
    pub fn from_type(ty: &Type, types: &Types) -> Self {
        match ty {
            Type::Bool => Self::Bool,
            Type::Int => Self::Int,
            Type::Float => Self::Float,
            Type::String => Self::String,
            Type::Function { arg: _, ret: _ } => Self::Function,
            Type::Array { ty: _ } => Self::Array,
            Type::Union { types: utypes } => {
                let mut iter = utypes.iter();
                if let Some(tid) = iter.next() {
                    let base = Self::from_type(&types[tid.clone()], types);
                    loop {
                        if let Some(tid) = iter.next() {
                            if base != Self::from_type(&types[tid.clone()], types) {
                                break Self::Union;
                            }
                        } else {
                            break base;
                        }
                    }
                } else {
                    panic!("Empty union")
                }
            }
            Type::Tuple { types: _ } => Self::Tuple,
            Type::Record { fields: _ } => Self::Record,
            Type::Tagged { ty, tags: _ } => Self::from_type(&types[ty.clone()], types),
            Type::None => Self::Tag,
            Type::True => Self::Tag,
            Type::False => Self::Tag,
        }
    }

    pub fn from_value(val: &anf::Value, body: &anf::Body, types: &anf::Types) -> Self {
        match val {
            anf::Value::Local(local) => Self::from_local(local, body, types),
            anf::Value::Int(_) => Self::Int,
            anf::Value::Float(_) => Self::Float,
            anf::Value::String(_) => Self::String,
        }
    }

    pub fn from_local(local: &anf::Local, body: &anf::Body, types: &anf::Types) -> Self {
        let tid = body.locals[local.0];
        Self::from_tid(tid, types)
    }

    fn from_tid(tid: anf::Tid, types: &anf::Types) -> Self {
        let terms = &types[tid].terms;

        if terms.len() == 2 {
            if (terms[0].tags.contains(solve::Tag::TRUE)
                && terms[1].tags.contains(solve::Tag::FALSE))
                || (terms[0].tags.contains(solve::Tag::FALSE)
                    && terms[1].tags.contains(solve::Tag::TRUE))
            {
                return Self::Bool;
            }
        };

        if terms[0].tags.contains(solve::Tag::INT) {
            let mut iter = terms.iter();
            if loop {
                if let Some(term) = iter.next() {
                    if term.tags.contains(solve::Tag::INT) {
                        continue;
                    } else {
                        break false;
                    }
                } else {
                    break true;
                }
            } {
                return Self::Int;
            }
        }
        if terms[0].tags.contains(solve::Tag::FLOAT) {
            let mut iter = terms.iter();
            if loop {
                if let Some(term) = iter.next() {
                    if term.tags.contains(solve::Tag::FLOAT) {
                        continue;
                    } else {
                        break false;
                    }
                } else {
                    break true;
                }
            } {
                return Self::Float;
            }
        }
        if terms[0].tags.contains(solve::Tag::STR) {
            let mut iter = terms.iter();
            if loop {
                if let Some(term) = iter.next() {
                    if term.tags.contains(solve::Tag::STR) {
                        continue;
                    } else {
                        break false;
                    }
                } else {
                    break true;
                }
            } {
                return Self::String;
            }
        }
        if terms[0].tags.contains(solve::Tag::NONE) {
            let mut iter = terms.iter();
            if loop {
                if let Some(term) = iter.next() {
                    if term.tags.contains(solve::Tag::NONE) {
                        continue;
                    } else {
                        break false;
                    }
                } else {
                    break true;
                }
            } {
                return Self::Tag;
            }
        }

        if let anf::Base::Array(tid) = &terms[0].base {
            let mut iter = terms.iter();
            _ = iter.next();
            let base = Self::from_tid(tid.clone(), types);
            loop {
                if let Some(term) = iter.next() {
                    if let anf::Base::Array(tid) = term.base {
                        if base != Self::from_tid(tid, types) {
                            return Self::Union;
                        }
                    } else {
                        return Self::Union;
                    }
                } else {
                    return Self::Array;
                }
            }
        };

        if let anf::Base::Tuple(tids) = &terms[0].base {
            let mut iter = terms.iter();
            _ = iter.next();
            let mut base = Vec::new();
            for tid in tids {
                base.push(Self::from_tid(tid.clone(), types));
            }
            loop {
                if let Some(term) = iter.next() {
                    if let anf::Base::Tuple(tids) = &term.base {
                        let mut cmp = Vec::new();
                        for tid in tids {
                            cmp.push(Self::from_tid(tid.clone(), types));
                        }
                        if base != cmp {
                            return Self::Union;
                        }
                    } else {
                        return Self::Union;
                    }
                } else {
                    return Self::Tuple;
                }
            }
        };

        if let anf::Base::Record(_) = &terms[0].base {
            if terms.len() > 1 {
                return Self::Union;
            } else {
                return Self::Record;
            }
        };

        if let anf::Base::Function(input, output) = &terms[0].base {
            let mut iter = terms.iter();
            _ = iter.next();
            let base_i = Self::from_tid(input.clone(), types);
            let base_o = Self::from_tid(output.clone(), types);
            loop {
                if let Some(term) = iter.next() {
                    if let anf::Base::Function(input, output) = term.base {
                        if base_i != Self::from_tid(input, types) {
                            return Self::Union;
                        }
                        if base_o != Self::from_tid(output, types) {
                            return Self::Union;
                        }
                    } else {
                        return Self::Union;
                    }
                } else {
                    return Self::Function;
                }
            }
        };

        panic!()
    }
}
