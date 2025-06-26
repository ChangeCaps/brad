use solve::{Tag, Tags};
use std::collections::HashMap;
use std::{
    mem,
    ops::{Index, IndexMut},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tid(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Type {
    /// Terms of a type (union)
    pub terms: Vec<Term>,
}

impl Type {
    pub fn any() -> Self {
        Self {
            terms: vec![Term {
                tags: Tags::default(),
                base: Base::Any,
            }],
        }
    }

    pub fn int() -> Self {
        Self::any().with_tag(Tag::INT)
    }

    pub fn none() -> Self {
        Self::any().with_tag(Tag::NONE)
    }

    pub fn function(input: Tid, output: Tid) -> Self {
        Self {
            terms: vec![Term {
                tags: Tags::default(),
                base: Base::Function(input, output),
            }],
        }
    }

    pub fn with_tag(mut self, tag: Tag) -> Self {
        for term in &mut self.terms {
            term.tags.insert(tag);
        }

        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Term {
    pub tags: Tags,
    pub base: Base,
}

impl Term {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Base {
    Any,
    Record(Vec<(&'static str, Tid)>),
    Tuple(Vec<Tid>),
    Array(Tid),
    Function(Tid, Tid),
}

#[derive(Clone, Debug, Default)]
pub struct Types {
    types: Vec<Type>,
    map: HashMap<Type, Tid>,
}

impl Types {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, ty: Type) -> Tid {
        if let Some(&tid) = self.map.get(&ty) {
            return tid;
        }

        let tid = Tid(self.types.len());
        self.types.push(ty.clone());
        self.map.insert(ty, tid);
        tid
    }

    pub fn iter(&self) -> impl Iterator<Item = (Tid, &Type)> {
        self.types.iter().enumerate().map(|(id, ty)| (Tid(id), ty))
    }

    pub fn union_tids(&mut self, tid: Tid, other: Tid) -> Tid {
        let mut ty = self[tid].clone();
        let other = self[other].clone();

        ty.terms.extend(other.terms);

        self.insert(ty)
    }

    pub fn intersect_tids(&mut self, tid: Tid, other: Tid) -> Tid {
        let mut ty = self[tid].clone();
        let other = self[other].clone();

        self.intersect(&mut ty, &other);
        self.insert(ty)
    }

    pub fn intersect(&mut self, ty: &mut Type, other: &Type) {
        for term in mem::take(&mut ty.terms) {
            for other_term in &other.terms {
                let mut new_term = term.clone();
                self.intersect_terms(&mut new_term, other_term);
                ty.terms.push(new_term);
            }
        }
    }

    fn intersect_terms(&mut self, term: &mut Term, other: &Term) {
        term.tags.union(&other.tags);

        match (term.base.clone(), other.base.clone()) {
            (Base::Any, other) | (other, Base::Any) => term.base = other,

            (Base::Record(ref mut fields), Base::Record(other)) => {
                for (name, tid) in other {
                    if let Some(&(_, existing)) = fields.iter().find(|(n, _)| *n == name) {
                        fields.push((name, self.intersect_tids(existing, tid)));
                    } else {
                        fields.push((name, tid));
                    }
                }
            }

            (Base::Tuple(ref mut types), Base::Tuple(other)) => {
                assert_eq!(types.len(), other.len(), "tuple lengths must match");

                for (this, other) in types.iter_mut().zip(other) {
                    *this = self.intersect_tids(*this, other);
                }
            }

            (Base::Array(ref mut ty), Base::Array(other)) => {
                *ty = self.intersect_tids(*ty, other);
            }

            (
                Base::Function(ref mut input, ref mut output),
                Base::Function(other_input, other_output),
            ) => {
                *input = self.union_tids(*input, other_input);
                *output = self.union_tids(*output, other_output);
            }

            (_, _) => unreachable!("this should not happen... congratulations!"),
        }
    }

    pub fn format_tid(&self, tid: Tid) -> String {
        self.format_type(self[tid].clone())
    }

    pub fn format_type(&self, ty: Type) -> String {
        if ty.terms.is_empty() {
            return "!".to_string();
        }

        let mut result = String::new();

        for term in &ty.terms {
            if !result.is_empty() {
                result.push_str(" | ");
            }

            result.push_str(&self.format_term(term));
        }

        result
    }

    pub fn format_term(&self, term: &Term) -> String {
        let mut result = String::new();

        if !term.tags.is_empty() {
            for tag in term.tags.iter() {
                result.push_str(&format!("{} & ", tag));
            }
        }

        match &term.base {
            Base::Any => result.push_str("any"),

            Base::Record(fields) => {
                result.push_str("{ ");

                for (name, tid) in fields {
                    result.push_str(&format!("{}: {}, ", name, self.format_tid(*tid)));
                }

                result.push('}');
            }

            Base::Tuple(types) => {
                result.push('(');

                for tid in types {
                    result.push_str(&format!("{}, ", self.format_tid(*tid)));
                }

                result.push(')');
            }

            Base::Array(ty) => {
                result.push_str(&format!("[{}]", self.format_tid(*ty)));
            }

            Base::Function(input, output) => {
                result.push_str(&format!(
                    "({}) -> {}",
                    self.format_tid(*input),
                    self.format_tid(*output)
                ));
            }
        }

        result
    }
}

impl Index<Tid> for Types {
    type Output = Type;

    fn index(&self, Tid(id): Tid) -> &Self::Output {
        &self.types[id]
    }
}

impl IndexMut<Tid> for Types {
    fn index_mut(&mut self, Tid(id): Tid) -> &mut Self::Output {
        &mut self.types[id]
    }
}
