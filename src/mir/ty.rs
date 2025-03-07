use std::{collections::BTreeSet, ops::Index};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    /// Represents an integer type.
    Int,

    /// Represents a floating point type.
    Float,

    /// Represents a string type.
    Str,

    /// Represents a true type.
    True,

    /// Represents a false type.
    False,

    /// Represents a none type.
    None,

    /// Represents a never type.
    Never,

    /// Represents a generic type.
    Generic(u16),

    /// Represents a named type.
    Named(u32, Vec<Self>),

    /// Represents a reference type.
    Ref(Box<Self>),

    /// Represents a list type.
    List(Box<Self>),

    /// Represents a function type.
    Func(Box<Self>, Box<Self>),

    /// Represents a tuple type.
    Tuple(Vec<Self>),

    /// Represents a record type.
    Record(Vec<(&'static str, Self)>),

    /// Represents a union type.
    Union(BTreeSet<Self>),
}

impl Ty {
    pub fn specialize(self, generics: &[Self]) -> Self {
        match self {
            Ty::Int | Ty::Float | Ty::Str | Ty::True | Ty::False | Ty::None | Ty::Never => self,

            Ty::Generic(i) => generics[i as usize].clone(),

            Ty::Named(id, mut tys) => {
                for ty in tys.iter_mut() {
                    *ty = ty.clone().specialize(generics);
                }

                Ty::Named(id, tys)
            }

            Ty::Ref(mut ty) => {
                *ty = ty.specialize(generics);
                Ty::Ref(ty)
            }

            Ty::List(mut ty) => {
                *ty = ty.specialize(generics);
                Ty::List(ty)
            }

            Ty::Func(mut i, mut o) => {
                *i = i.specialize(generics);
                *o = o.specialize(generics);

                Ty::Func(i, o)
            }

            Ty::Tuple(mut tys) => {
                for ty in tys.iter_mut() {
                    *ty = ty.clone().specialize(generics);
                }

                Ty::Tuple(tys)
            }

            Ty::Record(mut fields) => {
                for (_, ty) in fields.iter_mut() {
                    *ty = ty.clone().specialize(generics);
                }

                Ty::Record(fields)
            }

            Ty::Union(tys) => {
                let mut new_tys = BTreeSet::new();

                for variant in tys.into_iter() {
                    new_tys.insert(variant.specialize(generics));
                }

                Ty::Union(new_tys)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Named {
    pub name: String,
    pub generics: u16,
    pub ty: Ty,
}

#[derive(Clone, Debug, Default)]
pub struct Types {
    named: Vec<Named>,
}

impl Types {
    pub fn new() -> Self {
        Self { named: Vec::new() }
    }

    pub fn push(&mut self, named: Named) -> u32 {
        let id = self.named.len() as u32;
        self.named.push(named);
        id
    }

    pub fn format(&self, ty: &Ty) -> String {
        match ty {
            Ty::Int => String::from("int"),
            Ty::Float => String::from("float"),
            Ty::Str => String::from("str"),
            Ty::True => String::from("true"),
            Ty::False => String::from("false"),
            Ty::None => String::from("none"),
            Ty::Never => String::from("!"),

            Ty::Generic(idx) => format!("'{}", idx),

            Ty::Named(idx, generics) => {
                let generics = generics
                    .iter()
                    .map(|ty| self.format(ty))
                    .collect::<Vec<_>>();

                let name = &self.named[*idx as usize].name;

                if generics.is_empty() {
                    String::from(name)
                } else {
                    format!("{}<{}>", name, generics.join(", "))
                }
            }

            Ty::Ref(ty) => format!("ref {}", self.format(ty)),

            Ty::List(ty) => format!("[{}]", self.format(ty)),

            Ty::Func(input, output) => format!("{} -> {}", self.format(input), self.format(output)),

            Ty::Tuple(tys) => {
                let tys: Vec<_> = tys.iter().map(|ty| self.format(ty)).collect();
                format!("({})", tys.join(", "))
            }

            Ty::Record(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.format(ty)))
                    .collect();

                format!("{{ {} }}", fields.join("; "))
            }

            Ty::Union(tys) => {
                let tys: Vec<_> = tys.iter().map(|ty| self.format(ty)).collect();
                format!("({})", tys.join(" | "))
            }
        }
    }
}

impl Index<u32> for Types {
    type Output = Named;

    fn index(&self, id: u32) -> &Self::Output {
        &self.named[id as usize]
    }
}
