use std::{collections::HashMap, hash::Hash, ops::Index};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Tid(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    /// integer type
    Int,
    /// float type
    Float,
    /// string type
    Str,
    /// false bool + named
    False,
    /// true bool + named
    True,
    /// boolean type (union of false | true)
    Bool,
    /// zero size type
    Empty,
    /// never type
    Never,
    /// compiler generated pointer type
    Ptr(Tid),
    /// reference to a type
    Ref(Tid),
    /// list of a type
    List(Tid),
    /// function of a type to a type
    Func(Tid, Tid),
    /// tuple of types
    Tuple(Vec<Tid>),
    /// record of types
    Record(Vec<(&'static str, Tid)>),
    /// union of types
    Union(Vec<Tid>),
    /// unique rename of existing type
    Named(u32, Tid),
}

#[derive(Clone, Debug)]
pub struct Types {
    type_vec: Vec<Ty>,
    type_map: HashMap<Ty, Tid>,
}

impl Types {
    pub fn new() -> Self {
        Self {
            type_vec: Vec::new(),
            type_map: HashMap::new(),
        }
    }

    pub fn push(&mut self, ty: Ty) -> Tid {
        let id: u32 = self.type_map.len().try_into().unwrap();
        self.type_vec.push(ty);
        Tid(id)
    }

    pub fn get_id(&mut self, ty: Ty) -> Tid {
        match self.type_map.get(&ty) {
            Some(id) => *id,
            None => {
                let id: u32 = self.type_vec.len().try_into().unwrap();
                self.type_vec.push(ty.clone());
                self.type_map.insert(ty, Tid(id));

                Tid(id)
            }
        }
    }
}

impl Index<Tid> for Types {
    type Output = Ty;

    fn index(&self, Tid(i): Tid) -> &Self::Output {
        &self.type_vec[i as usize]
    }
}
