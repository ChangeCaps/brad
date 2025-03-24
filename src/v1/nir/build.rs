use std::{cell::RefCell, rc::Rc};

use super::{
    body::{Body, Name},
    ty::Tid,
};

#[derive(Debug)]
pub struct BodyBuilder {
    body: Body,
}

impl BodyBuilder {
    pub fn new() -> Self {
        Self { body: Body::new() }
    }

    pub fn next_name(&mut self, tid: Tid) -> Name {
        let id = self.body.locals.len();
        self.body.locals.push(tid);
        Name(id as u32)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    ethereal: bool,
    name: Name,
    place: Option<Rc<RefCell<Place>>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Place {
    name: Name,
    rename: bool,
    shared: bool,
    access: Access,
    parent: Option<Rc<RefCell<Place>>>,
}

impl Place {
    pub fn get_name(&mut self) -> Name {
        let rename = if self.rename {
            true
        } else if let Some(parent) = &mut self.parent {
            parent.borrow().rename
        } else {
            false
        };

        if rename {
            // do rename (read into local and give local)
            todo!()
        }

        self.rename = self.shared;

        self.name.clone()
    }

    pub fn rename(&mut self) {
        self.rename = true;
    }

    pub fn access(self_rc: Rc<RefCell<Place>>, access: Access) -> Rc<RefCell<Place>> {
        Rc::new(RefCell::new(Self {
            name: Name(0),
            rename: true,
            shared: self_rc.borrow().shared,
            access,
            parent: Some(self_rc.clone()),
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Access {
    /// record field
    Field { name: &'static str },
    /// tuple index
    Element { index: u32 },
    /// array indexing
    Index { index: Var },
    /// ref dereferenc
    Deref {},
}

impl Access {}
