use std::{cell::RefCell, collections::BTreeSet, rc::Rc};

use super::{
    annotation::VarAnnotation,
    body::{Body, Local},
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

    pub fn next_name(&mut self, tid: Tid) -> Local {
        let id = self.body.locals.len();
        self.body.locals.push(tid);
        Local(id as u32)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    name: Local,
    place: Option<Rc<RefCell<Place>>>,
    annotations: BTreeSet<VarAnnotation>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Place {
    name: Option<Local>,
    tid: Tid,
    shared: bool,
    access: Access,
    parent: Option<Rc<RefCell<Place>>>,
}

impl Place {
    pub fn get_name(&mut self) -> Local {
        let rename = if self.name.is_none() {
            true
        } else if let Some(parent) = &mut self.parent {
            parent.borrow().name.is_none()
        } else {
            false
        };

        let name = if rename {
            // do rename (read into local and give local)
            todo!()
        } else {
            Local(0)
        };

        if self.shared {
            self.name = None;
        } else {
            self.name = Some(name.clone());
        }

        name
    }

    pub fn rename(&mut self) {
        self.name = None;
    }

    pub fn access(self_rc: Rc<RefCell<Place>>, access: Access) -> Rc<RefCell<Place>> {
        let self_br = self_rc.borrow();
        Rc::new(RefCell::new(Place {
            name: None,
            tid: match self_br.access {
                Access::Field(tid, _)
                | Access::Element(tid, _)
                | Access::Index(tid, _)
                | Access::Deref(tid) => tid.clone(),
            },
            shared: self_br.shared,
            access,
            parent: Some(self_rc.clone()),
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Access {
    /// record field
    Field(Tid, &'static str),
    /// tuple index
    Element(Tid, u32),
    /// array indexing
    Index(Tid, Var),
    /// ref dereference
    Deref(Tid),
}

impl Access {}
