use std::borrow::Cow;

use crate::diagnostic::Span;

#[derive(Clone, Debug, Default)]
pub struct Attributes {
    pub attributes: Vec<Attribute>,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub name: Cow<'static, str>,
    pub value: Option<Cow<'static, str>>,
    pub span: Span,
}

impl Attributes {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    pub fn find_attr(&self, name: &str) -> Option<&Attribute> {
        self.attributes.iter().find(|attr| attr.name == name)
    }

    pub fn find_value(&self, name: &str) -> Option<&str> {
        self.find_attr(name)?.value.as_deref()
    }
}
