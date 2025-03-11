use crate::diagnostic::Span;
use std::borrow::Cow;

#[derive(Clone, Debug, Default)]
pub struct Attributes {
    pub attributes: Vec<Attribute>,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub name: Cow<'static, str>,
    pub value: Option<Cow<'static, str>>,
    pub span: Option<Span>,
}

impl Attributes {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    pub fn with(mut self, attr: impl Into<Attribute>) -> Self {
        self.attributes.push(attr.into());
        self
    }

    pub fn find_attr(&self, name: &str) -> Option<&Attribute> {
        self.attributes.iter().find(|attr| attr.name == name)
    }

    pub fn find_value(&self, name: &str) -> Option<&str> {
        self.find_attr(name)?.value.as_deref()
    }
}

impl From<(&'static str, &'static str)> for Attribute {
    fn from((name, value): (&'static str, &'static str)) -> Self {
        Self {
            name: Cow::Borrowed(name),
            value: Some(Cow::Borrowed(value)),
            span: None,
        }
    }
}
