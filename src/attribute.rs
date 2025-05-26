use crate::ast::Spanned;
use std::borrow::Cow;

use diagnostic::Span;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Attributes {
    pub attributes: Vec<Attribute>,
}

impl Spanned for Attributes {
    fn span(&self) -> Span {
        self.attributes
            .iter()
            .fold(Span::default(), |acc, attr| acc.join(attr.span()))
    }

    fn reset_spans(&mut self) {
        self.attributes.iter_mut().for_each(Spanned::reset_spans);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub name: Cow<'static, str>,
    pub value: Option<Cow<'static, str>>,
    pub span: Option<Span>,
}

impl Spanned for Attribute {
    fn span(&self) -> Span {
        self.span.unwrap_or_default()
    }

    fn reset_spans(&mut self) {
        self.span = Some(Span::default());
    }
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
