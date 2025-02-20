mod source;
mod span;

pub use source::{Source, SourceId, Sources};
pub use span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub message: Option<String>,
    pub labels: Vec<Label>,
}

#[derive(Debug)]
pub struct Label {
    pub message: Option<String>,
    pub span: Span,
}

impl Diagnostic {
    pub fn new(severity: Severity, code: impl Into<String>) -> Diagnostic {
        Diagnostic {
            severity,
            code: code.into(),
            message: None,
            labels: Vec::new(),
        }
    }

    pub fn error(code: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Error, code)
    }

    pub fn warn(code: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Warning, code)
    }

    pub fn note(code: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Note, code)
    }

    pub fn help(code: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Help, code)
    }

    pub fn message(mut self, message: impl Into<String>) -> Diagnostic {
        self.message = Some(message.into());
        self
    }

    pub fn label(mut self, span: Span, message: impl Into<String>) -> Diagnostic {
        self.labels.push(Label {
            message: Some(message.into()),
            span,
        });

        self
    }

    pub fn span(mut self, span: Span) -> Diagnostic {
        self.labels.push(Label {
            message: None,
            span,
        });

        self
    }
}
