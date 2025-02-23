mod formatter;
mod source;
mod span;

use std::fmt;

pub use formatter::Formatter;
pub use source::{Source, SourceId, Sources};
pub use span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Note => write!(f, "note"),
            Severity::Help => write!(f, "help"),
        }
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: Option<String>,
    pub labels: Vec<Label>,
}

#[derive(Debug)]
pub struct Label {
    pub message: Option<String>,
    pub span: Span,
}

impl Diagnostic {
    pub fn new(severity: Severity) -> Diagnostic {
        Diagnostic {
            severity,
            code: None,
            message: None,
            labels: Vec::new(),
        }
    }

    pub fn error(code: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Error).code(code)
    }

    pub fn warn() -> Diagnostic {
        Diagnostic::new(Severity::Warning)
    }

    pub fn note() -> Diagnostic {
        Diagnostic::new(Severity::Note)
    }

    pub fn help() -> Diagnostic {
        Diagnostic::new(Severity::Help)
    }

    pub fn code(mut self, code: impl Into<String>) -> Diagnostic {
        self.code = Some(code.into());
        self
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
