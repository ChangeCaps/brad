mod formatter;
mod source;
mod span;

use std::{
    fmt,
    ops::{Deref, DerefMut},
};

pub use formatter::Formatter;
pub use source::{Source, SourceId, Sources};
pub use span::Span;

#[derive(Debug)]
pub struct Report {
    diagnostics: Vec<Diagnostic>,
}

impl Deref for Report {
    type Target = Vec<Diagnostic>;

    fn deref(&self) -> &Self::Target {
        &self.diagnostics
    }
}

impl DerefMut for Report {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.diagnostics
    }
}

impl From<Diagnostic> for Report {
    fn from(diagnostic: Diagnostic) -> Report {
        let mut report = Report::new();
        report.push(diagnostic);
        report
    }
}

impl From<Vec<Diagnostic>> for Report {
    fn from(diagnostics: Vec<Diagnostic>) -> Report {
        Report { diagnostics }
    }
}

impl Report {
    pub fn new() -> Report {
        Report {
            diagnostics: Vec::new(),
        }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }
}

impl IntoIterator for Report {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.into_iter()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Help,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
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
    pub notes: Vec<String>,
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
            notes: Vec::new(),
        }
    }

    pub fn error(code: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Error).code(code)
    }

    pub fn warn() -> Diagnostic {
        Diagnostic::new(Severity::Warning)
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

    pub fn note(mut self, note: impl Into<String>) -> Diagnostic {
        self.notes.push(note.into());
        self
    }
}
