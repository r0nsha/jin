use codespan_reporting::{
    diagnostic as codespan_diagnostic,
    term::termcolor::{ColorChoice, StandardStream},
};

use crate::span::{SourceId, Sources, Span};

pub type DiagnosticResult<T> = Result<T, Diagnostic>;

#[must_use]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    severity: Severity,
    message: String,
    labels: Vec<Label>,
    notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self { severity, message: message.into(), labels: vec![], notes: vec![] }
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn push_label(&mut self, label: Label) {
        self.labels.push(label);
    }

    pub fn push_labels(&mut self, labels: impl IntoIterator<Item = Label>) {
        self.labels.extend(labels);
    }

    pub fn with_labels(mut self, labels: impl IntoIterator<Item = Label>) -> Self {
        self.labels.extend(labels);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_notes(mut self, notes: impl IntoIterator<Item = String>) -> Self {
        self.notes.extend(notes);
        self
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Label {
    style: LabelStyle,
    message: String,
    span: Span,
}

impl Label {
    pub fn primary(span: Span, message: impl Into<String>) -> Self {
        Self { style: LabelStyle::Primary, message: message.into(), span }
    }

    pub fn secondary(span: Span, message: impl Into<String>) -> Self {
        Self { style: LabelStyle::Secondary, message: message.into(), span }
    }
}

#[derive(Debug, Clone)]
enum LabelStyle {
    Primary,
    Secondary,
}

impl From<Diagnostic> for codespan_diagnostic::Diagnostic<SourceId> {
    fn from(val: Diagnostic) -> Self {
        Self {
            severity: val.severity.into(),
            code: None,
            message: val.message,
            labels: val.labels.into_iter().map(Into::into).collect(),
            notes: val.notes,
        }
    }
}

impl From<Label> for codespan_diagnostic::Label<SourceId> {
    fn from(val: Label) -> Self {
        Self {
            style: match val.style {
                LabelStyle::Primary => codespan_diagnostic::LabelStyle::Primary,
                LabelStyle::Secondary => codespan_diagnostic::LabelStyle::Secondary,
            },
            file_id: val.span.source_id(),
            range: val.span.start() as usize..val.span.end() as usize,
            message: val.message,
        }
    }
}

impl From<Severity> for codespan_diagnostic::Severity {
    fn from(val: Severity) -> Self {
        match val {
            Severity::Error => Self::Error,
            Severity::Warning => Self::Warning,
        }
    }
}

#[derive(Debug)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
    config: codespan_reporting::term::Config,
    had_errors: bool,
}

impl Diagnostics {
    pub fn new() -> Self {
        let config = codespan_reporting::term::Config::default();
        Self { diagnostics: vec![], config, had_errors: false }
    }

    pub fn add(&mut self, diagnostic: impl Into<Diagnostic>) {
        let diagnostic = diagnostic.into();

        if let Severity::Error = diagnostic.severity() {
            self.had_errors = true;
        }

        self.diagnostics.push(diagnostic);
    }

    pub fn print(self, sources: &Sources) {
        let w = StandardStream::stderr(ColorChoice::Always);
        let mut w = w.lock();

        for diagnostic in self.diagnostics {
            codespan_reporting::term::emit(&mut w, &self.config, sources, &diagnostic.into())
                .expect("failed emitting diagnostic");
        }
    }

    pub fn any_errors(&self) -> bool {
        self.had_errors
    }
}

impl Default for Diagnostics {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Into<Diagnostic>> Extend<T> for Diagnostics {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for d in iter {
            self.add(d);
        }
    }
}
