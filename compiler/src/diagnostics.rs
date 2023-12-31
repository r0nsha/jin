use std::{cell::RefCell, rc::Rc};

use codespan_reporting::{
    diagnostic as codespan_diagnostic,
    term::termcolor::{ColorChoice, StandardStream, StandardStreamLock},
};

use crate::span::{SourceId, Sources, Span};

pub type DiagnosticResult<T> = Result<T, Diagnostic>;

#[must_use]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    severity: Severity,
    message: Option<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
}

impl Diagnostic {
    pub fn error() -> Self {
        Self::new(Severity::Error)
    }

    pub fn warning() -> Self {
        Self::new(Severity::Warning)
    }

    fn new(severity: Severity) -> Self {
        Self { severity, message: None, labels: vec![], notes: vec![] }
    }

    pub fn set_message(&mut self, message: impl Into<String>) {
        self.message = Some(message.into());
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.set_message(message);
        self
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

    pub fn with_labels(
        mut self,
        labels: impl IntoIterator<Item = Label>,
    ) -> Self {
        self.labels.extend(labels);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_notes(
        mut self,
        notes: impl IntoIterator<Item = String>,
    ) -> Self {
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
    message: Option<String>,
    span: Span,
}

impl Label {
    pub fn primary(span: Span) -> Self {
        Self { style: LabelStyle::Primary, message: None, span }
    }

    pub fn secondary(span: Span) -> Self {
        Self { style: LabelStyle::Secondary, message: None, span }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
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
            message: val.message.unwrap_or_default(),
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
                LabelStyle::Secondary => {
                    codespan_diagnostic::LabelStyle::Secondary
                }
            },
            file_id: val.span.source_id(),
            range: val.span.start() as usize..val.span.end() as usize,
            message: val.message.unwrap_or_default(),
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
    sources: Rc<RefCell<Sources>>,
    config: codespan_reporting::term::Config,
    had_errors: bool,
}

impl Diagnostics {
    pub fn new(sources: Rc<RefCell<Sources>>) -> Self {
        let config = codespan_reporting::term::Config {
            chars: codespan_reporting::term::Chars::ascii(),
            ..Default::default()
        };
        Self { sources, config, had_errors: false }
    }

    pub fn emit(&mut self, diagnostic: impl Into<Diagnostic>) {
        self.emit_(&mut Self::writer().lock(), diagnostic.into());
    }

    pub fn emit_many<T, I>(&mut self, diagnostics: I)
    where
        T: Into<Diagnostic>,
        I: IntoIterator<Item = T>,
    {
        let w = Self::writer();
        let mut w = w.lock();

        for diagnostic in diagnostics {
            self.emit_(&mut w, diagnostic.into());
        }
    }

    pub fn any(&self) -> bool {
        self.had_errors
    }

    fn emit_(&mut self, w: &mut StandardStreamLock, diagnostic: Diagnostic) {
        if let Severity::Error = diagnostic.severity() {
            self.had_errors = true;
        }

        let sources: &Sources = &self.sources.borrow();

        codespan_reporting::term::emit(
            w,
            &self.config,
            sources,
            &diagnostic.into(),
        )
        .expect("failed emitting diagnostic");
    }

    fn writer() -> StandardStream {
        StandardStream::stderr(ColorChoice::Always)
    }
}
