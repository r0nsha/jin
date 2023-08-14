use codespan_reporting::{
    diagnostic as codespan_diagnostic,
    term::termcolor::{ColorChoice, StandardStream},
};

use crate::span::{SourceId, Sources, Span};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    severity: Severity,
    code: String,
    message: Option<String>,
    labels: Vec<Label>,
    help: Option<String>,
}

impl Diagnostic {
    pub fn error(code: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            code: code.into(),
            message: None,
            labels: vec![],
            help: None,
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

#[derive(Debug, Clone)]
pub enum Severity {
    Error,
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
            code: Some(val.code),
            message: val.message.unwrap_or_default(),
            labels: val.labels.into_iter().map(Into::into).collect(),
            notes: val.help.map_or(vec![], |help| vec![help]),
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
        }
    }
}

#[derive(Debug)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Default for Diagnostics {
    fn default() -> Self {
        Self::new()
    }
}

impl Diagnostics {
    pub fn new() -> Self {
        Self { diagnostics: vec![] }
    }

    pub fn add(&mut self, item: impl Into<Diagnostic>) {
        self.diagnostics.push(item.into());
    }

    pub fn extend<T: Into<Diagnostic>>(
        &mut self,
        items: impl IntoIterator<Item = T>,
    ) {
        self.diagnostics.extend(items.into_iter().map(Into::into));
    }

    pub fn any(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn print(
        &self,
        sources: &Sources,
    ) -> Result<(), codespan_reporting::files::Error> {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let mut writer_lock = writer.lock();

        for diagnostic in &self.diagnostics {
            codespan_reporting::term::emit(
                &mut writer_lock,
                &config,
                sources,
                &diagnostic.clone().into(),
            )?;
        }

        Ok(())
    }
}
