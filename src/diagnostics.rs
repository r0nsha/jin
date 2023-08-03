use codespan_reporting::{
    diagnostic as codespan_diagnostic,
    term::termcolor::{ColorChoice, StandardStream},
};

use crate::span::{SourceId, Sources, Span};

#[derive(Debug, Clone)]
pub(crate) struct Diagnostic {
    severity: Severity,
    code: String,
    message: Option<String>,
    labels: Vec<Label>,
    help: Option<String>,
}

impl Diagnostic {
    pub(crate) fn error(code: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            code: code.into(),
            message: None,
            labels: vec![],
            help: None,
        }
    }

    pub(crate) fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub(crate) fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub(crate) fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Severity {
    Error,
}

#[derive(Debug, Clone)]
pub(crate) struct Label {
    style: LabelStyle,
    message: Option<String>,
    span: Span,
}

impl Label {
    pub(crate) fn primary(span: Span) -> Self {
        Self {
            style: LabelStyle::Primary,
            message: None,
            span,
        }
    }

    pub(crate) fn secondary(span: Span) -> Self {
        Self {
            style: LabelStyle::Secondary,
            message: None,
            span,
        }
    }

    pub(crate) fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }
}

#[derive(Debug, Clone)]
enum LabelStyle {
    Primary,
    Secondary,
}

impl Into<codespan_diagnostic::Diagnostic<SourceId>> for Diagnostic {
    fn into(self) -> codespan_diagnostic::Diagnostic<SourceId> {
        codespan_diagnostic::Diagnostic {
            severity: self.severity.into(),
            code: Some(self.code),
            message: self.message.unwrap_or_else(|| "".to_string()),
            labels: self.labels.into_iter().map(|label| label.into()).collect(),
            notes: self.help.map_or_else(|| vec![], |help| vec![help]),
        }
    }
}

impl Into<codespan_diagnostic::Label<SourceId>> for Label {
    fn into(self) -> codespan_diagnostic::Label<SourceId> {
        codespan_diagnostic::Label {
            style: match self.style {
                LabelStyle::Primary => codespan_diagnostic::LabelStyle::Primary,
                LabelStyle::Secondary => codespan_diagnostic::LabelStyle::Secondary,
            },
            file_id: self.span.source_id(),
            range: self.span.start() as usize..self.span.end() as usize,
            message: self.message.unwrap_or_else(|| "".to_string()),
        }
    }
}

impl Into<codespan_diagnostic::Severity> for Severity {
    fn into(self) -> codespan_diagnostic::Severity {
        match self {
            Severity::Error => codespan_diagnostic::Severity::Error,
        }
    }
}

#[derive(Debug)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub(crate) fn new() -> Self {
        Self {
            diagnostics: vec![],
        }
    }

    pub(crate) fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn any(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub(crate) fn print(&self, sources: &Sources) -> Result<(), codespan_reporting::files::Error> {
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
