use codespan_reporting::diagnostic as codespan_diagnostic;

use crate::span::{SourceId, Span};

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) enum Severity {
    Error,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
