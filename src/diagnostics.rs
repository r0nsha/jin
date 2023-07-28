use crate::span::Span;

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

pub enum Severity {
    Error,
}

pub struct Label {
    kind: LabelKind,
    message: Option<String>,
    span: Span,
}

impl Label {
    pub fn primary(span: Span) -> Self {
        Self {
            kind: LabelKind::Primary,
            message: None,
            span,
        }
    }

    pub fn secondary(span: Span) -> Self {
        Self {
            kind: LabelKind::Secondary,
            message: None,
            span,
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }
}

enum LabelKind {
    Primary,
    Secondary,
}
