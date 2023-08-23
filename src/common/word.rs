use std::{cmp, fmt};

use ustr::Ustr;

use crate::span::{Span, Spanned};

#[derive(Debug, Clone, Copy)]
pub struct SpannedWord(Ustr, Span);

impl SpannedWord {
    pub fn new(name: Ustr, span: Span) -> Self {
        Self(name, span)
    }

    pub fn name(&self) -> Ustr {
        self.0
    }

    pub fn name_mut(&mut self) -> &mut Ustr {
        &mut self.0
    }

    pub fn as_str(&self) -> &str {
        self.name().as_str()
    }
}

impl From<SpannedWord> for Ustr {
    fn from(value: SpannedWord) -> Self {
        value.name()
    }
}

impl<'a> From<SpannedWord> for &'a str {
    fn from(value: SpannedWord) -> Self {
        value.name().as_str()
    }
}

impl From<SpannedWord> for Span {
    fn from(value: SpannedWord) -> Self {
        value.span()
    }
}

impl Spanned for SpannedWord {
    fn span(&self) -> Span {
        self.1
    }

    fn span_mut(&mut self) -> &mut Span {
        &mut self.1
    }
}

impl cmp::PartialEq for SpannedWord {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl cmp::Eq for SpannedWord {}

impl fmt::Display for SpannedWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
