use std::{cmp, fmt};

use ustr::Ustr;

use crate::span::Span;

#[derive(Debug, Clone, Copy)]
pub struct Word(Ustr, Span);

impl Word {
    pub fn new(name: Ustr, span: Span) -> Self {
        Word(name, span)
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

    pub fn span(&self) -> Span {
        self.1
    }

    pub fn span_mut(&mut self) -> &mut Span {
        &mut self.1
    }
}

impl From<Word> for Ustr {
    fn from(value: Word) -> Self {
        value.name()
    }
}

impl<'a> From<Word> for &'a str {
    fn from(value: Word) -> Self {
        value.name().as_str()
    }
}

impl From<Word> for Span {
    fn from(value: Word) -> Self {
        value.span()
    }
}

impl cmp::PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl cmp::Eq for Word {}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
