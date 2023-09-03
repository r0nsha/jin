use std::{cmp, fmt};

use ustr::Ustr;

use crate::span::{Span, Spanned};

#[derive(Debug, Clone, Copy)]
pub struct Word(Ustr, Span);

impl Word {
    #[inline]
    pub fn new(name: Ustr, span: Span) -> Self {
        Self(name, span)
    }

    #[inline]
    pub fn name(&self) -> Ustr {
        self.0
    }

    #[inline]
    pub fn name_mut(&mut self) -> &mut Ustr {
        &mut self.0
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.name().as_str()
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

impl Spanned for Word {
    fn span(&self) -> Span {
        self.1
    }

    fn span_mut(&mut self) -> &mut Span {
        &mut self.1
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
