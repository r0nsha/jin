use std::{cmp, fmt};

use ustr::{Ustr, UstrMap};

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
}

impl cmp::PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl<'a> cmp::PartialEq<&'a str> for Word {
    fn eq(&self, other: &&'a str) -> bool {
        self.name() == *other
    }
}

impl cmp::Eq for Word {}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Default)]
pub struct WordMap(UstrMap<Span>);

impl WordMap {
    pub fn insert(&mut self, word: Word) -> Option<Span> {
        self.insert_split(word.name(), word.span())
    }

    pub fn insert_split(&mut self, name: Ustr, span: Span) -> Option<Span> {
        self.0.insert(name, span)
    }
}
