use core::fmt;

use camino::Utf8Path;
use ustr::{ustr, Ustr};

use crate::word::Word;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QPath(Vec<Ustr>);

impl QPath {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn from_path(root: &Utf8Path, target: &Utf8Path) -> Option<Self> {
        let target = target.with_extension("");
        let stripped = target.strip_prefix(root).ok()?;

        Some(Self(stripped.iter().map(ustr).collect()))
    }

    pub fn root(&self) -> Ustr {
        *self.0.first().expect("to have at least one segment")
    }

    pub fn name(&self) -> Ustr {
        *self.0.last().expect("to have at least one segment")
    }

    pub fn join(&self) -> String {
        self.join_with(".")
    }

    pub fn join_with(&self, separator: &str) -> String {
        self.0.iter().map(Ustr::as_str).collect::<Vec<_>>().join(separator)
    }

    pub fn push(&mut self, seg: impl Into<Segment>) {
        match seg.into() {
            Segment::Qualified(q) => self.extend(q.0),
            Segment::Single(s) => self.0.push(s),
        }
    }

    pub fn child(mut self, seg: impl Into<Segment>) -> Self {
        self.push(seg);
        self
    }

    pub fn with_name(mut self, name: Ustr) -> Self {
        *self.0.last_mut().expect("to have at least one segment") = name;
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &Ustr> {
        self.0.iter()
    }

    pub fn is_subpath(&self, of: &Self) -> bool {
        self.iter().zip(of.iter()).all(|(a, b)| a == b)
    }
}

impl Default for QPath {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Segment {
    Qualified(QPath),
    Single(Ustr),
}

impl From<QPath> for Segment {
    fn from(value: QPath) -> Self {
        Self::Qualified(value)
    }
}

impl From<Ustr> for Segment {
    fn from(value: Ustr) -> Self {
        Self::Single(value)
    }
}

impl From<Ustr> for QPath {
    fn from(value: Ustr) -> Self {
        Self(vec![value])
    }
}

impl From<Word> for QPath {
    fn from(value: Word) -> Self {
        Self::from(value.name())
    }
}

impl<'a> FromIterator<&'a str> for QPath {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        let mut this = Self::new();
        this.extend(iter.into_iter().map(ustr));
        this
    }
}

impl IntoIterator for QPath {
    type Item = Ustr;

    type IntoIter = std::vec::IntoIter<Ustr>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Extend<Ustr> for QPath {
    fn extend<T: IntoIterator<Item = Ustr>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl fmt::Display for QPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.join())
    }
}
