use std::{fmt, path::Path};

use ustr::{ustr, Ustr};

use crate::common::Word;

#[derive(Debug, Clone)]
pub struct QualifiedName(Vec<Ustr>);

impl QualifiedName {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn from_path(root: &Path, target: &Path) -> Option<Self> {
        let target = target.with_extension("");
        let stripped = target.strip_prefix(root).ok()?;

        Some(Self(
            stripped.iter().map(|component| ustr(component.to_string_lossy().as_ref())).collect(),
        ))
    }

    pub fn name(&self) -> Ustr {
        *self.0.last().unwrap()
    }

    pub fn full_name(&self, separator: &str) -> String {
        self.0.iter().map(Ustr::as_str).collect::<Vec<_>>().join(separator)
    }

    pub fn standard_full_name(&self) -> String {
        self.full_name(".")
    }

    pub fn full_c_name(&self) -> String {
        self.full_name("_")
    }

    pub fn child(mut self, child: impl Into<Child>) -> Self {
        match child.into() {
            Child::Qualified(q) => self.extend(q.0),
            Child::Single(s) => self.0.push(s),
        }
        self
    }
}

#[derive(Debug, Clone)]
pub enum Child {
    Qualified(QualifiedName),
    Single(Ustr),
}

impl From<QualifiedName> for Child {
    fn from(value: QualifiedName) -> Self {
        Self::Qualified(value)
    }
}

impl From<Ustr> for Child {
    fn from(value: Ustr) -> Self {
        Self::Single(value)
    }
}

impl Default for QualifiedName {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Ustr> for QualifiedName {
    fn from(value: Ustr) -> Self {
        if value.is_empty() {
            Self::new()
        } else {
            Self(vec![value])
        }
    }
}

impl From<Word> for QualifiedName {
    fn from(value: Word) -> Self {
        Self::from(value.name())
    }
}

impl IntoIterator for QualifiedName {
    type Item = Ustr;

    type IntoIter = std::vec::IntoIter<Ustr>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Extend<Ustr> for QualifiedName {
    fn extend<T: IntoIterator<Item = Ustr>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.standard_full_name())
    }
}
