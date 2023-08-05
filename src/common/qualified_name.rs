use std::path::Path;

use ustr::{ustr, Ustr};

#[derive(Debug, Clone)]
pub(crate) struct QualifiedName(Vec<Ustr>);

impl QualifiedName {
    pub(crate) fn new(full_name: Vec<Ustr>) -> Self {
        assert!(!full_name.is_empty());
        Self(full_name)
    }

    pub(crate) fn from_path(root: &Path, target: &Path) -> Option<Self> {
        let target = target.with_extension("");
        let stripped = target.strip_prefix(root).ok()?;

        Some(Self(
            stripped
                .iter()
                .map(|component| ustr(component.to_string_lossy().as_ref()))
                .collect(),
        ))
    }

    pub(crate) fn name(&self) -> Ustr {
        *self.0.last().unwrap()
    }

    pub(crate) fn full_name(&self, separator: &str) -> String {
        self.0
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub(crate) fn standard_full_name(&self) -> String {
        self.full_name(".")
    }

    pub(crate) fn full_c_name(&self) -> String {
        self.full_name("_")
    }

    pub(crate) fn child(mut self, name: Ustr) -> Self {
        self.0.push(name);
        self
    }
}
