use std::path::Path;

use ustr::{ustr, Ustr};

#[derive(Debug, Clone)]
pub struct QualifiedName(Vec<Ustr>);

impl QualifiedName {
    pub fn from_path(root: &Path, target: &Path) -> Option<Self> {
        let target = target.with_extension("");
        let stripped = target.strip_prefix(root).ok()?;

        Some(Self(stripped.iter().map(|component| ustr(component.to_string_lossy().as_ref())).collect()))
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

    pub fn child(mut self, name: Ustr) -> Self {
        self.0.push(name);
        self
    }
}
