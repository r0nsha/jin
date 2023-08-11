use std::ops;

use crate::db::TyId;

#[derive(Debug, Clone)]
pub(crate) struct Constraints(Vec<Constraint>);

impl Constraints {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }
}

impl ops::Deref for Constraints {
    type Target = Vec<Constraint>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for Constraints {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Eq { expected: TyId, actual: TyId },
}
