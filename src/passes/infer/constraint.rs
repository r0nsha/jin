use std::cmp::Ordering;
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
    Callable { callee: TyId },
    CallResult { callee: TyId, result: TyId },
}

impl Constraint {
    fn order(&self) -> usize {
        match self {
            Constraint::Eq { .. } => 0,
            Constraint::Callable { .. } => 1,
            Constraint::CallResult { .. } => 2,
        }
    }
}

impl Ord for Constraint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.order().cmp(&other.order())
    }
}

impl PartialOrd for Constraint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
