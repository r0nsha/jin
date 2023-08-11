use crate::db::TyId;

#[derive(Debug, Clone)]
pub(crate) struct Constraints(Vec<Constraint>);

impl Constraints {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }

    pub(crate) fn one(c: Constraint) -> Self {
        Self(vec![c])
    }

    pub(crate) fn push(&mut self, constraint: Constraint) {
        self.0.push(constraint)
    }

    pub(crate) fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub(crate) fn merge(self, other: Self) -> Self {
        Self(self.0.into_iter().chain(other.0).collect())
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Constraint> {
        self.0.iter()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Constraint {
    Eq { expected: TyId, actual: TyId },
    Callable { callee: TyId },
}
