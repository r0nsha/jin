use crate::ty::Ty;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Coercions(Vec<Coercion>);

impl Coercions {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn one(c: Coercion) -> Self {
        Self(vec![c])
    }

    pub fn push(&mut self, adj: Coercion) {
        self.0.push(adj);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Coercion> {
        self.0.iter()
    }
}

impl Default for Coercions {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Coercion {
    pub kind: CoercionKind,
    pub target: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoercionKind {
    NeverToAny,
    IntPromotion,
    MutRefToImm,
}
