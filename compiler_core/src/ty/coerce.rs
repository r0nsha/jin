use crate::ty::Ty;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Coercions(Vec<Coercion>);

impl Coercions {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, c: Coercion) {
        self.0.push(c);
    }

    pub fn extend(&mut self, c: Coercions) {
        self.0.extend(c.0);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Coercion> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Coercion> {
        self.0.iter_mut()
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
    AnyToNever,
    AnyToUnit,
    IntPromotion,
    MutRefToImm,
    OwnedToRef,
    RefToOwned,
}
