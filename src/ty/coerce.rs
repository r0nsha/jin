use crate::{hir::Expr, ty::Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Coercions(Vec<Coercion>);

impl Coercions {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, adj: Coercion) {
        self.0.push(adj);
    }

    pub fn apply(&self, mut expr: Expr) -> Expr {
        for adj in &self.0 {
            expr = match adj.kind {
                CoercionKind::NeverToAny => expr,
            };
            expr.ty = adj.target;
        }

        expr
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
}
