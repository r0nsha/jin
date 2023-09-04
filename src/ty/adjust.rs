use crate::{hir::Expr, ty::Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Adjustments(Vec<Adjust>);

impl Adjustments {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, adj: Adjust) {
        self.0.push(adj);
    }

    pub fn apply(&self, mut expr: Expr) -> Expr {
        for adj in &self.0 {
            expr = match adj.kind {
                AdjustKind::CoerceNever => expr,
            };
            expr.ty = adj.target;
        }

        expr
    }
}

impl Default for Adjustments {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Adjust {
    pub kind: AdjustKind,
    pub target: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdjustKind {
    CoerceNever,
}
