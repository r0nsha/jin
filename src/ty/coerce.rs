use crate::{
    hir,
    hir::{Cast, Expr, ExprKind},
    ty::Ty,
};

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

    pub fn apply(&self, mut expr: Expr) -> Expr {
        for coercion in &self.0 {
            expr = match coercion.kind {
                CoercionKind::NeverToAny => expr,
                CoercionKind::IntPromotion => Expr {
                    id: expr.id,
                    span: expr.span,
                    ty: coercion.target,
                    kind: ExprKind::Cast(Cast {
                        // NOTE: used a dummy Ty node here...
                        target: hir::Ty::Infer(expr.span),
                        expr: Box::new(expr),
                    }),
                },
            };
            expr.ty = coercion.target;
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
    IntPromotion,
}
