use std::collections::HashMap;

use crate::hir::{Expr, ExprId, ExprKind};

#[derive(Debug)]
pub struct ConstStorage {
    exprs: HashMap<ExprId, Const>,
}

impl ConstStorage {
    pub fn new() -> Self {
        Self { exprs: HashMap::new() }
    }

    pub fn expr(&self, id: ExprId) -> Option<&Const> {
        self.exprs.get(&id)
    }

    pub fn eval_expr(&mut self, expr: &Expr) {
        // TODO: const eval lit
        // TODO: const eval unary
        // TODO: const eval binary
        // TODO: const eval block
        // TODO: const eval cast
        // TODO: const eval name
        let result = match &expr.kind {
            ExprKind::Let(_)
            | ExprKind::If(_)
            | ExprKind::Return(_)
            | ExprKind::Call(_)
            | ExprKind::Cast(_)
            | ExprKind::Block(_)
            | ExprKind::Unary(_)
            | ExprKind::Binary(_)
            | ExprKind::Name(_)
            | ExprKind::Lit(_) => None,
        };

        if let Some(result) = result {
            self.exprs.insert(expr.id, result);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Int(i128),
    Uint(u128),
    Bool(bool),
    Unit,
}
