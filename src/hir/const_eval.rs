use std::collections::HashMap;

use crate::{
    ast::UnOp,
    hir::{Expr, ExprId, ExprKind, Lit},
};

#[derive(Debug)]
pub struct ConstStorage {
    exprs: HashMap<ExprId, Const>,
}

impl ConstStorage {
    pub fn new() -> Self {
        Self { exprs: HashMap::new() }
    }

    #[inline]
    pub fn expr(&self, id: ExprId) -> Option<&Const> {
        self.exprs.get(&id)
    }

    pub fn eval_expr(&mut self, expr: &Expr) {
        // TODO: const eval arithmetic
        // TODO: const eval cmp
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
            | ExprKind::Binary(_)
            | ExprKind::Name(_) => None,
            ExprKind::Unary(un) => self.expr(un.expr.id).map(|val| match un.op {
                UnOp::Neg => match val {
                    Const::Int(v) => Const::Int(-v),
                    Const::Uint(v) => Const::Int(-i128::try_from(*v).unwrap()),
                    _ => unreachable!("invalid input in const neg: {:?}", val),
                },
                UnOp::Not => match val {
                    Const::Bool(v) => Const::Bool(!v),
                    _ => unreachable!("invalid input in const not: {:?}", val),
                },
            }),
            ExprKind::Lit(lit) => Some(match lit {
                Lit::Int(value) => Const::Uint(*value),
                Lit::Bool(value) => Const::Bool(*value),
                Lit::Unit => Const::Unit,
            }),
        };

        if let Some(result) = result {
            self.exprs.insert(expr.id, result);
        }
    }
}

impl Default for ConstStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Int(i128),
    Uint(u128),
    Bool(bool),
    Unit,
}
