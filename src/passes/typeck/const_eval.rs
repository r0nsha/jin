use std::collections::HashMap;

use crate::hir::{Expr, ExprId};

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

    pub fn eval_expr(&self, expr: &Expr) {
        todo!()
        // self.exprs.get(&id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Int(isize),
    Uint(usize),
    Bool(bool),
    Unit,
}
