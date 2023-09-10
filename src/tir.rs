mod lower;

pub use lower::lower;

use crate::{
    ast::{BinOp, UnOp},
    common::{new_key_type, IndexVec},
    db::DefId,
    ty::Ty,
};

new_key_type!(ExprId);

#[derive(Debug)]
pub struct Tir {
    pub functions: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: DefId,
    pub sig: FnSig,
    pub body: ExprId,

    exprs: IndexVec<ExprId, Expr>,
}

impl Fn {
    #[inline]
    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id]
    }

    #[inline]
    pub fn create_expr(&mut self, kind: ExprKind, ty: Ty) -> ExprId {
        self.exprs.push_with_key(|id| Expr { id, kind, ty })
    }
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub params: Vec<FnParam>,
    pub ret: Ty,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub id: DefId,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Let { id: DefId, value: ExprId },
    If { cond: ExprId, then: ExprId, otherwise: ExprId },
    Block { exprs: Vec<ExprId> },
    Return { value: ExprId },
    Call { callee: ExprId, args: Vec<ExprId> },
    Binary { lhs: ExprId, rhs: ExprId, op: BinOp },
    Unary { value: ExprId, op: UnOp },
    Cast { value: ExprId, target: Ty },
    Name { id: DefId },
    IntLit { value: usize },
    BoolLit { value: bool },
    UnitLit,
}
