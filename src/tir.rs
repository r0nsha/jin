mod lower;
mod pretty_print;

use std::io;

pub use lower::lower;
use ustr::Ustr;

use crate::{
    ast::{BinOp, UnOp},
    common::{new_key_type, IndexVec},
    db::{Db, DefId},
    ty::Ty,
};

new_key_type!(FnId);
new_key_type!(FnSigId);
new_key_type!(LocalId);
new_key_type!(ExprId);

#[derive(Debug)]
pub struct Tir {
    pub sigs: IndexVec<FnSigId, FnSig>,
    pub fns: IndexVec<FnId, Fn>,
    pub main_fn: Option<FnSigId>,
}

impl Tir {
    pub fn new() -> Self {
        Self { sigs: IndexVec::new(), fns: IndexVec::new(), main_fn: None }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: FnId,
    pub def_id: DefId,
    pub sig: FnSigId,
    pub body: ExprId,
    exprs: Exprs,
    locals: Locals,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub id: LocalId,
    pub def_id: DefId,
    pub name: Ustr,
    pub ty: Ty,
}

pub type Exprs = IndexVec<ExprId, Expr>;
pub type Locals = IndexVec<LocalId, Local>;

impl Fn {
    #[inline]
    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id]
    }
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub id: FnSigId,
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub ret: Ty,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub def_id: DefId,
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
    Let { def_id: DefId, value: ExprId },
    If { cond: ExprId, then: ExprId, otherwise: Option<ExprId> },
    Block { exprs: Vec<ExprId> },
    Return { value: ExprId },
    Call { callee: ExprId, args: Vec<ExprId> },
    Binary { lhs: ExprId, rhs: ExprId, op: BinOp },
    Unary { value: ExprId, op: UnOp },
    Cast { value: ExprId, target: Ty },
    Id { id: Id },
    IntLit { value: usize },
    BoolLit { value: bool },
    UnitLit,
}

#[derive(Debug, Clone)]
pub enum Id {
    Fn(FnSigId),
    Local(LocalId),
}
