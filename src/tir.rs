mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
pub use lower::lower;
use ustr::Ustr;

use crate::{
    ast::{BinOp, UnOp},
    common::{new_key_type, IndexVec},
    db::{Db, DefId},
    hir::const_eval::Const,
    ty::Ty,
};

new_key_type!(FnSigId);
new_key_type!(GlobalId);
new_key_type!(LocalId);
new_key_type!(ExprId);

#[derive(Debug)]
pub struct Tir {
    pub sigs: IndexVec<FnSigId, FnSig>,
    pub fns: Vec<Fn>,
    pub extern_fns: Vec<ExternFn>,
    pub globals: IndexVec<GlobalId, Global>,
    pub main_fn: Option<FnSigId>,
}

impl Tir {
    pub fn new() -> Self {
        Self {
            sigs: IndexVec::new(),
            fns: vec![],
            extern_fns: vec![],
            globals: IndexVec::new(),
            main_fn: None,
        }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub def_id: DefId,
    pub sig: FnSigId,
    pub value: ExprId,
    pub body: Body,
}

impl Fn {
    #[inline]
    pub fn params(&self, tir: &Tir) -> &[Local] {
        &self.body.locals.as_slice()[0..tir.sigs[self.sig].params.len()]
    }
}

#[derive(Debug, Clone)]
pub struct ExternFn {
    pub def_id: DefId,
    pub sig: FnSigId,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub id: FnSigId,
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub ret: Ty,
    pub ty: Ty,
    pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub id: GlobalId,
    pub def_id: DefId,
    pub name: Ustr,
    pub value: ExprId,
    pub ty: Ty,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub struct Body {
    exprs: Exprs,
    locals: Locals,
}

impl Body {
    pub fn new() -> Self {
        Self { exprs: IndexVec::new(), locals: IndexVec::new() }
    }

    #[inline]
    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id]
    }

    #[inline]
    pub fn local(&self, id: LocalId) -> &Local {
        &self.locals[id]
    }
}

pub type Exprs = IndexVec<ExprId, Expr>;
pub type Locals = IndexVec<LocalId, Local>;

#[derive(Debug, Clone)]
pub struct Local {
    pub id: LocalId,
    pub def_id: DefId,
    pub name: Ustr,
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

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExprKind {
    Let { id: LocalId, def_id: DefId, value: ExprId },
    If { cond: ExprId, then: ExprId, otherwise: Option<ExprId> },
    Block { exprs: Vec<ExprId> },
    Return { value: ExprId },
    Call { callee: ExprId, args: Vec<ExprId> },
    Binary { lhs: ExprId, rhs: ExprId, op: BinOp },
    Unary { value: ExprId, op: UnOp },
    Cast { value: ExprId, target: Ty },
    Index { value: ExprId, index: usize },
    Id(Id),
    Const(Const),
}

#[derive(Debug, Clone)]
pub enum Id {
    Fn(FnSigId),
    Global(GlobalId),
    Local(LocalId),
}
