use ustr::Ustr;

use crate::{
    hir::{BindingId, ModuleId},
    scopes::Scopes,
    ty::TyId,
};

pub struct Env {
    module_id: ModuleId,
    pub scopes: Scopes<Ustr, BindingId>,
    pub fun_scopes: FunScopes,
}

impl Env {
    pub fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            scopes: Scopes::new(),
            fun_scopes: FunScopes::new(),
        }
    }

    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

#[derive(Debug)]
pub struct FunScopes(Vec<FunScope>);

impl FunScopes {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, new_scope: FunScope) {
        self.0.push(new_scope);
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn current(&self) -> Option<&FunScope> {
        self.0.last()
    }
}

#[derive(Debug, Clone)]
pub struct FunScope {
    pub ret_ty: TyId,
}
