use crate::db::{DefinitionId, ModuleId};
use crate::ty::typecx::TyId;

pub struct TypeEnv {
    module_id: ModuleId,
    pub fun_scopes: FunScopes,
}

impl TypeEnv {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, fun_scopes: FunScopes::new() }
    }

    #[allow(unused)]
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
    #[allow(unused)]
    pub id: DefinitionId,
    pub ret_ty: TyId,
}
