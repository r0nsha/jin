use crate::db::{FunId, ModuleId, TyId};

pub(crate) struct TypeEnv {
    module_id: ModuleId,
    pub(crate) fun_scopes: FunScopes,
}

impl TypeEnv {
    pub(crate) fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            fun_scopes: FunScopes::new(),
        }
    }

    pub(crate) fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

#[derive(Debug)]
pub(crate) struct FunScopes(Vec<FunScope>);

impl FunScopes {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }

    pub(crate) fn push(&mut self, new_scope: FunScope) {
        self.0.push(new_scope);
    }

    pub(crate) fn pop(&mut self) {
        self.0.pop();
    }

    pub(crate) fn current(&self) -> Option<&FunScope> {
        self.0.last()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FunScope {
    pub(crate) id: FunId,
    pub(crate) ret_ty: TyId,
}
