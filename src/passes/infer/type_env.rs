use crate::db::{DefinitionId, ModuleId};
use crate::ty::typecx::TyId;

pub struct TypeEnv {
    module_id: ModuleId,
    pub call_stack: CallStack,
}

impl TypeEnv {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, call_stack: CallStack::new() }
    }

    #[allow(unused)]
    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

#[derive(Debug)]
pub struct CallStack(Vec<CallFrame>);

impl CallStack {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, frame: CallFrame) {
        self.0.push(frame);
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn current(&self) -> Option<&CallFrame> {
        self.0.last()
    }
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    #[allow(unused)]
    pub id: DefinitionId,
    pub ret_ty: TyId,
}
