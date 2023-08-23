use crate::{db::SymbolId, ty::Type};

pub struct TypeEnv {
    pub call_stack: CallStack,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self { call_stack: CallStack::new() }
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
    pub id: SymbolId,
    pub ret_ty: Type,
}
