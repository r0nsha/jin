use crate::ty::Ty;

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
    pub ret_ty: Ty,
}
