use crate::{db::SymbolId, tast::Function, ty::Ty};

pub struct FnCtxt {
    pub id: SymbolId,
    pub ret_ty: Ty,
}

impl FnCtxt {
    pub fn from_function(fun: &Function) -> Self {
        FnCtxt { id: fun.id, ret_ty: fun.ty.as_function().unwrap().ret }
    }
}
