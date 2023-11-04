use pretty::RcDoc as D;

use crate::{
    cgen::{generate::Generator, util::if_stmt},
    middle::BinOp,
    mir::ValueId,
    ty::Ty,
};

impl<'db> Generator<'db> {
    pub fn check_arithmetic_overflow(&mut self, ty: Ty, op: BinOp, value: ValueId) {
        const FNAME: &str = "jin_panic_arithmetic_overflow";
        todo!()
        // if_stmt(D::text, then, otherwise)
    }
}
