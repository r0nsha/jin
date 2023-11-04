use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::Generator,
        util::{if_stmt, stmt},
    },
    middle::BinOp,
    mir::ValueId,
    ty::Ty,
};

impl<'db> Generator<'db> {
    pub fn bin_op_safety_check(&mut self, ty: Ty, lhs: ValueId, rhs: ValueId, op: BinOp) -> D<'db> {
        match op {
            BinOp::Add => todo!(),
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Rem => todo!(),
            BinOp::Shl => todo!(),
            BinOp::Shr => todo!(),
            BinOp::BitAnd => todo!(),
            BinOp::BitOr => todo!(),
            BinOp::BitXor => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            BinOp::Cmp(_) => todo!(),
        }
    }

    pub fn safety_check(&mut self, cond: D<'db>, msg: &str) -> D<'db> {
        let print_msg = stmt(|| D::text(format!("printf(\"panic: {msg}\\n\")")));
        let exit = stmt(|| D::text("exit(1)"));
        let then = D::intersperse([print_msg, exit], D::hardline());
        if_stmt(cond, then, None)
    }
}
