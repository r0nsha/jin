use pretty::RcDoc as D;

use crate::{
    cgen::util::{if_stmt, stmt},
    middle::BinOp,
    mir::ValueId,
    ty::Ty,
};

pub fn bin_op_safety_check<'a>(ty: Ty, lhs: ValueId, rhs: ValueId, op: BinOp) -> D<'a> {
    match op {
        BinOp::Add => add_safety_check(ty, lhs, rhs),
        BinOp::Sub => sub_safety_check(ty, lhs, rhs),
        BinOp::Mul => mul_safety_check(ty, lhs, rhs),
        BinOp::Div | BinOp::Rem => div_safety_check(ty, lhs, rhs),
        _ => D::nil(),
    }
}

pub fn add_safety_check<'a>(ty: Ty, lhs: ValueId, rhs: ValueId) -> D<'a> {
    todo!()
}

pub fn sub_safety_check<'a>(ty: Ty, lhs: ValueId, rhs: ValueId) -> D<'a> {
    todo!()
}

pub fn mul_safety_check<'a>(ty: Ty, lhs: ValueId, rhs: ValueId) -> D<'a> {
    todo!()
}

pub fn div_safety_check<'a>(ty: Ty, lhs: ValueId, rhs: ValueId) -> D<'a> {
    todo!()
}

pub fn panic_if<'a>(cond: D<'a>, msg: &str) -> D<'a> {
    let print_msg = stmt(|| D::text(format!("printf(\"panic: {msg}\\n\")")));
    let exit = stmt(|| D::text("exit(1)"));
    let then = D::intersperse([print_msg, exit], D::hardline());
    if_stmt(cond, then, None)
}

fn overflow_msg(action: &str) -> String {
    format!("attempt to {action} with overflow")
}
