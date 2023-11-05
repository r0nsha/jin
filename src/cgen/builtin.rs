use pretty::RcDoc as D;

use crate::{
    cgen::util::{if_stmt, stmt, value_name_str},
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
    let (lhs, rhs) = (value_name_str(lhs), value_name_str(rhs));
    let (min, max) = (ty.min(), ty.max());
    let cond = D::text(format!(
        "(({rhs} < 0) && ({lhs} > ({max} + {rhs}))) || \
        (({rhs} >= 0) && ({lhs} < ({min} + {rhs})))"
    ));
    panic_if(cond, &overflow_msg("subtract"))
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
