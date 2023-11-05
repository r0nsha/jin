use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{FnState, Generator},
        util::{if_stmt, stmt, value_name_str},
    },
    middle::BinOp,
    mir::ValueId,
    ty::Ty,
};

impl<'db> Generator<'db> {
    pub fn codegen_bin_op(
        &self,
        state: &FnState<'db>,
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        op: BinOp,
        ty: Ty,
    ) -> D<'db> {
        match op {
            BinOp::Add => self.codegen_bin_op_add(state, target, lhs, rhs, ty),
            BinOp::Sub => self.codegen_bin_op_sub(state, target, lhs, rhs, ty),
            BinOp::Mul => self.codegen_bin_op_mul(state, target, lhs, rhs, ty),
            BinOp::Div | BinOp::Rem => self.codegen_bin_op_div(state, target, lhs, rhs, ty, op),
            _ => D::nil(),
        }
    }

    pub fn codegen_bin_op_add(
        &self,
        state: &FnState<'db>,
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        ty: Ty,
    ) -> D<'db> {
        self.codegen_bin_op_aux(state, "add", "add", target, lhs, rhs, ty)
    }

    pub fn codegen_bin_op_sub(
        &self,
        state: &FnState<'db>,
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        ty: Ty,
    ) -> D<'db> {
        self.codegen_bin_op_aux(state, "sub", "subtract", target, lhs, rhs, ty)
    }

    pub fn codegen_bin_op_mul(
        &self,
        state: &FnState<'db>,
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        ty: Ty,
    ) -> D<'db> {
        self.codegen_bin_op_aux(state, "mul", "multiply", target, lhs, rhs, ty)
    }

    pub fn codegen_bin_op_div(
        &self,
        state: &FnState<'db>,
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        ty: Ty,
        op: BinOp,
    ) -> D<'db> {
        todo!("divide by zero check")
    }

    fn codegen_bin_op_aux(
        &self,
        state: &FnState<'db>,
        fname: &str,
        action: &str,
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        ty: Ty,
    ) -> D<'db> {
        let decl = self.value_decl(state, target);
        let call = D::text(call_safe_arith_fn(fname, ty, lhs, rhs, target));
        D::intersperse([decl, panic_if(call, &overflow_msg(action))], D::hardline())
    }
}

fn panic_if<'a>(cond: D<'a>, msg: &str) -> D<'a> {
    let print_msg = stmt(|| D::text(format!("printf(\"panic: {msg}\\n\")")));
    let exit = stmt(|| D::text("exit(1)"));
    let then = D::intersperse([print_msg, exit], D::hardline());
    if_stmt(cond, then, None)
}

fn call_safe_arith_fn(action: &str, ty: Ty, lhs: ValueId, rhs: ValueId, target: ValueId) -> String {
    let (target, lhs, rhs) = (value_name_str(target), value_name_str(lhs), value_name_str(rhs));
    let builtin_name = get_safe_arith_fn(action, ty);
    format!("{builtin_name}({lhs}, {rhs}, &{target})")
}

fn get_safe_arith_fn(action: &str, ty: Ty) -> String {
    format!(
        "__builtin_{}{}{}_overflow",
        if ty.is_signed() { "s" } else { "u" },
        action,
        match ty.bits() {
            8..=16 => "",
            32 => "l",
            64 => "ll",
            _ => unreachable!(),
        }
    )
}

fn overflow_msg(action: &str) -> String {
    format!("attempt to {action} with overflow")
}
