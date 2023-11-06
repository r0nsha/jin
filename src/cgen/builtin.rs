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

#[derive(Debug)]
pub struct BinOpData {
    pub target: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
    pub op: BinOp,
    pub ty: Ty,
}

impl<'db> Generator<'db> {
    pub fn codegen_bin_op(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        match data.op {
            BinOp::Add => self.codegen_bin_op_add(state, data),
            BinOp::Sub => self.codegen_bin_op_sub(state, data),
            BinOp::Mul => self.codegen_bin_op_mul(state, data),
            BinOp::Div | BinOp::Rem => self.codegen_bin_op_div(state, data),
            _ => D::nil(),
        }
    }

    pub fn codegen_bin_op_add(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_bin_op_aux(state, "add", "add", data)
    }

    pub fn codegen_bin_op_sub(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_bin_op_aux(state, "sub", "subtract", data)
    }

    pub fn codegen_bin_op_mul(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_bin_op_aux(state, "mul", "multiply", data)
    }

    pub fn codegen_bin_op_div(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        let safety_check = panic_if(D::text(format!("{} == 0", data.rhs)), div_by_zero_msg());
        let op = self.value_assign(state, data.target, || {
            D::text(format!("{} {} {}", data.lhs, data.op, data.rhs))
        });
        D::intersperse([safety_check, op], D::hardline())
    }

    fn codegen_bin_op_aux(
        &self,
        state: &FnState<'db>,
        fname: &str,
        action: &str,
        data: &BinOpData,
    ) -> D<'db> {
        let decl = self.value_decl(state, data.target);
        let call = D::text(call_safe_arith_fn(fname, data));
        D::intersperse([decl, panic_if(call, &overflow_msg(action))], D::hardline())
    }
}

fn panic_if<'a>(cond: D<'a>, msg: &str) -> D<'a> {
    let print_msg = stmt(|| D::text(format!("printf(\"panic: {msg}\\n\")")));
    let exit = stmt(|| D::text("exit(1)"));
    let then = D::intersperse([print_msg, exit], D::hardline());
    if_stmt(cond, then, None)
}

fn call_safe_arith_fn(action: &str, data: &BinOpData) -> String {
    let (target, lhs, rhs) =
        (value_name_str(data.target), value_name_str(data.lhs), value_name_str(data.rhs));
    let builtin_name = get_safe_arith_fn(action, data.ty);
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

fn div_by_zero_msg() -> &'static str {
    "attempt to divide by zero"
}
