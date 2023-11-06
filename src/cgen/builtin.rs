use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{FnState, Generator},
        ty::CTy,
        util::{panic_if, value_name, value_name_str},
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
    pub fn codegen_cast(&self, value: ValueId, target: Ty) -> D<'db> {
        D::text("(").append(target.cty(self)).append(D::text(")")).append(value_name(value))
    }

    pub fn codegen_bin_op(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        match data.op {
            BinOp::Add => self.codegen_bin_op_add(state, data),
            BinOp::Sub => self.codegen_bin_op_sub(state, data),
            BinOp::Mul => self.codegen_bin_op_mul(state, data),
            BinOp::Div | BinOp::Rem => self.codegen_bin_op_div(state, data),
            _ => D::nil(),
        }
    }

    fn codegen_bin_op_add(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_bin_op_aux(state, "add", "add", data)
    }

    fn codegen_bin_op_sub(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_bin_op_aux(state, "sub", "subtract", data)
    }

    fn codegen_bin_op_mul(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_bin_op_aux(state, "mul", "multiply", data)
    }

    fn codegen_bin_op_div(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        let (lhs, rhs) = (value_name_str(data.lhs), value_name_str(data.rhs));
        let safety_check = panic_if(D::text(format!("{rhs} == 0")), "attempt to divide by zero");
        let op = self
            .value_assign(state, data.target, || D::text(format!("{} {} {}", lhs, data.op, rhs)));
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
        let call = D::text(call_checked_arithmetic_builtin(fname, data));
        D::intersperse([decl, panic_if(call, &overflow_msg(action))], D::hardline())
    }
}

fn call_checked_arithmetic_builtin(action: &str, data: &BinOpData) -> String {
    let (target, lhs, rhs) =
        (value_name_str(data.target), value_name_str(data.lhs), value_name_str(data.rhs));
    let builtin_name = format!(
        "__builtin_{}{}{}_overflow",
        if data.ty.is_signed() { "s" } else { "u" },
        action,
        match data.ty.bits() {
            8..=16 => "",
            32 => "l",
            64 => "ll",
            _ => unreachable!(),
        }
    );
    format!("{builtin_name}({lhs}, {rhs}, &{target})")
}

fn overflow_msg(action: &str) -> String {
    format!("attempt to {action} with overflow")
}
