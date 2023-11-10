use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{FnState, Generator},
        ty::CTy,
    },
    middle::BinOp,
    mir::ValueId,
    span::Span,
    ty::Ty,
};

#[derive(Debug)]
pub struct BinOpData {
    pub target: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
    pub op: BinOp,
    pub ty: Ty,
    pub span: Span,
}

impl<'db> Generator<'db> {
    pub fn codegen_cast(
        &self,
        state: &FnState<'db>,
        value: ValueId,
        casted: ValueId,
        target: Ty,
        span: Span,
    ) -> D<'db> {
        let cast = self.value_assign(state, value, || {
            D::text("(")
                .append(target.cty(self))
                .append(D::text(")"))
                .append(self.value(state, casted))
        });

        let casted_ty = state.body.value(casted).ty;

        if casted_ty.is_any_int() && target.is_any_int() {
            let (value_bits, target_bits) =
                (casted_ty.size(&self.target_metrics), target.size(&self.target_metrics));

            if target_bits < value_bits {
                let casted_doc = self.value(state, casted);
                let (min, max) = (target.min(), target.max());

                let cond = D::text("(")
                    .append(casted_doc.clone())
                    .append(format!(" < {min}) || ("))
                    .append(casted_doc)
                    .append(format!(" > {max})"));

                return D::intersperse(
                    [
                        self.panic_if(
                            cond,
                            &format!(
                                "value is out of range of type `{}`: {min}..{max}",
                                target.display(self.db)
                            ),
                            span,
                        ),
                        cast,
                    ],
                    D::hardline(),
                );
            }
        }

        cast
    }

    pub fn codegen_bin_op(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        if data.ty.is_any_int() {
            // Perform safety checked ops
            match data.op {
                BinOp::Add => return self.codegen_bin_op_add(state, data),
                BinOp::Sub => return self.codegen_bin_op_sub(state, data),
                BinOp::Mul => return self.codegen_bin_op_mul(state, data),
                BinOp::Div | BinOp::Rem => return self.codegen_bin_op_div(state, data),
                _ => (),
            }
        }

        let (lhs, rhs) = (self.value(state, data.lhs), self.value(state, data.rhs));
        self.value_assign(state, data.target, || bin_op(lhs, data.op, rhs))
    }

    fn codegen_bin_op_add(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_safe_bin_op(state, "add", "add", data)
    }

    fn codegen_bin_op_sub(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_safe_bin_op(state, "sub", "subtract", data)
    }

    fn codegen_bin_op_mul(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        self.codegen_safe_bin_op(state, "mul", "multiply", data)
    }

    fn codegen_bin_op_div(&self, state: &FnState<'db>, data: &BinOpData) -> D<'db> {
        let (lhs, rhs) = (self.value(state, data.lhs), self.value(state, data.rhs));

        let cond = rhs.clone().append(D::text(" == 0"));
        let safety_check = self.panic_if(cond, "attempt to divide by zero", data.span);

        let op = self.value_assign(state, data.target, || bin_op(lhs, data.op, rhs));

        D::intersperse([safety_check, op], D::hardline())
    }

    fn codegen_safe_bin_op(
        &self,
        state: &FnState<'db>,
        fname: &str,
        action: &str,
        data: &BinOpData,
    ) -> D<'db> {
        let decl = self.value_decl(state, data.target);

        let builtin_name = format!(
            "__builtin_{}{}{}_overflow",
            if data.ty.is_int() { "s" } else { "u" },
            fname,
            match data.ty.size(&self.target_metrics) {
                8..=16 => "",
                32 => "l",
                64 => "ll",
                _ => unreachable!(),
            }
        );

        let (target, lhs, rhs) = (
            self.value(state, data.target),
            self.value(state, data.lhs),
            self.value(state, data.rhs),
        );

        let builtin_call = D::text(builtin_name)
            .append("(")
            .append(lhs)
            .append(", ")
            .append(rhs)
            .append(", &")
            .append(target)
            .append(")");

        D::intersperse(
            [decl, self.panic_if(builtin_call, &overflow_msg(action), data.span)],
            D::hardline(),
        )
    }
}

fn overflow_msg(action: &str) -> String {
    format!("attempt to {action} with overflow")
}

fn bin_op<'a>(lhs: D<'a>, op: BinOp, rhs: D<'a>) -> D<'a> {
    D::intersperse([lhs, D::text(op.to_string()), rhs], D::space())
}
