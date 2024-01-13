use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{FnState, Generator},
        ty::CTy,
        util,
        util::cmp_strs,
    },
    middle::{BinOp, CmpOp},
    mir::ValueId,
    span::Span,
    ty::{FloatTy, IntTy, Ty, TyKind, UintTy},
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
        &mut self,
        state: &FnState<'db>,
        value: ValueId,
        casted: ValueId,
        target: Ty,
        span: Span,
    ) -> D<'db> {
        let cast = self.value_assign(state, value, |this| {
            util::cast(target.cty(this), this.value(state, casted))
        });

        let casted_ty = state.body.value(casted).ty;

        if casted_ty.is_any_int() && target.is_any_int() {
            let (value_bits, target_bits) = (
                casted_ty.size(&self.target_metrics),
                target.size(&self.target_metrics),
            );

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
                                "value is out of range of type `{}`: \
                                 {min}..{max}",
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

    pub fn codegen_bin_op(
        &mut self,
        state: &FnState<'db>,
        data: &BinOpData,
    ) -> D<'db> {
        if data.ty.is_any_int() {
            // Perform safety checked ops
            match data.op {
                BinOp::Add | BinOp::Sub | BinOp::Mul => {
                    return self.codegen_safe_bin_op2(state, data)
                }
                BinOp::Div | BinOp::Rem => {
                    return self.codegen_safe_bin_op_div(state, data)
                }
                _ => (),
            }
        }

        let (lhs, rhs) =
            (self.value(state, data.lhs), self.value(state, data.rhs));

        let init = match (data.op, data.ty.kind()) {
            (BinOp::Rem, TyKind::Float(fty)) => util::call(
                D::text(match fty {
                    FloatTy::F32 => "fmodf",
                    FloatTy::F64 => "fmod",
                }),
                [lhs, rhs],
            ),
            (BinOp::Cmp(CmpOp::Eq), TyKind::Str) => cmp_strs(lhs, rhs),
            (BinOp::Cmp(CmpOp::Ne), TyKind::Str) => {
                D::text("!").append(cmp_strs(lhs, rhs))
            }
            _ => bin_op(lhs, data.op, rhs),
        };

        self.value_assign(state, data.target, |_| init)
    }

    fn codegen_safe_bin_op_div(
        &mut self,
        state: &FnState<'db>,
        data: &BinOpData,
    ) -> D<'db> {
        let (lhs, rhs) =
            (self.value(state, data.lhs), self.value(state, data.rhs));

        let cond = rhs.clone().append(D::text(" == 0"));
        let safety_check =
            self.panic_if(cond, "attempt to divide by zero", data.span);

        let op = self
            .value_assign(state, data.target, |_| bin_op(lhs, data.op, rhs));

        D::intersperse([safety_check, op], D::hardline())
    }

    fn codegen_safe_bin_op2(
        &mut self,
        state: &FnState<'db>,
        data: &BinOpData,
    ) -> D<'db> {
        let (lhs, rhs) =
            (self.value(state, data.lhs), self.value(state, data.rhs));

        let cond = self.codegen_bin_op_overflow_check_cond(data, &lhs, &rhs);

        let action = match data.op {
            BinOp::Add => "add",
            BinOp::Sub => "subtract",
            BinOp::Mul => "multiply",
            BinOp::Div => "divide",
            op => unreachable!("{op}"),
        };

        D::intersperse(
            [
                self.panic_if(cond, &overflow_msg(action), data.span),
                self.value_assign(state, data.target, |_| {
                    bin_op(lhs, data.op, rhs)
                }),
            ],
            D::hardline(),
        )
    }

    fn codegen_bin_op_overflow_check_cond(
        &self,
        data: &BinOpData,
        lhs: &D<'db>,
        rhs: &D<'db>,
    ) -> D<'db> {
        let target_metrics = &self.target_metrics;

        let (min, max) = match data.ty.kind() {
            TyKind::Int(IntTy::Int) => {
                (D::text("INTPTR_MIN"), D::text("INTPTR_MAX"))
            }
            TyKind::Int(ity) => {
                let size = ity.size(target_metrics);
                (
                    D::text(format!("INT{size}_MIN")),
                    D::text(format!("INT{size}_MAX")),
                )
            }
            TyKind::Uint(UintTy::Uint) => {
                (D::text("UINTPTR_MIN"), D::text("UINTPTR_MAX"))
            }
            TyKind::Uint(uty) => {
                let size = uty.size(target_metrics);
                (
                    D::text(format!("UINT{size}_MIN")),
                    D::text(format!("UINT{size}_MAX")),
                )
            }
            ty => unreachable!("{ty:?}"),
        };

        match (data.op, data.ty.kind()) {
            // (rhs > 0 && lhs > max - rhs) || (rhs < 0 && lhs < min - rhs)
            (BinOp::Add, TyKind::Int(_)) => D::text("(")
                .append(rhs.clone())
                .append(D::text(" > 0 && "))
                .append(lhs.clone())
                .append(D::text(" > "))
                .append(max)
                .append(D::text(" - "))
                .append(rhs.clone())
                .append(D::text(") || ("))
                .append(rhs.clone())
                .append(D::text(" < 0 && "))
                .append(lhs.clone())
                .append(D::text(" < "))
                .append(min)
                .append(D::text(" - "))
                .append(rhs.clone())
                .append(D::text(")")),

            // (rhs < 0 && lhs > max + rhs) || (rhs > 0 && lhs < min + rhs)
            (BinOp::Sub, TyKind::Int(_)) => D::text("(")
                .append(rhs.clone())
                .append(D::text(" < 0 && "))
                .append(lhs.clone())
                .append(D::text(" > "))
                .append(max)
                .append(D::text(" + "))
                .append(rhs.clone())
                .append(D::text(") || ("))
                .append(rhs.clone())
                .append(D::text(" > 0 && "))
                .append(lhs.clone())
                .append(D::text(" < "))
                .append(min)
                .append(D::text(" + "))
                .append(rhs.clone())
                .append(D::text(")")),

            // (rhs != 0 && lhs > max / rhs) || (rhs != 0 && lhs < min / rhs)
            (BinOp::Mul, TyKind::Int(_)) => D::text("(")
                .append(rhs.clone())
                .append(D::text(" != 0 && "))
                .append(lhs.clone())
                .append(D::text(" > "))
                .append(max)
                .append(D::text(" / "))
                .append(rhs.clone())
                .append(D::text(") || ("))
                .append(rhs.clone())
                .append(D::text(" != 0 && "))
                .append(lhs.clone())
                .append(D::text(" < "))
                .append(min)
                .append(D::text(" / "))
                .append(rhs.clone())
                .append(D::text(")")),

            // (rhs > 0 && lhs > max - rhs)
            (BinOp::Add, TyKind::Uint(_)) => D::text("(")
                .append(rhs.clone())
                .append(D::text(" > 0 && "))
                .append(lhs.clone())
                .append(D::text(" > "))
                .append(max)
                .append(D::text(" - "))
                .append(rhs.clone())
                .append(D::text(")")),

            // (rhs > lhs)
            (BinOp::Sub, TyKind::Uint(_)) => D::text("(")
                .append(rhs.clone())
                .append(D::text(" > "))
                .append(lhs.clone())
                .append(D::text(")")),

            // (rhs != 0 && lhs > max / rhs)
            (BinOp::Mul, TyKind::Uint(_)) => D::text("(")
                .append(rhs.clone())
                .append(D::text(" != 0 && "))
                .append(lhs.clone())
                .append(D::text(" > "))
                .append(max)
                .append(D::text(" / "))
                .append(rhs.clone())
                .append(D::text(")")),

            (op, ty) => unreachable!("{op} {ty:?}"),
        }
    }
}

fn overflow_msg(action: &str) -> String {
    format!("attempt to {action} with overflow")
}

fn bin_op<'a>(lhs: D<'a>, op: BinOp, rhs: D<'a>) -> D<'a> {
    D::intersperse([lhs, D::text(op.to_string()), rhs], D::space())
}
