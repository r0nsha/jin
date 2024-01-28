use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{GenState, Generator},
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
    pub span: Option<Span>,
}

impl BinOpData {
    fn do_safety_checks(&self) -> bool {
        self.span.is_some()
    }
}

impl<'db> Generator<'db> {
    pub fn codegen_cast(
        &mut self,
        state: &GenState<'db>,
        value: ValueId,
        casted: ValueId,
        target: Ty,
        span: Span,
    ) -> D<'db> {
        let cast = self.value_assign(state, value, |this| {
            util::cast(target.cty(this), this.value(state, casted))
        });

        let casted_ty = state.ty_of(casted);

        if casted_ty.is_any_int() && target.is_any_int() {
            let (value_bits, target_bits) =
                (casted_ty.size(&self.target_metrics), target.size(&self.target_metrics));

            if target_bits < value_bits {
                let casted_doc = self.value(state, casted);
                let (min, max) = (target.min(), target.max());

                let cond = util::group(casted_doc.clone().append(format!(" < {min}")))
                    .append(D::text(" || "))
                    .append(util::group(casted_doc.append(format!(" > {max}"))));

                return D::intersperse(
                    [
                        self.panic_if(
                            state,
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

    pub fn codegen_bin_op(&mut self, state: &GenState<'db>, data: &BinOpData) -> D<'db> {
        if data.do_safety_checks() && data.ty.is_any_int() {
            match data.op {
                BinOp::Add | BinOp::Sub | BinOp::Mul => {
                    return self.codegen_safe_bin_op(state, data)
                }
                BinOp::Div | BinOp::Rem => return self.codegen_safe_bin_op_div(state, data),
                _ => (),
            }
        }

        self.codegen_bin_op_unchecked(state, data)
    }

    pub fn codegen_bin_op_unchecked(&mut self, state: &GenState<'db>, data: &BinOpData) -> D<'db> {
        let (lhs, rhs) = (self.value(state, data.lhs), self.value(state, data.rhs));

        let init = match (data.op, data.ty.kind()) {
            (BinOp::Rem, TyKind::Float(fty)) => util::call(
                D::text(match fty {
                    FloatTy::F32 => "fmodf",
                    FloatTy::F64 => "fmod",
                }),
                [lhs, rhs],
            ),
            (BinOp::Cmp(CmpOp::Eq), TyKind::Str) => cmp_strs(lhs, rhs),
            (BinOp::Cmp(CmpOp::Ne), TyKind::Str) => D::text("!").append(cmp_strs(lhs, rhs)),
            _ => bin_op(lhs, data.op, rhs),
        };

        self.value_assign(state, data.target, |_| init)
    }

    fn codegen_safe_bin_op_div(&mut self, state: &GenState<'db>, data: &BinOpData) -> D<'db> {
        let (lhs, rhs) = (self.value(state, data.lhs), self.value(state, data.rhs));

        let cond = rhs.clone().append(D::text(" == 0"));
        let safety_check =
            self.panic_if(state, cond, "attempt to divide by zero", data.span.unwrap());

        let op = self.value_assign(state, data.target, |_| bin_op(lhs, data.op, rhs));

        D::intersperse([safety_check, op], D::hardline())
    }

    fn codegen_safe_bin_op(&mut self, state: &GenState<'db>, data: &BinOpData) -> D<'db> {
        let (lhs, rhs) = (self.value(state, data.lhs), self.value(state, data.rhs));

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
                self.panic_if(state, cond, &overflow_msg(action), data.span.unwrap()),
                self.value_assign(state, data.target, |_| bin_op(lhs, data.op, rhs)),
            ],
            D::hardline(),
        )
    }

    #[allow(clippy::too_many_lines)]
    fn codegen_bin_op_overflow_check_cond(
        &self,
        data: &BinOpData,
        lhs: &D<'db>,
        rhs: &D<'db>,
    ) -> D<'db> {
        let target_metrics = &self.target_metrics;

        let (min, max) = match data.ty.kind() {
            TyKind::Int(IntTy::Int) => (D::text("INTPTR_MIN"), D::text("INTPTR_MAX")),
            TyKind::Int(ity) => {
                let size = ity.size(target_metrics);
                (D::text(format!("INT{size}_MIN")), D::text(format!("INT{size}_MAX")))
            }
            TyKind::Uint(UintTy::Uint) => (D::text("UINTPTR_MIN"), D::text("UINTPTR_MAX")),
            TyKind::Uint(uty) => {
                let size = uty.size(target_metrics);
                (D::text(format!("UINT{size}_MIN")), D::text(format!("UINT{size}_MAX")))
            }
            ty => unreachable!("{ty:?}"),
        };

        match (data.op, data.ty.kind()) {
            // (rhs > 0 && lhs > max - rhs) || (rhs < 0 && lhs < min - rhs)
            (BinOp::Add, TyKind::Int(_)) => util::group(
                rhs.clone()
                    .append(D::text(" > 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" > "))
                    .append(max)
                    .append(D::text(" - "))
                    .append(rhs.clone()),
            )
            .append(D::text(" || "))
            .append(util::group(
                rhs.clone()
                    .append(D::text(" < 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" < "))
                    .append(min)
                    .append(D::text(" - "))
                    .append(rhs.clone()),
            )),

            // (rhs < 0 && lhs > max + rhs) || (rhs > 0 && lhs < min + rhs)
            (BinOp::Sub, TyKind::Int(_)) => util::group(
                rhs.clone()
                    .append(D::text(" < 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" > "))
                    .append(max)
                    .append(D::text(" + "))
                    .append(rhs.clone()),
            )
            .append(D::text(" || "))
            .append(util::group(
                rhs.clone()
                    .append(D::text(" > 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" < "))
                    .append(min)
                    .append(D::text(" + "))
                    .append(rhs.clone()),
            )),

            // (rhs != 0 && lhs > max / rhs) || (rhs != 0 && lhs < min / rhs)
            (BinOp::Mul, TyKind::Int(_)) => util::group(
                rhs.clone()
                    .append(D::text(" != 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" > "))
                    .append(max)
                    .append(D::text(" / "))
                    .append(rhs.clone()),
            )
            .append(D::text(" || "))
            .append(util::group(
                rhs.clone()
                    .append(D::text(" != 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" < "))
                    .append(min)
                    .append(D::text(" / "))
                    .append(rhs.clone()),
            )),

            // (rhs > 0 && lhs > max - rhs)
            (BinOp::Add, TyKind::Uint(_)) => util::group(
                rhs.clone()
                    .append(D::text(" > 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" > "))
                    .append(max)
                    .append(D::text(" - "))
                    .append(rhs.clone()),
            ),

            // (rhs > lhs)
            (BinOp::Sub, TyKind::Uint(_)) => {
                util::group(rhs.clone().append(D::text(" > ")).append(lhs.clone()))
            }

            // (rhs != 0 && lhs > max / rhs)
            (BinOp::Mul, TyKind::Uint(_)) => util::group(
                rhs.clone()
                    .append(D::text(" != 0 && "))
                    .append(lhs.clone())
                    .append(D::text(" > "))
                    .append(max)
                    .append(D::text(" / "))
                    .append(rhs.clone()),
            ),

            (op, ty) => unreachable!("{op} {ty:?}"),
        }
    }

    pub fn maybe_slice_index_boundscheck(
        &mut self,
        state: &GenState<'db>,
        slice: ValueId,
        index: ValueId,
        guarded_stmt: D<'db>,
        span: Option<Span>,
    ) -> D<'db> {
        if let Some(span) = span {
            let safety_check =
                util::stmt(|| self.slice_index_boundscheck(state, slice, index, span));

            D::intersperse([safety_check, guarded_stmt], D::hardline())
        } else {
            guarded_stmt
        }
    }

    pub fn slice_index_boundscheck(
        &mut self,
        state: &GenState<'db>,
        slice: ValueId,
        index: ValueId,
        span: Span,
    ) -> D<'db> {
        util::call(
            D::text("jinrt_slice_index_boundscheck"),
            [
                D::text("backtrace"),
                self.value(state, slice),
                self.value(state, index),
                self.create_stackframe_value(state, span),
            ],
        )
    }
}

fn overflow_msg(action: &str) -> String {
    format!("attempt to {action} with overflow")
}

fn bin_op<'a>(lhs: D<'a>, op: BinOp, rhs: D<'a>) -> D<'a> {
    D::intersperse([lhs, D::text(op.to_string()), rhs], D::space())
}
