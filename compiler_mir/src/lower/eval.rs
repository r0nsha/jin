use compiler_core::middle::{BinOp, CmpOp, UnOp};
use compiler_core::span::Span;
use compiler_core::ty::{Ty, TyKind};
use rustc_hash::FxHashMap;

use crate::{lower::Lower, BlockId, Body, Const, Inst, ValueId, ValueKind};

impl<'db> Lower<'db> {
    pub(crate) fn eval(&mut self, body: &Body) -> EvalResult<Const> {
        Eval::new(self, body).eval()
    }
}

pub(super) struct Eval<'cx, 'db> {
    _cx: &'cx mut Lower<'db>,
    body: &'cx Body,
    current_block: BlockId,
    current_inst: usize,
    values: FxHashMap<ValueId, Const>,
}

impl<'cx, 'db> Eval<'cx, 'db> {
    fn new(cx: &'cx mut Lower<'db>, body: &'cx Body) -> Self {
        Self {
            _cx: cx,
            body,
            current_block: BlockId::start(),
            current_inst: 0,
            values: FxHashMap::default(),
        }
    }

    fn eval(&mut self) -> EvalResult<Const> {
        loop {
            let inst = self.current_inst();

            match inst {
                Inst::Br { target } => {
                    self.current_block = *target;
                    continue;
                }
                Inst::BrIf { cond, then, otherwise, span } => {
                    let cond = self.value(*cond, *span)?.as_bool().unwrap();

                    if *cond {
                        self.current_block = *then;
                    } else if let Some(otherwise) = otherwise {
                        self.current_block = *otherwise;
                    }

                    continue;
                }
                Inst::Switch { cond, blocks, span } => {
                    let cond = self.value(*cond, *span)?.as_int().unwrap();
                    self.current_block = blocks[*cond as usize];
                    continue;
                }
                Inst::Return { value, span } => {
                    let value = self.value(*value, *span)?;
                    return Ok(value.clone());
                }
                Inst::Unary { value, inner, op, span, .. } => {
                    let inner = self.value(*inner, *span)?;
                    let result = match op {
                        UnOp::Neg => inner.neg(),
                        UnOp::Not => inner.not(),
                        UnOp::Ref(_) => return Err(EvalError::UnsupportedInst(*span)),
                    };
                    self.store(*value, result);
                }
                Inst::Binary { value, lhs, rhs, op, span, .. } => {
                    let lhs = self.value(*lhs, *span)?;
                    let rhs = self.value(*rhs, *span)?;

                    let result = match op {
                        BinOp::Add => lhs.add(rhs),
                        BinOp::Sub => lhs.sub(rhs),
                        BinOp::Mul => lhs.mul(rhs),
                        BinOp::Div => lhs.div(rhs),
                        BinOp::Rem => lhs.rem(rhs),
                        BinOp::Shl => lhs.shl(rhs),
                        BinOp::Shr => lhs.shr(rhs),
                        BinOp::BitAnd => lhs.bitand(rhs),
                        BinOp::BitOr => lhs.bitor(rhs),
                        BinOp::BitXor => lhs.bitxor(rhs),
                        BinOp::And => lhs.and(rhs),
                        BinOp::Or => lhs.or(rhs),
                        BinOp::Cmp(op) => match op {
                            CmpOp::Eq => lhs.eq(rhs),
                            CmpOp::Ne => lhs.ne(rhs),
                            CmpOp::Lt => lhs.lt(rhs),
                            CmpOp::Le => lhs.le(rhs),
                            CmpOp::Gt => lhs.gt(rhs),
                            CmpOp::Ge => lhs.ge(rhs),
                        },
                    };
                    self.store(*value, result);
                }
                Inst::Convert { value, source, target, span } => {
                    let source_ty = self.ty_of(*source);
                    let source = self.value(*source, *span)?;
                    let result = match (source_ty.kind(), target.kind()) {
                        (TyKind::Int(_) | TyKind::Uint(_), TyKind::Float(_)) => {
                            Const::Float(*source.as_int().unwrap() as f64)
                        }
                        (TyKind::Float(_), TyKind::Int(_) | TyKind::Uint(_)) => {
                            Const::Int(*source.as_float().unwrap() as i128)
                        }
                        (
                            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Char,
                            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Char,
                        ) => source.clone(),
                        _ => return Err(EvalError::UnsupportedInst(*span)),
                    };
                    self.store(*value, result);
                }
                Inst::StrLit { value, lit, .. } => {
                    self.store(*value, Const::Str(*lit));
                }
                Inst::StackAlloc { span, .. }
                | Inst::Store { span, .. }
                | Inst::Alloc { span, .. }
                | Inst::SliceAlloc { span, .. }
                | Inst::SliceIndex { span, .. }
                | Inst::SliceSlice { span, .. }
                | Inst::SliceStore { span, .. }
                | Inst::Destroy { span, .. }
                | Inst::Free { span, .. }
                | Inst::IncRef { span, .. }
                | Inst::DecRef { span, .. }
                | Inst::Call { span, .. }
                | Inst::RtCall { span, .. }
                | Inst::Cast { span, .. }
                | Inst::Unreachable { span, .. } => {
                    // dbg!(inst);
                    return Err(EvalError::UnsupportedInst(*span));
                }
            }

            self.current_inst += 1;
        }
    }

    #[inline]
    fn value(&self, value: ValueId, span: Span) -> EvalResult<&Const> {
        match &self.body.value(value).kind {
            ValueKind::Const(v) => Ok(v),
            _ => self.values.get(&value).ok_or(EvalError::NonConstValue(value, span)),
        }
    }

    #[inline]
    fn store(&mut self, target: ValueId, value: Const) {
        self.values.insert(target, value);
    }

    #[inline]
    fn current_inst(&self) -> &Inst {
        &self.body.block(self.current_block).insts[self.current_inst]
    }

    #[inline]
    fn ty_of(&self, value: ValueId) -> Ty {
        self.body.value(value).ty
    }
}

pub(crate) type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub(crate) enum EvalError {
    UnsupportedInst(Span),
    NonConstValue(ValueId, Span),
}
