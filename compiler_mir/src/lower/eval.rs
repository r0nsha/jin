use compiler_core::span::Span;

use crate::{lower::Lower, Body, Const};
use crate::{BlockId, Inst, ValueId, ValueKind};

impl<'db> Lower<'db> {
    pub(crate) fn eval(&mut self, body: &Body) -> EvalResult<Const> {
        Eval::new(self, body).eval()
    }
}

pub(super) struct Eval<'cx, 'db> {
    cx: &'cx mut Lower<'db>,
    body: &'cx Body,
    current_block: BlockId,
    current_inst: usize,
}

impl<'cx, 'db> Eval<'cx, 'db> {
    fn new(cx: &'cx mut Lower<'db>, body: &'cx Body) -> Self {
        Self { cx, body, current_block: BlockId::start(), current_inst: 0 }
    }

    fn eval(&mut self) -> EvalResult<Const> {
        loop {
            let inst = self.current_inst();

            match inst {
                Inst::Br { target } => todo!(),
                Inst::BrIf { cond, then, otherwise } => todo!(),
                Inst::Switch { cond, blocks } => todo!(),
                Inst::Return { value, span } => {
                    let value = self.value(*value, *span)?;
                    return Ok(value.clone());
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
                | Inst::Return { span, .. }
                | Inst::Call { span, .. }
                | Inst::RtCall { span, .. }
                | Inst::Binary { span, .. }
                | Inst::Unary { span, .. }
                | Inst::Convert { span, .. }
                | Inst::Cast { span, .. }
                | Inst::StrLit { span, .. }
                | Inst::Unreachable { span, .. } => return Err(EvalError::UnsupportedInst(*span)),
            }
        }
    }

    #[inline]
    fn value(&self, value: ValueId, span: Span) -> EvalResult<&Const> {
        match &self.body.value(value).kind {
            ValueKind::Const(v) => Ok(v),
            _ => Err(EvalError::NonConstValue(value, span)),
        }
    }

    #[inline]
    fn current_inst(&self) -> &Inst {
        &self.body.block(self.current_block).insts[self.current_inst]
    }
}

pub(crate) type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub(crate) enum EvalError {
    UnsupportedInst(Span),
    NonConstValue(ValueId, Span),
}
