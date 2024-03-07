use crate::{lower::Lower, Body, Const};

impl<'db> Lower<'db> {
    pub(crate) fn eval(&mut self, body: &Body) -> Const {
        todo!()
        // Eval::new(self, body)
    }
}

pub(super) struct Eval<'cx, 'db> {
    pub(super) cx: &'cx mut Lower<'db>,
    pub(super) body: &'cx Body,
}

impl<'cx, 'db> Eval<'cx, 'db> {
    pub(super) fn new(cx: &'cx mut Lower<'db>, body: &'cx Body) -> Self {
        Self { cx, body }
    }
}
