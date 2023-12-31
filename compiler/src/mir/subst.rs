use crate::{
    mir::{
        Block, Body, Fn, FnSig, Global, GlobalKind, Inst, Mir, StaticGlobal,
        Value,
    },
    span::{Span, Spanned},
    subst,
    subst::{Subst, SubstTy},
};

impl<S: SubstTy> Subst<S> for Mir {
    fn subst(&mut self, s: &mut S) {
        self.fn_sigs.values_mut().for_each(|f| f.subst(s));
        self.fns.values_mut().for_each(|f| f.subst(s));
        self.globals.values_mut().for_each(|g| g.subst(s));
    }
}

impl<S: SubstTy> Subst<S> for FnSig {
    fn subst(&mut self, s: &mut S) {
        self.ty = s.subst_ty(self.ty, self.span);

        for p in &mut self.params {
            p.ty = s.subst_ty(p.ty, p.pat.span());
        }
    }
}

impl<S: SubstTy> Subst<S> for Fn {
    fn subst(&mut self, s: &mut S) {
        self.body.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for Global {
    fn subst(&mut self, s: &mut S) {
        match &mut self.kind {
            GlobalKind::Static(StaticGlobal { body, result: _ }) => {
                body.subst(s);
            }
            GlobalKind::Extern => (),
        }
    }
}

impl<S: SubstTy> Subst<S> for Body {
    fn subst(&mut self, s: &mut S) {
        self.blocks_mut().into_iter().for_each(|b| b.subst(s));
        self.values_mut().into_iter().for_each(|v| v.subst(s));
        self.instantations_mut().iter_mut().for_each(|(_, i)| {
            subst::subst_instantation(s, i, Span::unknown());
        });
    }
}

impl<S: SubstTy> Subst<S> for Block {
    fn subst(&mut self, s: &mut S) {
        self.insts.iter_mut().for_each(|i| i.subst(s));
    }
}

impl<S: SubstTy> Subst<S> for Inst {
    fn subst(&mut self, s: &mut S) {
        if let Inst::Cast { target, span, .. } = self {
            *target = s.subst_ty(*target, *span);
        }
    }
}
impl<S: SubstTy> Subst<S> for Value {
    fn subst(&mut self, s: &mut S) {
        self.ty = s.subst_ty(self.ty, Span::unknown());
    }
}
