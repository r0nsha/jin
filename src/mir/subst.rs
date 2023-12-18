use crate::{
    mir::{Body, Fn, FnSig, Mir},
    span::Spanned,
    subst::{Subst, SubstTy},
};

impl<S: SubstTy> Subst<S> for Mir {
    fn subst(&mut self, s: &mut S) {
        self.fn_sigs.iter_mut().for_each(|f| f.subst(s));
        self.fns.iter_mut().for_each(|f| f.subst(s));
    }
}

impl<S: SubstTy> Subst<S> for FnSig {
    fn subst(&mut self, s: &mut S) {
        // self.ty = s.subst_ty(self.ty, self.span);
    }
}

impl<S: SubstTy> Subst<S> for Fn {
    fn subst(&mut self, s: &mut S) {
        self.body.subst(s);
        // self.ty = s.subst_ty(self.ty, self.span);
    }
}

impl<S: SubstTy> Subst<S> for Body {
    fn subst(&mut self, s: &mut S) {
        // self.blocks_mut().into_iter().for_each(|b| b.subst(s));
        // self.values_mut().into_iter().for_each(|v| v.subst(s));
    }
}
