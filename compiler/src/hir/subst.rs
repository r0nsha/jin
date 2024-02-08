use crate::{
    hir::{
        Assign, Binary, Expr, ExprKind, ExternLet, Fn, FnKind, FnSig, Let, MatchArm, MatchPat,
        Name, Swap, Variant,
    },
    span::Spanned,
    subst,
    subst::{Subst, SubstTy},
};

impl<S: SubstTy> Subst<S> for Expr {
    fn subst(&mut self, s: &mut S) {
        match &mut self.kind {
            ExprKind::Let(let_) => let_.subst(s),
            ExprKind::Assign(Assign { lhs, rhs, .. })
            | ExprKind::Swap(Swap { lhs, rhs })
            | ExprKind::Binary(Binary { lhs, rhs, .. }) => {
                lhs.subst(s);
                rhs.subst(s);
            }
            ExprKind::Match(match_) => {
                match_.expr.subst(s);
                match_.arms.subst(s);
            }
            ExprKind::Loop(loop_) => {
                loop_.cond.subst(s);
                loop_.expr.subst(s);
            }
            ExprKind::Block(block) => {
                block.exprs.subst(s);
            }
            ExprKind::Unsafe(uns) => {
                uns.expr.subst(s);
            }
            ExprKind::Return(ret) => {
                ret.expr.subst(s);
            }
            ExprKind::Call(call) => {
                call.callee.subst(s);
                for arg in &mut call.args {
                    arg.expr.subst(s);
                }
            }
            ExprKind::Unary(un) => {
                un.expr.subst(s);
            }
            ExprKind::Deref(deref) => {
                deref.expr.subst(s);
            }
            ExprKind::Cast(cast) => {
                cast.expr.subst(s);
                cast.target = s.subst_ty(cast.target, self.span);
            }
            ExprKind::Transmute(trans) => {
                trans.expr.subst(s);
                trans.target = s.subst_ty(trans.target, self.span);
            }
            ExprKind::Field(access) => {
                access.expr.subst(s);
            }
            ExprKind::Index(index) => {
                index.expr.subst(s);
                index.index.subst(s);
            }
            ExprKind::Slice(slice) => {
                slice.expr.subst(s);
                slice.low.subst(s);
                slice.high.subst(s);
            }
            ExprKind::Name(Name { instantiation, .. })
            | ExprKind::Variant(Variant { instantiation, .. }) => {
                subst::subst_instantation(s, instantiation, self.span);
            }
            ExprKind::SliceLit(lit) => {
                lit.exprs.subst(s);
                lit.cap.subst(s);
            }
            ExprKind::Break
            | ExprKind::BoolLit(_)
            | ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::StrLit(_) => (),
        }

        self.ty = s.subst_ty(self.ty, self.span);
    }
}

impl<S: SubstTy> Subst<S> for Option<Box<Expr>> {
    fn subst(&mut self, s: &mut S) {
        if let Some(this) = self {
            this.subst(s);
        }
    }
}

impl<S: SubstTy, U: Subst<S>> Subst<S> for Vec<U> {
    fn subst(&mut self, s: &mut S) {
        for x in self {
            x.subst(s);
        }
    }
}

impl<S: SubstTy> Subst<S> for Fn {
    fn subst(&mut self, s: &mut S) {
        self.sig.subst(s);

        match &mut self.kind {
            FnKind::Bare { body } => body.subst(s),
            FnKind::Extern { .. } => (),
        }
    }
}

impl<S: SubstTy> Subst<S> for FnSig {
    fn subst(&mut self, s: &mut S) {
        for param in &mut self.params {
            param.ty = s.subst_ty(param.ty, param.pat.span());
            param.pat.subst(s);
        }

        self.ret = s.subst_ty(self.ret, self.ret_span);
        self.ty = s.subst_ty(self.ty, self.ret_span);
    }
}

impl<S: SubstTy> Subst<S> for Let {
    fn subst(&mut self, s: &mut S) {
        self.value.subst(s);
        self.ty = self.value.ty;
        self.pat.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for ExternLet {
    fn subst(&mut self, s: &mut S) {
        self.ty = s.subst_ty(self.ty, self.word.span());
    }
}

impl<S: SubstTy> Subst<S> for MatchArm {
    fn subst(&mut self, s: &mut S) {
        self.pat.subst(s);
        self.guard.subst(s);
        self.expr.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for MatchPat {
    fn subst(&mut self, s: &mut S) {
        match self {
            MatchPat::Name(_, ty, span) => {
                *ty = s.subst_ty(*ty, *span);
            }
            MatchPat::Adt(_, pats, _) | MatchPat::Variant(_, pats, _) | MatchPat::Or(pats, _) => {
                pats.subst(s);
            }
            MatchPat::Wildcard(_)
            | MatchPat::Unit(_)
            | MatchPat::Bool(_, _)
            | MatchPat::Int(_, _)
            | MatchPat::Str(_, _) => (),
        }
    }
}
