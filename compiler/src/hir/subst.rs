use crate::{
    hir::{
        Assign, Expr, ExprKind, Fn, FnKind, FnSig, Let, MatchArm, MatchPat,
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
            | ExprKind::Swap(Swap { lhs, rhs }) => {
                lhs.subst(s);
                rhs.subst(s);
            }
            ExprKind::Match(match_) => {
                match_.expr.subst(s);

                for arm in &mut match_.arms {
                    arm.subst(s);
                }
            }
            ExprKind::Loop(loop_) => {
                loop_.expr.subst(s);
            }
            ExprKind::Block(block) => {
                for stmt in &mut block.exprs {
                    stmt.subst(s);
                }
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
            ExprKind::Binary(bin) => {
                bin.lhs.subst(s);
                bin.rhs.subst(s);
            }
            ExprKind::Cast(cast) => {
                cast.expr.subst(s);
                cast.target = s.subst_ty(cast.target, self.span);
            }
            ExprKind::Field(access) => {
                access.expr.subst(s);
            }
            ExprKind::Name(Name { instantiation, .. })
            | ExprKind::Variant(Variant { instantiation, .. }) => {
                subst::subst_instantation(s, instantiation, self.span);
            }
            ExprKind::Break | ExprKind::Lit(_) => (),
        }

        self.ty = s.subst_ty(self.ty, self.span);
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
    }
}

impl<S: SubstTy> Subst<S> for Let {
    fn subst(&mut self, s: &mut S) {
        self.value.subst(s);
        self.ty = self.value.ty;
        self.pat.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for MatchArm {
    fn subst(&mut self, s: &mut S) {
        self.pat.subst(s);

        if let Some(guard) = &mut self.guard {
            guard.subst(s);
        }

        self.expr.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for MatchPat {
    fn subst(&mut self, s: &mut S) {
        match self {
            MatchPat::Name(_, ty, span) => {
                *ty = s.subst_ty(*ty, *span);
            }
            MatchPat::Adt(_, pats, _)
            | MatchPat::Variant(_, pats, _)
            | MatchPat::Or(pats, _) => {
                for pat in pats {
                    pat.subst(s);
                }
            }
            MatchPat::Wildcard(_)
            | MatchPat::Unit(_)
            | MatchPat::Bool(_, _)
            | MatchPat::Int(_, _)
            | MatchPat::Str(_, _) => (),
        }
    }
}
