use crate::{
    hir::{Expr, ExprKind, Fn, FnKind, FnSig, Let},
    span::Spanned,
    subst,
    subst::{Subst, SubstTy},
};

impl<S: SubstTy> Subst<S> for Expr {
    fn subst(&mut self, s: &mut S) {
        match &mut self.kind {
            ExprKind::Let(let_) => let_.subst(s),
            ExprKind::Assign(assign) => {
                assign.lhs.subst(s);
                assign.rhs.subst(s);
            }
            ExprKind::If(if_) => {
                if_.cond.subst(s);
                if_.then.subst(s);
                if_.otherwise.subst(s);
            }
            ExprKind::Loop(loop_) => {
                loop_.expr.subst(s);
            }
            ExprKind::Block(blk) => {
                for stmt in &mut blk.exprs {
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
            ExprKind::Name(name) => {
                subst::subst_instantation(
                    s,
                    &mut name.instantiation,
                    self.span,
                );
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

        let ty = {
            let ty = s.db()[self.def_id].ty;
            s.subst_ty(ty, self.span)
        };

        s.db()[self.def_id].ty = ty;
    }
}

impl<S: SubstTy> Subst<S> for FnSig {
    fn subst(&mut self, s: &mut S) {
        for param in &mut self.params {
            param.ty = s.subst_ty(param.ty, param.pat.span());
            // TODO: remove?
            param.pat.subst(s);
        }
    }
}

impl<S: SubstTy> Subst<S> for Let {
    fn subst(&mut self, s: &mut S) {
        self.value.subst(s);
        self.ty = self.value.ty;
        // TODO: remove?
        self.pat.subst(s);
    }
}
