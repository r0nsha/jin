use std::collections::HashMap;

use crate::{
    hir::Hir,
    passes::{
        subst::{Subst, SubstTy},
        typeck::{
            error::InferError,
            infcx::{InferCtxt, InferCtxtInner},
            normalize::NormalizeTy,
        },
    },
    span::Span,
    ty::{fold::TyFolder, InferTy, Ty, TyKind},
};

impl<'db> InferCtxt<'db> {
    pub fn subst(&mut self, hir: &mut Hir) {
        let mut cx = SubstCtxt { infcx: &mut self.inner.borrow_mut(), errs: HashMap::new() };

        for def in self.db.defs.iter_mut() {
            def.ty = cx.subst_ty(def.ty, def.span);
        }

        for item in &mut hir.items {
            item.subst(&mut cx);
        }

        let diagnostics: Vec<_> =
            cx.errs.into_values().map(|e| e.into_diagnostic(self.db)).collect();
        self.db.diagnostics.extend(diagnostics);
    }
}

struct SubstCtxt<'db> {
    infcx: &'db mut InferCtxtInner,
    errs: HashMap<Span, InferError>,
}

impl SubstTy for SubstCtxt<'_> {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut folder = VarFolder { cx: self, has_unbound_vars: false };
        let ty = folder.fold(ty);

        if folder.has_unbound_vars {
            folder
                .cx
                .errs
                .insert(span, InferError::CannotInfer { ty: ty.normalize(folder.cx.infcx), span });
        }

        ty
    }
}

struct VarFolder<'db, 'a> {
    cx: &'a mut SubstCtxt<'db>,
    has_unbound_vars: bool,
}

impl TyFolder for VarFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Infer(InferTy::TyVar(var)) => {
                let root = self.cx.infcx.ty_unification_table.find(*var);

                if let Some(ty) = self.cx.infcx.ty_unification_table.probe_value(root) {
                    self.fold(ty)
                } else {
                    self.has_unbound_vars = true;
                    TyKind::Infer(InferTy::TyVar(*var)).into()
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                let root = self.cx.infcx.int_unification_table.find(*var);

                self.cx
                    .infcx
                    .int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
                    .into()
            }
            _ => self.super_fold(ty),
        }
    }
}
