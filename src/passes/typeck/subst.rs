use std::collections::HashMap;

use crate::{
    db::Db,
    hir::Fn,
    passes::{
        subst::{Subst, SubstTy},
        typeck::{
            error::InferError,
            infcx::{InferCtxt, InferCtxtStorage},
            normalize::NormalizeTy,
        },
    },
    span::Span,
    ty::{fold::TyFolder, InferTy, Ty, TyKind},
};

impl<'db> InferCtxt<'db> {
    pub fn subst_fn(&mut self, f: &mut Fn) {
        let mut cx =
            SubstCtxt { db: self.db, infcx: &mut self.storage.borrow_mut(), errs: HashMap::new() };

        f.subst(&mut cx);

        let diagnostics: Vec<_> =
            cx.errs.into_values().map(|e| e.into_diagnostic(self.db)).collect();
        self.db.diagnostics.emit_many(diagnostics);
    }
}

struct SubstCtxt<'db> {
    db: &'db mut Db,
    infcx: &'db mut InferCtxtStorage,
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

    fn db(&mut self) -> &mut Db {
        self.db
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
