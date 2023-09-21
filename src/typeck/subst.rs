use std::collections::HashMap;

use crate::{
    db::Db,
    hir::Hir,
    typeck::{
        error::TypeckError,
        normalize::NormalizeTy,
        tcx::{TyCx, TyStorage},
    },
    span::Span,
    subst::{Subst, SubstTy},
    ty::{fold::TyFolder, InferTy, Ty, TyKind},
};

impl<'db> TyCx<'db> {
    pub fn subst(&mut self, hir: &mut Hir) {
        let mut cx =
            SubstCtxt { db: self.db, tcx: &mut self.storage.borrow_mut(), errs: HashMap::new() };

        for f in &mut hir.fns {
            f.subst(&mut cx);
        }

        for let_ in &mut hir.lets {
            let_.subst(&mut cx);
        }

        let diagnostics: Vec<_> =
            cx.errs.into_values().map(|e| e.into_diagnostic(self.db)).collect();

        self.db.diagnostics.emit_many(diagnostics);
    }
}

struct SubstCtxt<'db> {
    db: &'db mut Db,
    tcx: &'db mut TyStorage,
    errs: HashMap<Span, TypeckError>,
}

impl SubstTy for SubstCtxt<'_> {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut folder = VarFolder { cx: self, has_unbound_vars: false };
        let ty = folder.fold(ty);

        if folder.has_unbound_vars {
            folder
                .cx
                .errs
                .insert(span, TypeckError::CannotInfer { ty: ty.normalize(folder.cx.tcx), span });
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
                let root = self.cx.tcx.ty_unification_table.find(*var);

                if let Some(ty) = self.cx.tcx.ty_unification_table.probe_value(root) {
                    self.fold(ty)
                } else {
                    self.has_unbound_vars = true;
                    TyKind::Infer(InferTy::TyVar(*var)).into()
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                let root = self.cx.tcx.int_unification_table.find(*var);

                self.cx
                    .tcx
                    .int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
                    .into()
            }
            _ => self.super_fold(ty),
        }
    }
}
