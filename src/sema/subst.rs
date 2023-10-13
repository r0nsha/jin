use rustc_hash::FxHashMap;

use crate::{
    db::Db,
    sema::{error::CheckError, normalize::NormalizeTy, Sema, TyStorage},
    span::Span,
    subst::{Subst, SubstTy},
    ty::{fold::TyFolder, InferTy, Ty, TyKind},
};

impl<'db> Sema<'db> {
    pub fn subst(&mut self) {
        let mut cx = SubstCx {
            db: self.db,
            storage: &mut self.storage.borrow_mut(),
            errors: FxHashMap::default(),
        };

        for f in &mut self.hir.fns {
            f.subst(&mut cx);
        }

        for let_ in &mut self.hir.lets {
            let_.subst(&mut cx);
        }

        let diagnostics: Vec<_> =
            cx.errors.into_values().map(|e| e.into_diagnostic(self.db)).collect();

        self.db.diagnostics.emit_many(diagnostics);
    }
}

struct SubstCx<'db> {
    db: &'db mut Db,
    storage: &'db mut TyStorage,
    errors: FxHashMap<Span, CheckError>,
}

impl SubstTy for SubstCx<'_> {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut folder = VarFolder { cx: self, has_unbound_vars: false };
        let ty = folder.fold(ty);

        if folder.has_unbound_vars {
            folder.cx.errors.insert(
                span,
                CheckError::CannotInfer { ty: ty.normalize(folder.cx.storage), span },
            );
        }

        ty
    }

    fn db(&mut self) -> &mut Db {
        self.db
    }
}

struct VarFolder<'db, 'a> {
    cx: &'a mut SubstCx<'db>,
    has_unbound_vars: bool,
}

impl TyFolder for VarFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Infer(InferTy::TyVar(var)) => {
                let root = self.cx.storage.ty_unification_table.find(*var);

                if let Some(ty) = self.cx.storage.ty_unification_table.probe_value(root) {
                    self.fold(ty)
                } else {
                    self.has_unbound_vars = true;
                    TyKind::Infer(InferTy::TyVar(*var)).into()
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                let root = self.cx.storage.int_unification_table.find(*var);

                self.cx
                    .storage
                    .int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
                    .into()
            }
            _ => self.super_fold(ty),
        }
    }
}
