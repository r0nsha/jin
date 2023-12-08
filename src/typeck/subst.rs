use rustc_hash::FxHashMap;

use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    typeck::{normalize::NormalizeTy, Typeck, TyStorage},
    span::Span,
    subst::{Subst, SubstTy},
    ty::{fold::TyFolder, FloatVar, InferTy, IntVar, Ty, TyKind, TyVar},
};

impl<'db> Typeck<'db> {
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

        let diagnostics: Vec<_> = cx.errors.into_values().collect();

        self.db.diagnostics.emit_many(diagnostics);

        self.db
            .emit_file(crate::db::build_options::EmitOption::Hir, |db, file| {
                self.hir.pretty_print(db, file)
            })
            .expect("emitting hir failed");
    }
}

struct SubstCx<'db> {
    db: &'db mut Db,
    storage: &'db mut TyStorage,
    errors: FxHashMap<Span, Diagnostic>,
}

impl SubstTy for SubstCx<'_> {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut folder = VarFolder { cx: self, has_unbound_vars: false };
        let ty = folder.fold(ty);

        if folder.has_unbound_vars {
            self.errors.entry(span).or_insert_with(|| {
                let ty = ty.normalize(self.storage);
                Diagnostic::error()
                    .with_message(format!("type annotations needed for `{}`", ty.display(self.db)))
                    .with_label(Label::primary(span).with_message("cannot infer type"))
            });
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

impl VarFolder<'_, '_> {
    fn fold_tyvar(&mut self, var: TyVar) -> Ty {
        let root = self.cx.storage.ty_unification_table.find(var);

        if let Some(ty) = self.cx.storage.ty_unification_table.probe_value(root) {
            self.fold(ty)
        } else {
            self.has_unbound_vars = true;
            TyKind::Infer(InferTy::Ty(var)).into()
        }
    }

    fn fold_intvar(&mut self, var: IntVar) -> Ty {
        let root = self.cx.storage.int_unification_table.find(var);

        self.cx
            .storage
            .int_unification_table
            .probe_value(root)
            .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
            .into()
    }

    fn fold_floatvar(&mut self, var: FloatVar) -> Ty {
        let root = self.cx.storage.float_unification_table.find(var);

        self.cx
            .storage
            .float_unification_table
            .probe_value(root)
            .map_or_else(|| TyKind::DEFAULT_FLOAT, Into::into)
            .into()
    }
}

impl TyFolder for VarFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            // TyKind::Param(p) => TyKind::Param(ParamTy {
            //     name: p.name,
            //     var: self.cx.storage.ty_unification_table.find(p.var),
            // })
            // .into(),
            TyKind::Infer(InferTy::Ty(var)) => self.fold_tyvar(*var),
            TyKind::Infer(InferTy::Int(var)) => self.fold_intvar(*var),
            TyKind::Infer(InferTy::Float(var)) => self.fold_floatvar(*var),
            _ => self.super_fold(ty),
        }
    }
}