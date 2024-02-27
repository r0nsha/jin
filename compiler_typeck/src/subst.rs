use compiler_core::{
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::{fold::TyFolder, FloatVar, InferTy, IntVar, Subst, SubstTy, Ty, TyKind, TyVar},
};
use rustc_hash::FxHashMap;

use crate::{TyStorage, Typeck};

pub(crate) fn subst(cx: &mut Typeck<'_>) {
    let mut scx =
        SubstCx { storage: &mut cx.storage.borrow_mut(), unbound_tys: FxHashMap::default() };

    for f in &mut cx.hir.fns {
        f.subst(&mut scx);
    }

    for let_ in &mut cx.hir.lets {
        let_.subst(&mut scx);
    }

    for let_ in &mut cx.hir.extern_lets {
        let_.subst(&mut scx);
    }

    for coercions in cx.hir.coercions.values_mut() {
        for coercion in coercions.iter_mut() {
            coercion.target = scx.subst_ty(coercion.target, Span::unknown());
        }
    }

    let diagnostics: Vec<_> = scx
        .unbound_tys
        .into_iter()
        .map(|(span, ty)| {
            Diagnostic::error(format!("type annotations needed for `{}`", ty.display(cx.db)))
                .with_label(Label::primary(span, "cannot infer type"))
        })
        .collect();

    cx.db.diagnostics.extend(diagnostics);
}

struct SubstCx<'db> {
    storage: &'db mut TyStorage,
    unbound_tys: FxHashMap<Span, Ty>,
}

impl SubstTy for SubstCx<'_> {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut folder = VarFolder { cx: self, has_unbound_vars: false };
        let ty = folder.fold(ty);

        if folder.has_unbound_vars && !self.unbound_tys.contains_key(&span) {
            self.unbound_tys.insert(span, ty);
        }

        ty
    }
}

struct VarFolder<'db, 'a> {
    cx: &'a mut SubstCx<'db>,
    has_unbound_vars: bool,
}

impl VarFolder<'_, '_> {
    fn fold_tyvar(&mut self, var: TyVar) -> Ty {
        let root = self.cx.storage.ty.find(var);

        if let Some(ty) = self.cx.storage.ty.probe_value(root) {
            self.fold(ty)
        } else {
            self.has_unbound_vars = true;
            TyKind::Infer(InferTy::Ty(var)).into()
        }
    }

    fn fold_intvar(&mut self, var: IntVar) -> Ty {
        let root = self.cx.storage.int.find(var);
        self.cx.storage.int.probe_value(root).map_or_else(|| TyKind::DEFAULT_INT, Into::into).into()
    }

    fn fold_floatvar(&mut self, var: FloatVar) -> Ty {
        let root = self.cx.storage.float.find(var);

        self.cx
            .storage
            .float
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
