use crate::{
    db::{Db, DefId},
    diagnostics::{Diagnostic, Label},
    hir::{FnSig, Hir},
    middle::{Pat, Vis},
    span::{Span, Spanned},
    ty::Ty,
};

pub fn leaky_items(db: &mut Db, hir: &Hir) {
    LeakyItems { db }.run(hir);
}

struct LeakyItems<'db> {
    db: &'db mut Db,
}

impl LeakyItems<'_> {
    fn run(&mut self, hir: &Hir) {
        for f in &hir.fns {
            if self.db[f.def_id].scope.vis == Vis::Public {
                self.sig(&f.sig);
            }
        }

        for let_ in &hir.lets {
            match &let_.pat {
                Pat::Name(name) => self.def(name.id, name.ty),
                Pat::Discard(_) => (),
            }
        }

        for let_ in &hir.extern_lets {
            self.def(let_.id, let_.ty);
        }
    }

    fn sig(&mut self, sig: &FnSig) {
        for param in &sig.params {
            self.ty(param.ty, param.pat.span(), "function parameter");
        }

        self.ty(sig.ret, sig.ret_span, "return type");
    }

    fn def(&mut self, id: DefId, ty: Ty) {
        let def = &self.db[id];

        if def.scope.vis == Vis::Public {
            self.ty(ty, def.span, &format!("`{}`", def.name));
        }
    }

    fn ty(&mut self, ty: Ty, span: Span, item_kind: &str) {
        if let Some(priv_ty) = ty.has_private_ty(self.db) {
            self.db.diagnostics.emit(
                Diagnostic::error(format!(
                    "private type `{}` used in public interface",
                    priv_ty.display(self.db)
                ))
                .with_label(Label::primary(
                    span,
                    format!("{item_kind} uses private type `{}`", ty.display(self.db)),
                )),
            );
        }
    }
}
