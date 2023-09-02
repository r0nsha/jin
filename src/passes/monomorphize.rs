use ustr::ustr;

use crate::{
    db::{Db, Def, DefId},
    hir::{Fn, Hir, Item, ItemKind},
    passes::subst::SubstTy,
    span::Span,
    ty::{fold::TyFolder, Ty, TyKind},
};

#[derive(Debug)]
pub struct MonoItem {
    pub args: Vec<Ty>,
    pub source_id: DefId,
    pub target_id: DefId,
}

pub fn monomorphize(db: &mut Db, hir: &Hir) -> Vec<MonoItem> {
    Collector::new(db).collect(hir).mono_items
}

struct Collector<'db> {
    db: &'db mut Db,
    mono_items: Vec<MonoItem>,
}

impl<'db> Collector<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, mono_items: vec![] }
    }

    fn collect(mut self, hir: &Hir) -> Self {
        for item in &hir.items {
            self.collect_root_item(item);
        }

        self
    }

    fn collect_root_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Fn(fun) => {
                if fun.ty.is_polymorphic() {
                    self.collect_fn(fun);
                }
            }
        }
    }

    fn collect_fn(&mut self, fun: &Fn) {
        for param in &fun.sig.params {
            self.collect_mono_def(param.id, &[]);
        }
    }

    fn collect_mono_def(&mut self, source_id: DefId, args: &[Ty]) -> DefId {
        if let Some(item) =
            self.mono_items.iter().find(|item| item.source_id == source_id && item.args == args)
        {
            item.target_id
        } else {
            let target_id = self.alloc_mono_def(source_id, args);
            self.mono_items.push(MonoItem { args: args.to_owned(), source_id, target_id });
            target_id
        }
    }

    fn alloc_mono_def(&mut self, id: DefId, args: &[Ty]) -> DefId {
        let def = &self.db[id];

        let new_qpath = if def.kind.is_fn() {
            let args_str =
                args.iter().map(|t| t.to_string(self.db)).collect::<Vec<String>>().join("_");
            def.qpath.clone().with_name(ustr(&format!("{}${}", def.name, args_str)))
        } else {
            def.qpath.clone()
        };

        let new_scope = def.scope.clone();
        let new_kind = def.kind.as_ref().clone();
        let new_span = def.span;

        let ty = def.ty;
        let new_ty = ParamFolder { db: self.db, args }.fold(ty);

        Def::alloc(self.db, new_qpath, new_scope, new_kind, new_ty, new_span)
    }
}

pub struct ParamFolder<'db, 'a> {
    db: &'db mut Db,
    args: &'a [Ty],
}

impl SubstTy for ParamFolder<'_, '_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }

    fn db(&mut self) -> &mut Db {
        self.db
    }
}

impl TyFolder for ParamFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => self.args[p.index],
            _ => self.super_fold(ty),
        }
    }
}
