use data_structures::index_vec::{IndexVecExt as _, Key as _};
use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    ast,
    ast::Ast,
    db::{Adt, AdtId, AdtKind, Db, Def, DefId, DefKind, ModuleId, ScopeInfo, ScopeLevel},
    diagnostics::DiagnosticResult,
    hir::Hir,
    middle::{Mutability, Vis},
    qpath::QPath,
    span::{Span, Spanned as _},
    sym,
    ty::{Ty, TyKind},
    typeck2::Typeck,
    word::Word,
};

impl<'db> Typeck<'db> {
    pub(super) fn define(&mut self) -> Define<'db, '_> {
        Define::new(self)
    }
}

pub(super) struct Define<'db, 'cx> {
    cx: &'cx mut Typeck<'db>,
}

impl<'db, 'cx> Define<'db, 'cx> {
    fn new(cx: &'cx mut Typeck<'db>) -> Self {
        Self { cx }
    }

    pub(super) fn adt(
        &mut self,
        module_id: ModuleId,
        tydef: &ast::TyDef,
        kind: impl FnOnce(AdtId) -> AdtKind,
    ) -> DiagnosticResult<(AdtId, DefId)> {
        let adt_id = self.cx.db.adts.push_with_key(|id| Adt {
            id,
            def_id: DefId::null(),
            name: tydef.word,
            ty_params: vec![],
            kind: kind(id),
        });

        let def_id =
            self.global(module_id, tydef.vis, DefKind::Adt(adt_id), tydef.word, Mutability::Imm)?;
        self.cx.db[adt_id].def_id = def_id;

        Ok((adt_id, def_id))
    }

    pub(super) fn global(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
    ) -> DiagnosticResult<DefId> {
        let qpath = self.cx.db[module_id].qpath.clone().child(name.name());
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let id = Def::alloc(self.cx.db, qpath, scope, kind, mutability, name.span());
        self.global_def(module_id, name, id, vis)
    }

    pub(super) fn global_def(
        &mut self,
        module_id: ModuleId,
        name: Word,
        id: DefId,
        vis: Vis,
    ) -> DiagnosticResult<DefId> {
        todo!("insert global");
        // let symbol = Symbol::new(module_id, name.name());

        // if let Some(candidates) = self.global_scope.fns.get(&symbol) {
        //     let last_candidate = candidates.iter().last().unwrap();
        //     return Err(errors::multiple_item_def_err(last_candidate.word.span(),
        // name)); }

        // let new_def = GlobalScopeDef::new(id, vis, name.span());

        // if let Some(prev) = self.global_scope.insert_def(symbol, new_def) {
        //     return Err(errors::multiple_item_def_err(prev.span, name));
        // }

        Ok(id)
    }
}
