use data_structures::index_vec::{IndexVecExt as _, Key as _};

use crate::{
    ast,
    db::{Adt, AdtId, AdtKind, Db, Def, DefId, DefKind, ModuleId, ScopeInfo, ScopeLevel},
    diagnostics::DiagnosticResult,
    middle::{Mutability, NamePat, Pat, Vis},
    span::Spanned as _,
    ty::Ty,
    typeck2::{
        errors,
        lookup::{FnCandidate, FnCandidateInsertError, FnCandidateSet},
        ns::{Env, NsDef},
        Typeck,
    },
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

        let def_id = self.new_global(
            module_id,
            tydef.vis,
            DefKind::Adt(adt_id),
            tydef.word,
            Mutability::Imm,
        )?;
        self.cx.db[adt_id].def_id = def_id;

        Ok((adt_id, def_id))
    }

    pub(super) fn new_global(
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
        self.global(module_id, name, id, vis)
    }

    pub(super) fn global(
        &mut self,
        module_id: ModuleId,
        name: Word,
        id: DefId,
        vis: Vis,
    ) -> DiagnosticResult<DefId> {
        // TODO:
        // if let Some(candidates) = self.global_scope.fns.get(&symbol) {
        //     let last_candidate = candidates.iter().last().unwrap();
        //     return Err(errors::multiple_item_def_err(last_candidate.word.span(),
        // name)); }

        let def = NsDef::new(id, vis, name.span());

        if let Some(prev) =
            self.cx.global_env.module_mut(module_id).ns.defs.insert(name.name(), def)
        {
            return Err(errors::multiple_item_def_err(prev.span, name));
        }

        Ok(id)
    }

    pub(super) fn new_local(
        &mut self,
        env: &mut Env,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> DefId {
        let qpath = env.scope_path(self.cx.db).child(name.name());
        let scope =
            ScopeInfo { module_id: env.module_id(), level: env.scope_level(), vis: Vis::Private };
        let id = Def::alloc(self.cx.db, qpath, scope, kind, mutability, name.span());
        self.cx.def_to_ty.insert(id, ty);
        env.insert(name.name(), id);
        id
    }

    pub(super) fn global_pat(
        &mut self,
        module_id: ModuleId,
        pat: &Pat,
        ty: Ty,
    ) -> DiagnosticResult<Pat> {
        match pat {
            Pat::Name(name) => {
                let id = self.new_global(
                    module_id,
                    name.vis,
                    DefKind::Global,
                    name.word,
                    name.mutability,
                )?;

                Ok(Pat::Name(NamePat { id, ty, ..name.clone() }))
            }
            Pat::Discard(span) => Ok(Pat::Discard(*span)),
        }
    }

    pub(super) fn local_pat(&mut self, env: &mut Env, pat: &Pat, ty: Ty) -> Pat {
        match pat {
            Pat::Name(name) => {
                let id = self.new_local(env, DefKind::Variable, name.word, name.mutability, ty);
                Pat::Name(NamePat { id, ty, ..name.clone() })
            }
            Pat::Discard(span) => Pat::Discard(*span),
        }
    }

    pub(super) fn fn_candidate(&mut self, candidate: FnCandidate) -> DiagnosticResult<()> {
        let module_id = candidate.module_id(self.cx.db);
        let name = candidate.word.name();
        let set = self.cx.global_env.module_mut(module_id).ns.fns.entry(name).or_default();
        Self::insert_fn_candidate_in(self.cx.db, set, candidate)
    }

    fn insert_fn_candidate_in(
        db: &Db,
        set: &mut FnCandidateSet,
        candidate: FnCandidate,
    ) -> DiagnosticResult<()> {
        set.try_insert(candidate).map_err(|err| err.to_diagnostic(db))
    }
}
