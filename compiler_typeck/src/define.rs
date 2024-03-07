use compiler_ast::{self as ast};
use compiler_core::{
    db::{Adt, AdtId, AdtKind, Db, Def, DefId, DefKind, ModuleId, ScopeInfo, ScopeLevel},
    diagnostics::DiagnosticResult,
    middle::{Mutability, NamePat, Pat, Vis},
    span::Spanned as _,
    ty::Ty,
    word::Word,
};
use compiler_data_structures::index_vec::{IndexVecExt as _, Key as _};

use crate::{
    errors,
    lookup::{FnCandidate, FnCandidateSet},
    ns::{AssocTy, Env, NsDef},
    Typeck,
};

impl<'db> Typeck<'db> {
    pub(crate) fn define(&mut self) -> Define<'db, '_> {
        Define::new(self)
    }
}

pub(crate) struct Define<'db, 'cx> {
    cx: &'cx mut Typeck<'db>,
}

impl<'db, 'cx> Define<'db, 'cx> {
    fn new(cx: &'cx mut Typeck<'db>) -> Self {
        Self { cx }
    }

    pub(crate) fn adt(
        &mut self,
        module_id: ModuleId,
        tydef: &ast::TyDef,
        kind: impl FnOnce(AdtId) -> AdtKind,
    ) -> AdtId {
        let adt_id = self.cx.db.adts.push_with_key(|id| Adt {
            id,
            def_id: DefId::null(),
            name: tydef.word,
            tparams: vec![],
            kind: kind(id),
        });

        let def_id = self.new_global(
            module_id,
            tydef.vis,
            DefKind::Adt(adt_id),
            tydef.word,
            Mutability::Imm,
        );
        self.cx.db[adt_id].def_id = def_id;

        adt_id
    }

    pub(crate) fn create_global(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
    ) -> DefId {
        let qpath = self.cx.db[module_id].qpath.clone().child(name.name());
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        Def::alloc(self.cx.db, qpath, scope, kind, mutability, name.span())
    }

    pub(crate) fn new_global(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
    ) -> DefId {
        let id = self.create_global(module_id, vis, kind, name, mutability);

        if let Err(diagnostic) = self.global(module_id, name, id, vis) {
            self.cx.db.diagnostics.add(diagnostic);
        }

        id
    }

    pub(crate) fn global(
        &mut self,
        module_id: ModuleId,
        name: Word,
        id: DefId,
        vis: Vis,
    ) -> DiagnosticResult<()> {
        let module = self.cx.global_env.module_mut(module_id);

        if let Some(fns) = module.ns.defined_fns.get(&name.name()) {
            let span = self.cx.db[fns[0]].span;
            return Err(errors::multiple_item_def_err(span, name));
        }

        let def = NsDef { id, name, module_id, vis };
        if let Some(prev) = module.ns.defs.insert(name.name(), def) {
            return Err(errors::multiple_item_def_err(prev.span(), name));
        }

        Ok(())
    }

    pub(crate) fn new_local(
        &mut self,
        env: &mut Env,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> DefId {
        let id = self.create_local(env, kind, name, mutability, ty);
        env.insert(name.name(), id);
        id
    }

    pub(crate) fn create_local(
        &mut self,
        env: &Env,
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
        id
    }

    pub(crate) fn global_pat(
        &mut self,
        module_id: ModuleId,
        pat: &Pat,
        kind: DefKind,
        ty: Ty,
    ) -> Pat {
        match pat {
            Pat::Name(name) => {
                let id = self.new_global(module_id, name.vis, kind, name.word, name.mutability);
                Pat::Name(NamePat { id, ty, ..name.clone() })
            }
            Pat::Discard(span) => Pat::Discard(*span),
        }
    }

    pub(crate) fn local_pat(&mut self, env: &mut Env, pat: &Pat, ty: Ty) -> Pat {
        match pat {
            Pat::Name(name) => {
                let id = self.new_local(env, DefKind::Variable, name.word, name.mutability, ty);
                Pat::Name(NamePat { id, ty, ..name.clone() })
            }
            Pat::Discard(span) => Pat::Discard(*span),
        }
    }

    pub(crate) fn fn_candidate(&mut self, candidate: FnCandidate) -> DiagnosticResult<()> {
        let module_id = candidate.module_id(self.cx.db);
        let name = candidate.word.name();
        let set = self.cx.global_env.module_mut(module_id).ns.fns.entry(name).or_default();
        Self::insert_fn_candidate_in(self.cx.db, set, candidate)
    }

    pub(crate) fn assoc_fn_candidate(
        &mut self,
        assoc_ty: AssocTy,
        candidate: FnCandidate,
    ) -> DiagnosticResult<()> {
        let name = candidate.word.name();
        let set =
            self.cx.global_env.assoc_ns.entry(assoc_ty).or_default().fns.entry(name).or_default();
        Self::insert_fn_candidate_in(self.cx.db, set, candidate)
    }

    fn insert_fn_candidate_in(
        db: &Db,
        set: &mut FnCandidateSet,
        candidate: FnCandidate,
    ) -> DiagnosticResult<()> {
        set.try_insert(candidate).map_err(|err| err.into_diagnostic(db))
    }
}
