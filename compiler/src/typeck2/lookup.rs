use std::iter;

use data_structures::index_vec::{IndexVecExt as _, Key as _};
use ustr::Ustr;

use crate::{
    ast,
    db::{Adt, AdtId, AdtKind, Def, DefId, DefKind, ModuleId, ScopeInfo, ScopeLevel, VariantId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    macros::create_bool_enum,
    middle::{IsUfcs, Mutability, Vis},
    span::{Span, Spanned as _},
    typeck2::{errors, ns::NsDef, FnCandidate, Query, Typeck},
    word::Word,
};

impl<'db> Typeck<'db> {
    pub(super) fn lookup(&self) -> Lookup<'db, '_> {
        Lookup::new(self)
    }

    pub(super) fn insert_import_lookup_results(
        &mut self,
        in_module: ModuleId,
        name: Word,
        vis: Vis,
        results: Vec<LookupResult>,
    ) -> DiagnosticResult<()> {
        for res in results {
            match res {
                LookupResult::Def(id) => {
                    self.define().global(in_module, name, id, vis)?;
                }
                LookupResult::Fn(candidate) => {
                    todo!()
                    // self.insert_fn_candidate(Symbol::new(env.module_id(),
                    // name.name()), candidate)?;
                }
            }
        }

        Ok(())
    }
}

pub(super) struct Lookup<'db, 'cx> {
    cx: &'cx Typeck<'db>,
}

impl<'db, 'cx> Lookup<'db, 'cx> {
    fn new(cx: &'cx Typeck<'db>) -> Self {
        Self { cx }
    }

    pub(super) fn query(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        query: &Query,
    ) -> DiagnosticResult<DefId> {
        let id = self.query_inner(from_module, in_module, query)?;
        self.check_def_access(from_module, id, query.span())?;
        Ok(id)
    }

    fn query_inner(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        query: &Query,
    ) -> DiagnosticResult<DefId> {
        let name = query.name();

        if let Query::Fn(fn_query) = query {
            todo!()
            // if let Some(id) = self.lookup_fn_candidate(from_module,
            // in_module, fn_query)? {     return Ok(id);
            // }
        }

        // We should only lookup functions if we didn't already query for a function
        let should_lookup_fns = ShouldLookupFns::from(!matches!(query, Query::Fn(_)));

        // Allow looking up builtin types only when looking up a symbol in the same
        // module as its environment's module.
        let allow_builtin_tys = AllowBuiltinTys::from(from_module == in_module);

        self.one(from_module, in_module, name, query.span(), should_lookup_fns, allow_builtin_tys)?
            .ok_or_else(|| match query {
                Query::Name(word) => {
                    errors::name_not_found(self.cx.db, from_module, in_module, *word)
                }
                Query::Fn(fn_query) => errors::fn_not_found(self.cx.db, fn_query),
            })
    }

    pub(super) fn import(
        &mut self,
        from_module: ModuleId,
        in_module: ModuleId,
        word: Word,
    ) -> DiagnosticResult<Vec<LookupResult>> {
        let results = self.many(
            from_module,
            in_module,
            word.name(),
            ShouldLookupFns::Yes,
            IsUfcs::No,
            AllowBuiltinTys::No,
        );

        if results.is_empty() {
            return Err(errors::name_not_found(self.cx.db, from_module, in_module, word));
        }

        if from_module != in_module {
            for res in &results {
                self.check_def_access(from_module, res.id(), word.span())?;
            }
        }

        Ok(results)
    }

    fn one(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        name: Ustr,
        span: Span,
        should_lookup_fns: ShouldLookupFns,
        allow_builtin_tys: AllowBuiltinTys,
    ) -> DiagnosticResult<Option<DefId>> {
        let results = self.many(
            from_module,
            in_module,
            name,
            should_lookup_fns,
            IsUfcs::No,
            allow_builtin_tys,
        );

        match results.len() {
            0 => Ok(None),
            1 => Ok(results.first().map(LookupResult::id)),
            _ => Err(Diagnostic::error(format!("ambiguous use of item `{}`", name))
                .with_label(Label::primary(span, "used here"))
                .with_labels(results.iter().map(|res| {
                    let def = &self.cx.db[res.id()];
                    Label::secondary(def.span, format!("`{}` is defined here", def.name))
                }))),
        }
    }

    fn many(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        name: Ustr,
        should_lookup_fns: ShouldLookupFns,
        is_ufcs: IsUfcs,
        allow_builtin_tys: AllowBuiltinTys,
    ) -> Vec<LookupResult> {
        let lookup_modules = self.get_lookup_modules(in_module, is_ufcs);
        let mut results = vec![];

        for module_id in lookup_modules {
            let env = self.cx.global_env.module(module_id);

            if let Some(id) = env.ns.get_def(from_module, name) {
                results.push(LookupResult::Def(id));
            }
            // TODO: lookup fns
            // else if should_lookup_fns == ShouldLookupFns::Yes {
            // if let Some(candidates) = env.ns.fns.get(&symbol) {
            //     results.extend(candidates.iter().cloned().
            // map(LookupResult::Fn)); }
            // }
        }

        if results.is_empty() && allow_builtin_tys == AllowBuiltinTys::Yes {
            return self
                .cx
                .global_env
                .builtin_tys
                .get(name)
                .into_iter()
                .map(LookupResult::Def)
                .collect();
        }

        results
    }

    fn check_def_access(
        &self,
        from_module: ModuleId,
        accessed: DefId,
        span: Span,
    ) -> DiagnosticResult<()> {
        if self.can_access(from_module, accessed) {
            Ok(())
        } else {
            Err(errors::private_access_violation(self.cx.db, accessed, span))
        }
    }

    fn get_lookup_modules(
        &self,
        in_module: ModuleId,
        is_ufcs: IsUfcs,
    ) -> impl Iterator<Item = ModuleId> + '_ {
        // TODO: glob
        // TODO: glob UFCS
        iter::once(in_module)
        // When `is_ufcs` is IsUfcs::No, we only include glob imports which are
        // IsUfcs::No
        // iter::once(in_module).chain(
        //     self.resolution_state
        //         .module_state(in_module)
        //         .globs
        //         .iter()
        //         .filter_map(move |(id, v)| match (is_ufcs, v) {
        //             (IsUfcs::Yes, _) | (IsUfcs::No, IsUfcs::No) => Some(id),
        //             (IsUfcs::No, IsUfcs::Yes) => None,
        //         })
        //         .copied(),
        // )
    }

    fn can_access(&self, from_module: ModuleId, accessed: DefId) -> bool {
        let def = &self.cx.db[accessed];
        def.scope.vis == Vis::Public || from_module == def.scope.module_id
    }
}

#[derive(Debug, Clone)]
pub(super) enum LookupResult {
    Def(DefId),
    Fn(FnCandidate),
}

impl LookupResult {
    pub(super) fn id(&self) -> DefId {
        match self {
            Self::Def(id) | Self::Fn(FnCandidate { id, .. }) => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PathLookup {
    Def(DefId),
    Variant(VariantId),
}

#[derive(Debug, Clone)]
pub enum TyLookup {
    AssocFn(DefId),
    Variant(VariantId),
}

create_bool_enum!(ShouldLookupFns);
create_bool_enum!(AllowBuiltinTys);
