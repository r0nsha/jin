use std::iter;

use compiler_core::ty::{FloatTy, InferTy, IntTy};
use compiler_core::{
    db::{AdtKind, Db, DefId, ModuleId, UnionDef, Variant, VariantId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{CallConv, IsUfcs},
    span::{Span, Spanned as _},
    ty::{printer::FnTyPrinter, FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    word::Word,
};
use itertools::Itertools as _;
use rustc_hash::FxHashSet;
use ustr::{ustr, Ustr};

use crate::unify::TyUnifyExt as _;
use crate::{
    coerce::{Coerce as _, CoerceOptions},
    errors,
    ns::{AssocTy, Env, NsDef},
    unify::UnifyOptions,
    Typeck,
};

impl<'db> Typeck<'db> {
    pub(crate) fn lookup(&self) -> Lookup<'db, '_> {
        Lookup::new(self)
    }
}

pub(crate) struct Lookup<'db, 'cx> {
    cx: &'cx Typeck<'db>,
    env: Option<&'cx Env>,
}

impl<'db, 'cx> Lookup<'db, 'cx> {
    fn new(cx: &'cx Typeck<'db>) -> Self {
        Self { cx, env: None }
    }

    pub(crate) fn with_env(mut self, env: &'cx Env) -> Self {
        self.env = Some(env);
        self
    }

    pub(crate) fn query(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        query: &Query,
    ) -> DiagnosticResult<DefId> {
        let name = query.name();

        if let Query::Name(_) | Query::Fn(FnQuery { is_ufcs: IsUfcs::No, .. }) = query {
            if let Some(id) = self.env.and_then(|env| env.lookup(name).copied()) {
                return Ok(id);
            }
        }

        if let Query::Fn(fn_query) = query {
            if let Some(id) = self.query_fns(from_module, in_module, fn_query)? {
                return Ok(id);
            }
        }

        // We should only lookup functions if we didn't already query for a function
        let should_lookup_fns = match query {
            Query::Name(_) => ShouldLookupFns::Candidates,
            Query::Fn(_) => ShouldLookupFns::No,
        };

        let id = self
            .one(from_module, in_module, name, query.span(), should_lookup_fns)?
            .ok_or_else(|| match query {
                Query::Name(word) => {
                    errors::name_not_found(self.cx.db, from_module, in_module, *word)
                }
                Query::Fn(fn_query) => errors::fn_not_found(self.cx.db, fn_query),
            })?;

        self.cx.check_access_def(from_module, id, query.span())?;

        Ok(id)
    }

    fn query_fns(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        query: &FnQuery,
    ) -> DiagnosticResult<Option<DefId>> {
        let candidates = self
            .get_lookup_modules(in_module, query.is_ufcs)
            .filter_map(|module_id| {
                self.cx.global_env.module(module_id).ns.fns.get(&query.word.name())
            })
            .flat_map(|set| set.find(self.cx, query))
            .unique_by(|candidate| candidate.id)
            .collect::<Vec<_>>();

        self.check_and_filter_fn_candidates(query, candidates, from_module)
    }

    fn query_assoc_fns(
        &self,
        from_module: ModuleId,
        assoc_ty: AssocTy,
        query: &FnQuery,
    ) -> DiagnosticResult<Option<DefId>> {
        let Some(set) = self
            .cx
            .global_env
            .assoc_ns
            .get(&assoc_ty)
            .and_then(|ns| ns.fns.get(&query.word.name()))
        else {
            return Ok(None);
        };

        let candidates = set.find(self.cx, query);
        self.check_and_filter_fn_candidates(query, candidates, from_module)
    }

    fn check_and_filter_fn_candidates(
        &self,
        query: &FnQuery,
        mut candidates: Vec<&FnCandidate>,
        from_module: ModuleId,
    ) -> DiagnosticResult<Option<DefId>> {
        if !candidates.is_empty()
            && candidates.iter().all(|c| !self.cx.can_access_def(from_module, c.id))
        {
            return Err(Diagnostic::error(format!(
                "all functions which apply to `{}` are private",
                query.display(self.cx.db)
            ))
            .with_label(Label::primary(query.word.span(), "no accessible function found")));
        }

        candidates.retain(|c| self.cx.can_access_def(from_module, c.id));

        match candidates.len() {
            0 => Ok(None),
            1 => Ok(Some(candidates.first().unwrap().id)),
            _ => {
                Err(Diagnostic::error(format!("ambiguous call to `{}`", query.display(self.cx.db)))
                    .with_label(Label::primary(query.word.span(), "call here"))
                    .with_note("these functions apply:")
                    .with_notes(
                        candidates.into_iter().map(|c| c.display_qualified(self.cx.db).to_string()),
                    ))
            }
        }
    }

    pub fn path(&self, from_module: ModuleId, path: &[Word]) -> DiagnosticResult<PathLookup> {
        let (&last, path) = path.split_last().expect("to have at least one element");

        let mut target_module = from_module;

        for (idx, &part) in path.iter().enumerate() {
            let part_id = self.query(from_module, target_module, &Query::Name(part))?;
            let part_ty = self.cx.normalize(self.cx.def_ty(part_id));

            match part_ty.kind() {
                TyKind::Module(module_id) => {
                    target_module = *module_id;
                }
                TyKind::Type(ty) => {
                    // The type could be a union, and the next part could be a variant, so we
                    // explicitly look it up
                    let next_part = path.get(idx + 1).copied().unwrap_or(last);

                    match self.query_assoc_ns(
                        from_module,
                        *ty,
                        part.span(),
                        &Query::Name(next_part),
                    )? {
                        AssocLookup::Variant(variant_id) => {
                            // If there are more parts after this variant, it's an error, since we
                            // there are no symbols under variants
                            if path.get(idx + 2).is_some() {
                                return Err(Diagnostic::error(format!(
                                    "`{}` is a variant, not a module",
                                    self.cx.db[variant_id].name,
                                ))
                                .with_label(Label::primary(next_part.span(), "not a module")));
                            }

                            return Ok(PathLookup::Variant(variant_id));
                        }
                        AssocLookup::AssocFn(id) => {
                            return Err(Diagnostic::error(format!(
                                "`{}` is an associated function, not a module",
                                self.cx.db[id].name,
                            ))
                            .with_label(Label::primary(next_part.span(), "not a module")));
                        }
                    }
                }
                ty => {
                    return Err(errors::expected_module(
                        format!("found type `{}`", ty.display(self.cx.db)),
                        part.span(),
                    ))
                }
            }
        }

        let id = self.query(from_module, target_module, &Query::Name(last))?;
        Ok(PathLookup::Def(id))
    }

    /// Looks up a `query` in the associated namespace of `ty`.
    pub(crate) fn query_assoc_ns(
        &self,
        from_module: ModuleId,
        ty: Ty,
        ty_span: Span,
        query: &Query,
    ) -> DiagnosticResult<AssocLookup> {
        if let Query::Fn(fn_query) = query {
            if let Some(id) = self.query_assoc_fns(from_module, AssocTy::from(ty), fn_query)? {
                return Ok(AssocLookup::AssocFn(id));
            }
        }

        if let TyKind::Adt(adt_id, _) = ty.kind() {
            let name = query.word();
            let adt = &self.cx.db[*adt_id];

            match &adt.kind {
                AdtKind::Union(union_def) => self
                    .variant_in_union(union_def, name, ty_span)
                    .map(|v| AssocLookup::Variant(v.id)),
                AdtKind::Struct(_) => {
                    Err(errors::assoc_name_not_found(self.cx.db, adt.ty(), query))
                }
            }
        } else {
            Err(errors::assoc_name_not_found(self.cx.db, ty, query))
        }
    }

    pub(crate) fn maybe_variant_in_union(
        &self,
        union_def: &'db UnionDef,
        name: Word,
    ) -> Option<&Variant> {
        union_def.variants(self.cx.db).find(|v| v.name.name() == name.name())
    }

    pub(crate) fn variant_in_union(
        &self,
        union_def: &'db UnionDef,
        name: Word,
        span: Span,
    ) -> DiagnosticResult<&Variant> {
        self.maybe_variant_in_union(union_def, name)
            .ok_or_else(|| errors::variant_not_found(self.cx.db, union_def.id, span, name))
    }

    fn one(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        name: Ustr,
        span: Span,
        should_lookup_fns: ShouldLookupFns,
    ) -> DiagnosticResult<Option<DefId>> {
        let results = self.many(in_module, name, should_lookup_fns, IsUfcs::No);

        if results.is_empty() {
            // We allow looking up builtin types only when looking up a symbol in the same
            // module as its environment's module.
            if from_module == in_module {
                return Ok(self.cx.global_env.builtin_tys.get(name));
            }

            return Ok(None);
        }

        let filtered_results = self.keep_accessible_lookup_results(from_module, results);
        if filtered_results.is_empty() {
            return Err(Diagnostic::error(format!("`{name}` is private"))
                .with_label(Label::primary(span, "private definition")));
        }

        match filtered_results.len() {
            0 => unreachable!(),
            1 => Ok(filtered_results.first().map(LookupResult::def_id)),
            _ => Err(Diagnostic::error(format!("ambiguous use of item `{}`", name))
                .with_label(Label::primary(span, "used here"))
                .with_labels(filtered_results.iter().map(|res| {
                    let def = &self.cx.db[res.def_id()];
                    Label::secondary(def.span, format!("`{}` defined here", def.name))
                }))),
        }
    }

    fn many(
        &self,
        in_module: ModuleId,
        name: Ustr,
        should_lookup_fns: ShouldLookupFns,
        is_ufcs: IsUfcs,
    ) -> Vec<LookupResult> {
        let lookup_modules = self.get_lookup_modules(in_module, is_ufcs);
        let mut results = FxHashSet::default();

        for module_id in lookup_modules {
            let env = self.cx.global_env.module(module_id);

            if let Some(def) = env.ns.defs.get(&name) {
                if module_id == in_module {
                    // For definitions, we always prioritize a definition which is defined or
                    // imported in scope. Glob imports always come next for definitions.
                    return vec![LookupResult::Def(*def)];
                }

                results.insert(LookupResult::Def(*def));
            } else if let ShouldLookupFns::Candidates = should_lookup_fns {
                if let Some(candidates) = env.ns.fns.get(&name) {
                    results.extend(candidates.iter().cloned().map(LookupResult::Fn));
                }
            }
        }

        results.into_iter().collect()
    }

    fn get_lookup_modules(
        &self,
        in_module: ModuleId,
        is_ufcs: IsUfcs,
    ) -> impl Iterator<Item = ModuleId> + '_ {
        // When `is_ufcs` is IsUfcs::No, we only want to include glob imports
        // which are IsUfcs::No
        iter::once(in_module).chain(
            self.cx
                .global_env
                .module(in_module)
                .globs
                .iter()
                .filter_map(move |(id, imp)| match (is_ufcs, imp.is_ufcs) {
                    (IsUfcs::Yes, _) | (IsUfcs::No, IsUfcs::No) => Some(id),
                    (IsUfcs::No, IsUfcs::Yes) => None,
                })
                .copied(),
        )
    }

    #[inline]
    fn keep_accessible_lookup_results(
        &self,
        from_module: ModuleId,
        results: Vec<LookupResult>,
    ) -> Vec<LookupResult> {
        results
            .into_iter()
            .filter(|r| match r {
                LookupResult::Def(def) => def.can_access(self.cx, from_module),
                LookupResult::Fn(c) => self.cx.can_access_def(from_module, c.id),
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum LookupResult {
    Def(NsDef),
    Fn(FnCandidate),
}

impl LookupResult {
    pub(crate) fn def_id(&self) -> DefId {
        match self {
            Self::Def(n) => n.id,
            Self::Fn(c) => c.id,
        }
    }
}

impl std::cmp::PartialEq for LookupResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LookupResult::Def(a), LookupResult::Def(b)) => {
                a.id == b.id && a.name.name() == b.name.name()
            }
            (LookupResult::Fn(a), LookupResult::Fn(b)) => {
                a.id == b.id && a.word.name() == b.word.name()
            }
            _ => false,
        }
    }
}

impl std::cmp::Eq for LookupResult {}

impl std::hash::Hash for LookupResult {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LookupResult::Def(def) => {
                def.id.hash(state);
                def.name.name().hash(state);
            }
            LookupResult::Fn(c) => {
                c.id.hash(state);
                c.word.name().hash(state);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum PathLookup {
    Def(DefId),
    Variant(VariantId),
}

#[derive(Debug, Clone)]
pub(crate) enum AssocLookup {
    AssocFn(DefId),
    Variant(VariantId),
}

#[derive(Debug)]
pub(crate) struct FnCandidateSet(Vec<FnCandidate>);

impl FnCandidateSet {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, FnCandidate> {
        self.0.iter()
    }

    pub(crate) fn try_insert(
        &mut self,
        candidate: FnCandidate,
    ) -> Result<(), FnCandidateInsertError> {
        if let Some(prev) = self.0.iter().find(|c| *c == &candidate) {
            return Err(FnCandidateInsertError::AlreadyExists {
                prev: prev.clone(),
                curr: candidate,
            });
        }

        self.0.push(candidate);
        Ok(())
    }

    pub(crate) fn find(&self, cx: &Typeck, query: &FnQuery) -> Vec<&FnCandidate> {
        let scores = self.scores(cx, query);
        let Some(&min_score) = scores.iter().map(|(_, s)| s).min() else {
            return vec![];
        };
        scores.into_iter().filter_map(|(c, s)| (s == min_score).then_some(c)).collect()
    }

    fn scores(&self, cx: &Typeck, query: &FnQuery) -> Vec<(&FnCandidate, u32)> {
        let mut scores = vec![];

        for c in &self.0 {
            if let Some(score) = c.test(cx, query) {
                scores.push((c, score));
            }
        }

        scores
    }
}

impl Default for FnCandidateSet {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub(crate) enum FnCandidateInsertError {
    AlreadyExists { prev: FnCandidate, curr: FnCandidate },
}

impl FnCandidateInsertError {
    pub(crate) fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            FnCandidateInsertError::AlreadyExists { prev, curr } => {
                errors::multiple_fn_def_err(db, prev.module_id(db), prev.word.span(), &curr)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FnCandidate {
    pub(crate) id: DefId,
    pub(crate) word: Word,
    pub(crate) ty: FnTy,
}

impl FnCandidate {
    pub(crate) fn module_id(&self, db: &Db) -> ModuleId {
        db[self.id].scope.module_id
    }

    // Tests the given query against the function candidate, returning
    // a Some(score) if there's a match, or a None if there isn't.
    // See `distance` for how parameter scoring works.
    fn test(&self, cx: &Typeck, query: &FnQuery) -> Option<u32> {
        if self.ty.params.len() != query.args.len() {
            return None;
        }

        // Check that the amount of given type arguments == the amount of type
        // parameters in this candidate
        match query.ty_args {
            Some(ty_args) if ty_args.len() != self.ty.collect_params().len() => {
                return None;
            }
            _ => (),
        }

        // Make sure that all passed named arguments exist in this candidate
        if !query.args.iter().all(|arg| {
            arg.name.map_or(true, |name| self.ty.params.iter().any(|p| Some(name) == p.name))
        }) {
            return None;
        }

        let mut total_score = 0;

        for (idx, (arg, param)) in query.args.iter().zip(self.ty.params.iter()).enumerate() {
            let allow_owned_to_ref = query.is_ufcs == IsUfcs::Yes && idx == 0;
            let score = Self::distance(cx, arg.ty, param.ty, allow_owned_to_ref)?;
            total_score += score as u32;
        }

        Some(total_score)
    }

    // Calculates the distance between an argument and the parameter it is applied
    // to. The actual distance is calculated by the amount of "steps"
    // required to convert the argument to the parameter.
    fn distance(
        cx: &Typeck,
        arg: Ty,
        param: Ty,
        allow_owned_to_ref: bool,
    ) -> Option<FnCandidateScore> {
        if arg == param {
            return Some(FnCandidateScore::EqExact);
        }

        match (arg.kind(), param.kind()) {
            (TyKind::Infer(InferTy::Int(_)), TyKind::Int(IntTy::Int))
            | (TyKind::Infer(InferTy::Float(_)), TyKind::Float(FloatTy::F64)) => {
                return Some(FnCandidateScore::EqPreferred)
            }
            _ => (),
        }

        if arg.can_unify(param, cx, UnifyOptions::default()).is_ok() {
            return Some(FnCandidateScore::Eq);
        }

        if arg.can_coerce(
            &param,
            cx,
            CoerceOptions {
                unify_options: UnifyOptions { unify_param_tys: true },
                rollback_unifications: true,
                allow_owned_to_ref,
            },
        ) {
            return Some(FnCandidateScore::Polymorphic);
        }

        if arg.can_coerce(
            &param,
            cx,
            CoerceOptions {
                unify_options: UnifyOptions::default(),
                rollback_unifications: true,
                allow_owned_to_ref,
            },
        ) {
            return Some(FnCandidateScore::Coerce);
        }

        // println!("arg: {} | param: {}", arg.display(cx.db), param.display(cx.db));

        None
    }

    pub(crate) fn display<'a>(&'a self, db: &'a Db) -> FnTyPrinter {
        self.ty.display(db, Some(self.word.name()))
    }

    pub(crate) fn display_qualified<'a>(&'a self, db: &'a Db) -> FnTyPrinter {
        self.ty.display(db, Some(ustr(&db[self.id].qpath.join())))
    }
}

impl PartialEq for FnCandidate {
    fn eq(&self, other: &Self) -> bool {
        if self.word.name() != other.word.name() || self.ty.params.len() != other.ty.params.len() {
            return false;
        }

        // Both function parameters are the same, order is insignificant
        if !self.ty.params.iter().all(|p1| {
            if let Some(name) = p1.name {
                other.ty.params.iter().any(|p2| Some(name) == p2.name)
            } else {
                false
            }
        }) {
            return false;
        }

        // Both function parameters are the same, in order
        if self
            .ty
            .params
            .iter()
            .zip(other.ty.params.iter())
            .any(|(p1, p2)| !fn_candidate_tys_eq(p1.ty, p2.ty))
        {
            return false;
        }

        true
    }
}

impl Eq for FnCandidate {}

fn fn_candidate_tys_eq(a: Ty, b: Ty) -> bool {
    match (a.kind(), b.kind()) {
        (TyKind::Ref(a, _), TyKind::Ref(b, _)) => {
            // Consider two references as equal candidates, regardless of their mutability
            a == b
        }
        _ => a == b,
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub(crate) enum FnCandidateScore {
    EqExact = 0,
    EqPreferred = 1,
    Eq = 2,
    Polymorphic = 3,
    Coerce = 4,
}

#[derive(Debug, Clone)]
pub(crate) enum Query<'a> {
    Name(Word),
    Fn(FnQuery<'a>),
}

impl<'a> Query<'a> {
    #[inline]
    pub(crate) fn word(&self) -> Word {
        match self {
            Query::Name(word) | Query::Fn(FnQuery { word, .. }) => *word,
        }
    }

    #[inline]
    pub(crate) fn name(&self) -> Ustr {
        self.word().name()
    }

    #[inline]
    pub(crate) fn span(&self) -> Span {
        self.word().span()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FnQuery<'a> {
    pub(crate) word: Word,
    pub(crate) ty_args: Option<&'a [Ty]>,
    pub(crate) args: &'a [FnTyParam],
    pub(crate) is_ufcs: IsUfcs,
}

impl<'a> FnQuery<'a> {
    pub(crate) fn new(
        word: Word,
        ty_args: Option<&'a [Ty]>,
        args: &'a [FnTyParam],
        is_ufcs: IsUfcs,
    ) -> Self {
        Self { word, ty_args, args, is_ufcs }
    }

    pub(crate) fn display<'db>(&'db self, db: &'db Db) -> FnTyPrinter {
        FnTyPrinter {
            db,
            name: Some(self.word.name()),
            params: self.args,
            ret: None,
            callconv: CallConv::default(),
            flags: FnTyFlags::empty(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShouldLookupFns {
    Candidates,
    No,
}
