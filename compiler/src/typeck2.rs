mod attrs;
mod builtins;
mod define;
mod errors;
mod fns;
mod imports;
mod lets;
mod lookup;
mod types;

use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    ast::Ast,
    db::{AdtId, Db, DefId, ModuleId},
    diagnostics::DiagnosticResult,
    hir::Hir,
    middle::{CallConv, IsUfcs, Pat, Vis},
    span::{Span, Spanned as _},
    ty::{printer::FnTyPrinter, FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    typeck2::builtins::BuiltinTys,
    word::Word,
};

pub fn typeck(db: &mut Db, ast: Ast) -> DiagnosticResult<Hir> {
    let mut cx = Typeck::new(db);
    let mut res_map = ResolutionMap::new();
    cx.init_global_env(&ast);
    imports::define_extern_imports(&mut cx, &ast)?;
    types::define(&mut cx, &mut res_map, &ast)?;
    lets::define(&mut cx, &mut res_map, &ast)?;
    fns::define(&mut cx, &mut res_map, &ast)?;
    imports::define_qualified_imports(&mut cx, &ast)?;
    imports::define_unqualified_imports(&mut cx, &ast)?;
    Ok(cx.hir)
}

pub(super) struct Typeck<'db> {
    db: &'db mut Db,

    /// The Hir being constructed
    hir: Hir,

    /// The global namespace, mapped by module
    global_env: GlobalEnv,

    /// A mapping from definitions to their resolved type
    def_to_ty: FxHashMap<DefId, Ty>,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db) -> Self {
        let mut def_to_ty = FxHashMap::default();
        let builtin_tys = BuiltinTys::new(db, &mut def_to_ty);
        Self { db, hir: Hir::new(), global_env: GlobalEnv::new(builtin_tys), def_to_ty }
    }

    fn init_global_env(&mut self, ast: &Ast) {
        for module in &ast.modules {
            self.global_env.insert_module(module.id);
        }
    }
}

/// Various mappings and resolutions from the `define_*` passes
pub(super) struct ResolutionMap {
    pub(super) item_to_def: FxHashMap<ast::GlobalItemId, DefId>,
    pub(super) item_to_adt: FxHashMap<ast::GlobalItemId, AdtId>,
    pub(super) item_to_pat: FxHashMap<ast::GlobalItemId, Pat>,
}

impl ResolutionMap {
    pub(super) fn new() -> Self {
        Self {
            item_to_def: FxHashMap::default(),
            item_to_adt: FxHashMap::default(),
            item_to_pat: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct GlobalEnv {
    modules: FxHashMap<ModuleId, ModuleEnv>,
    builtin_tys: BuiltinTys,
}

impl GlobalEnv {
    pub fn new(builtin_tys: BuiltinTys) -> Self {
        Self { modules: FxHashMap::default(), builtin_tys }
    }

    #[track_caller]
    pub fn module(&self, id: ModuleId) -> &ModuleEnv {
        self.modules.get(&id).unwrap()
    }

    #[track_caller]
    pub fn module_mut(&mut self, id: ModuleId) -> &mut ModuleEnv {
        self.modules.get_mut(&id).unwrap()
    }

    fn insert_module(&mut self, id: ModuleId) {
        self.modules.insert(id, ModuleEnv::new(id));
    }
}

#[derive(Debug)]
pub struct ModuleEnv {
    pub module_id: ModuleId,
    pub ns: Namespace,
    pub assoc_ns: FxHashMap<AssocTy, Namespace>,
}

impl ModuleEnv {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, ns: Namespace::new(module_id), assoc_ns: FxHashMap::default() }
    }
}

#[derive(Debug)]
pub struct Namespace {
    pub module_id: ModuleId,
    defs: UstrMap<NamespaceDef>,
    // pub fns: UstrMap<FnCandidateSet>,
    pub defined_fns: UstrMap<Vec<DefId>>,
}

impl Namespace {
    pub fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            defs: UstrMap::default(),
            // fns: FxHashMap::default()
            defined_fns: UstrMap::default(),
        }
    }

    pub fn insert_def(&mut self, name: Ustr, def: NamespaceDef) -> Option<NamespaceDef> {
        self.defs.insert(name, def)
    }

    pub fn get_def(&self, from_module: ModuleId, name: Ustr) -> Option<DefId> {
        self.defs
            .get(&name)
            .filter(|def| def.vis == Vis::Public || from_module == self.module_id)
            .map(|def| def.id)
    }

    pub fn contains_def(&self, name: Ustr) -> Option<&NamespaceDef> {
        self.defs.get(&name)
    }
}

#[derive(Debug)]
pub struct NamespaceDef {
    pub id: DefId,
    pub vis: Vis,
    pub span: Span,
}

impl NamespaceDef {
    pub fn new(id: DefId, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(super) enum AssocTy {
    Adt(AdtId),
    BuiltinTy(Ty),
}

impl AssocTy {
    pub(super) fn ty(self, db: &Db) -> Ty {
        match self {
            Self::Adt(adt_id) => db[adt_id].ty(),
            Self::BuiltinTy(ty) => ty,
        }
    }
}

impl From<Ty> for AssocTy {
    fn from(value: Ty) -> Self {
        match value.kind() {
            TyKind::Adt(adt_id, _) => Self::Adt(*adt_id),
            _ => Self::BuiltinTy(value),
        }
    }
}

#[derive(Debug)]
pub struct FnCandidateSet(Vec<FnCandidate>);

impl FnCandidateSet {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn iter(&self) -> std::slice::Iter<'_, FnCandidate> {
        self.0.iter()
    }

    pub fn try_insert(&mut self, candidate: FnCandidate) -> Result<(), FnCandidateInsertError> {
        if let Some(prev) = self.0.iter().find(|c| *c == &candidate) {
            return Err(FnCandidateInsertError::AlreadyExists {
                prev: prev.clone(),
                curr: candidate,
            });
        }

        self.0.push(candidate);
        Ok(())
    }

    pub fn find(&self, cx: &Typeck, query: &FnQuery) -> Vec<&FnCandidate> {
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
pub enum FnCandidateInsertError {
    AlreadyExists { prev: FnCandidate, curr: FnCandidate },
}

#[derive(Debug, Clone)]
pub struct FnCandidate {
    pub id: DefId,
    pub word: Word,
    pub ty: FnTy,
}

impl FnCandidate {
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
        todo!()
        // if arg.can_unify(param, cx, UnifyOptions::default()).is_ok() {
        //     return Some(FnCandidateScore::Eq);
        // }
        //
        // if arg.can_coerce(
        //     &param,
        //     cx,
        //     CoerceOptions {
        //         unify_options: UnifyOptions::default(),
        //         rollback_unifications: true,
        //         allow_owned_to_ref,
        //     },
        // ) {
        //     return Some(FnCandidateScore::Coerce);
        // }
        //
        // if arg.can_coerce(
        //     &param,
        //     cx,
        //     CoerceOptions {
        //         unify_options: UnifyOptions { unify_param_tys: true },
        //         rollback_unifications: true,
        //         allow_owned_to_ref,
        //     },
        // ) {
        //     return Some(FnCandidateScore::Polymorphic);
        // }
        //
        // // println!("arg: {} | param: {}", arg.display(cx.db),
        // param.display(cx.db));
        //
        // None
    }

    pub fn display<'a>(&'a self, db: &'a Db) -> FnTyPrinter {
        self.ty.display(db, Some(db[self.id].name))
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
pub enum FnCandidateScore {
    Eq = 0,
    Coerce = 1,
    Polymorphic = 2,
}

#[derive(Debug, Clone)]
pub enum Query<'a> {
    Name(Word),
    Fn(FnQuery<'a>),
}

impl<'a> Query<'a> {
    #[inline]
    pub fn word(&self) -> Word {
        match self {
            Query::Name(word) | Query::Fn(FnQuery { word, .. }) => *word,
        }
    }

    #[inline]
    pub fn name(&self) -> Ustr {
        self.word().name()
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.word().span()
    }

    pub fn is_ufcs(&self) -> IsUfcs {
        match self {
            Query::Name(_) => IsUfcs::No,
            Query::Fn(FnQuery { is_ufcs, .. }) => *is_ufcs,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnQuery<'a> {
    pub word: Word,
    pub ty_args: Option<&'a [Ty]>,
    pub args: &'a [FnTyParam],
    pub is_ufcs: IsUfcs,
}

impl<'a> FnQuery<'a> {
    pub fn new(
        word: Word,
        ty_args: Option<&'a [Ty]>,
        args: &'a [FnTyParam],
        is_ufcs: IsUfcs,
    ) -> Self {
        Self { word, ty_args, args, is_ufcs }
    }

    pub fn display<'db>(&'db self, db: &'db Db) -> FnTyPrinter {
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
