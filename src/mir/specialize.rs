use std::iter;

use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    index_vec::IndexVecExt,
    middle::{Mutability, NamePat, Pat, Vis},
    mir::*,
    span::Spanned,
    subst::{ParamFolder, Subst},
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

pub fn specialize(db: &mut Db, mir: &mut Mir) {
    Specialize::new(db).run(mir);
}

struct Specialize<'db> {
    db: &'db mut Db,

    // Functions that have already been monomorphized
    mono_fns: FxHashMap<MonoFn, FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoFn {
    pub id: FnSigId,
    pub ty: Ty,
}

impl<'db> Specialize<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, mono_fns: FxHashMap::default() }
    }

    fn run(&mut self, mir: &mut Mir) {
        // TODO: work: VecDeqeue<Job>
        // TODO: add main fn to work
        // TODO: while job = work.pop()
        // TODO: if no instantiations, return
        // TODO: for each instantation
        // TODO: if (value, instantation) wasn't monomorphized
        // TODO: push (FnSigId, Instantation) to work

        for fn_id in mir.fns.keys() {
            for (value, instantiation) in
                mir.fns[fn_id].body.instantations.clone()
            {
                self.monomorphize_value(mir, fn_id, value, &instantiation);
            }
        }
    }

    fn monomorphize_value(
        &mut self,
        mir: &mut Mir,
        fn_id: FnId,
        value: ValueId,
        instantiation: &Instantiation,
    ) {
        let value_kind = mir.fns[fn_id].body.value(value).kind.clone();

        match value_kind {
            ValueKind::Fn(id) => {
                let ty = ParamFolder { db: self.db, instantiation }
                    .fold(mir.fn_sigs[id].ty);

                let mono_fn = MonoFn { id, ty };

                let mono_sig_id =
                    self.mono_fns.get(&mono_fn).copied().unwrap_or_else(|| {
                        self.monomorphize_fn(&mono_fn, instantiation)
                    });

                mir.fns[fn_id].body.value_mut(value).kind =
                    ValueKind::Fn(mono_sig_id);
            }
            kind => unreachable!(
                "unexpected value kind in specialization: {kind:?}"
            ),
        }
    }

    #[must_use]
    fn monomorphize_fn_sig(
        &mut self,
        mono_fn: &MonoFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        todo!("monomorphize_fn_sig")
    }

    #[must_use]
    fn monomorphize_fn(
        &mut self,
        mono_fn: &MonoFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        if let Some(target_value) = self.mono_fns.get(mono_fn).copied() {
            return target_value;
        }

        todo!("monomorphize_fn")
        //     let mut new_fun = fun.clone();
        //     new_fun.subst(&mut ParamFolder { db: self.db, instantiation });
        // TODO: sig:
        // TODO:     get sig
        // TODO:     clone sig
        // TODO:     subst sig:
        // TODO:         fold ty
        // TODO:         fold param tys
        // TODO:     add instantation types to function name
        // TODO:     add sig to mir, get sig id
        // TODO:     insert lowered into mono_fns
        // TODO: fun:
        // TODO:     clone fun
        // TODO:     subst fun:
        // TODO:         subst body:
        // TODO:             subst values
        // TODO:             subst blocks
        // TODO:                 subst insts
        // TODO:             subst instantations
        // TODO: go over body's instantations (recursive)
    }
}

struct SpecializeBody<'cx, 'db> {
    cx: &'cx mut Specialize<'db>,
    body: &'cx mut Body,
    // destroy_glue: &'cx hir::DestroyGlue,
    // destroy_flags: FxHashMap<hir::DestroyGlueItem, ValueId>,
}

impl<'cx, 'db> SpecializeBody<'cx, 'db> {
    fn new(
        cx: &'cx mut Specialize<'db>,
        body: &'cx mut Body,
        // destroy_glue: &'cx hir::DestroyGlue,
    ) -> Self {
        Self { cx, body }
    }

    fn run(&mut self) {
        todo!("SpecializeBody")
    }
}
