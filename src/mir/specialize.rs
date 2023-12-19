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
    Specialize::new(db, mir).run();
}

struct Specialize<'db> {
    db: &'db mut Db,
    mir: &'db mut Mir,

    // Functions that have already been monomorphized
    mono_fns: FxHashMap<MonoValue, FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoValue {
    pub value: ValueId,
    pub ty: Ty,
}

impl<'db> Specialize<'db> {
    fn new(db: &'db mut Db, mir: &'db mut Mir) -> Self {
        Self { db, mir, mono_fns: FxHashMap::default() }
    }

    fn run(&mut self) {
        for fun in &self.mir.fns {
            for (value, instantiation) in &fun.body.instantations {
                self.monomorphize_value(&fun.body, *value, instantiation);
            }
        }
    }

    fn monomorphize_value(
        &mut self,
        body: &Body,
        value: ValueId,
        instantiation: &Instantiation,
    ) -> ValueKind {
        match &body.value(value).kind {
            ValueKind::Fn(id) => {
                let ty = ParamFolder { db: self.db, instantiation }
                    .fold(self.mir.fn_sigs[*id].ty);

                let mono_value = MonoValue { value, ty };

                let mono_sig_id =
                    self.mono_fns.get(&mono_value).copied().unwrap_or_else(
                        || self.monomorphize_fn(&mono_value, instantiation),
                    );

                ValueKind::Fn(mono_sig_id)
            }
            kind => unreachable!(
                "unexpected value kind in specialization: {kind:?}"
            ),
        }
    }

    fn monomorphize_fn_sig(
        &mut self,
        mono_value: &MonoValue,
        instantiation: &Instantiation,
    ) -> FnSigId {
        todo!("monomorphize_fn_sig")
    }

    fn monomorphize_fn(
        &mut self,
        mono_value: &MonoValue,
        instantiation: &Instantiation,
    ) -> FnSigId {
        if let Some(target_value) = self.mono_fns.get(mono_value).copied() {
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
