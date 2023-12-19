use std::iter;

use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    hir::mangle,
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

    // Functions that have already been specialized
    specialized_fns: FxHashMap<SpecializedFn, FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpecializedFn {
    pub id: FnSigId,
    pub ty: Ty,
}

impl<'db> Specialize<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, specialized_fns: FxHashMap::default() }
    }

    fn run(&mut self, mir: &mut Mir) {
        // TODO: work: VecDeqeue<Job>
        // TODO: add main fn to work
        // TODO: while job = work.pop()
        // TODO: if no instantiations, return
        // TODO: for each instantation
        // TODO: if (value, instantation) wasn't specialized
        // TODO: push (FnSigId, Instantation) to work

        for fn_id in mir.fns.keys() {
            self.specialize_fn_instantations(mir, fn_id);
        }

        // TODO: go over global instantiations
    }

    fn specialize_fn_instantations(&mut self, mir: &mut Mir, fn_id: FnId) {
        let instantations = mir.fns[fn_id].body.instantations.clone();
        for (value, instantiation) in instantations {
            self.specialize_value(mir, fn_id, value, &instantiation);
        }
    }

    fn specialize_value(
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

                let specialized_fn = SpecializedFn { id, ty };

                let specialized_sig_id = self
                    .specialized_fns
                    .get(&specialized_fn)
                    .copied()
                    .unwrap_or_else(|| {
                        self.specialize_fn(mir, specialized_fn, instantiation)
                    });

                mir.fns[fn_id].body.value_mut(value).kind =
                    ValueKind::Fn(specialized_sig_id);
            }
            kind => unreachable!(
                "unexpected value kind in specialization: {kind:?}"
            ),
        }
    }

    #[must_use]
    fn specialize_fn_sig(
        &mut self,
        mir: &mut Mir,
        specialized_fn: &SpecializedFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        let mut sig = mir.fn_sigs[specialized_fn.id].clone();
        sig.subst(&mut ParamFolder { db: self.db, instantiation });

        let instantation_str = instantiation
            .values()
            .map(|&ty| mangle::mangle_ty_name(self.db, ty))
            .collect::<Vec<_>>()
            .join("_");

        sig.name = ustr(&format!("{}{}", sig.name, instantation_str));

        let new_sig_id = mir.fn_sigs.push_with_key(|id| {
            sig.id = id;
            sig
        });

        new_sig_id
    }

    #[must_use]
    fn specialize_fn(
        &mut self,
        mir: &mut Mir,
        specialized_fn: SpecializedFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        if let Some(target_value) =
            self.specialized_fns.get(&specialized_fn).copied()
        {
            return target_value;
        }

        let new_sig_id =
            self.specialize_fn_sig(mir, &specialized_fn, instantiation);

        self.specialized_fns.insert(specialized_fn, new_sig_id);

        // TODO: fun:
        // TODO:     get fun
        // TODO:     clone fun
        // TODO:     subst fun
        // TODO: specialize body instantations (recursive)

        new_sig_id
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
