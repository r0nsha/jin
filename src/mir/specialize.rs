use rustc_hash::FxHashMap;
use ustr::ustr;

use crate::{
    data_structures::index_vec::IndexVecExt,
    db::Db,
    hir::mangle,
    mir::*,
    subst::{ParamFolder, Subst},
    ty::{fold::TyFolder, Instantiation, Ty},
};

pub fn specialize(db: &mut Db, mir: &mut Mir) {
    Specialize::new(db).run(mir);
}

struct Specialize<'db> {
    db: &'db mut Db,

    // Functions that have already been specialized
    specialized_fns: FxHashMap<SpecializedFn, FnSigId>,
    // A map of used function ids, pointing to their new function id
    // used_fns: FxHashSet<FnSigId, FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpecializedFn {
    pub id: FnSigId,
    pub ty: Ty,
}

impl<'db> Specialize<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self {
            db,
            specialized_fns: FxHashMap::default(),
            // used_fns: FxHashMap::default(),
        }
    }

    fn run(&mut self, mir: &mut Mir) {
        // for id in mir.fn_sigs.keys() {
        //     if mir.fns.contains_key(&id) {
        //         self.specialize_fn_instantations(mir, id);
        //     }
        // }
        //
        // for id in mir.globals.keys() {
        //     self.specialize_global_instantations(mir, id);
        // }
    }

    // fn specialize_fn_instantations(&mut self, mir: &mut Mir, id: FnSigId) {
    //     let instantations = mir.fns[&id].body.instantations.clone();
    //
    //     for (value, instantiation) in instantations {
    //         let new_value_kind = self.specialize_value(
    //             mir,
    //             mir.fns[&id].body.value(value).kind.clone(),
    //             &instantiation,
    //         );
    //         mir.fns.get_mut(&id).unwrap().body.value_mut(value).kind =
    //             new_value_kind;
    //     }
    // }

    // fn specialize_global_instantations(&mut self, mir: &mut Mir, id: GlobalId) {
    //     let instantations = match &mir.globals[id].kind {
    //         GlobalKind::Static(body, _) => body.instantations.clone(),
    //         GlobalKind::Extern => return,
    //     };
    //
    //     for (value, instantiation) in instantations {
    //         let value_kind = match &mir.globals[id].kind {
    //             GlobalKind::Static(body, _) => body.value(value).kind.clone(),
    //             GlobalKind::Extern => unreachable!(),
    //         };
    //
    //         let new_value_kind =
    //             self.specialize_value(mir, value_kind, &instantiation);
    //
    //         match &mut mir.globals[id].kind {
    //             GlobalKind::Static(body, _) => {
    //                 body.value_mut(value).kind = new_value_kind;
    //             }
    //             GlobalKind::Extern => unreachable!(),
    //         }
    //     }
    // }

    // fn specialize_value(
    //     &mut self,
    //     mir: &mut Mir,
    //     value_kind: ValueKind,
    //     instantiation: &Instantiation,
    // ) -> ValueKind {
    //     match value_kind {
    //         ValueKind::Fn(id) => {
    //             let ty = ParamFolder { db: self.db, instantiation }
    //                 .fold(mir.fn_sigs[id].ty);
    //
    //             let specialized_fn = SpecializedFn { id, ty };
    //
    //             let specialized_sig_id =
    //                 self.specialize_fn(mir, specialized_fn, instantiation);
    //
    //             ValueKind::Fn(specialized_sig_id)
    //         }
    //         kind => unreachable!(
    //             "unexpected value kind in specialization: {kind:?}"
    //         ),
    //     }
    // }

    // #[must_use]
    // fn specialize_fn_sig(
    //     &mut self,
    //     mir: &mut Mir,
    //     specialized_fn: &SpecializedFn,
    //     instantiation: &Instantiation,
    // ) -> FnSigId {
    //     let mut sig = mir.fn_sigs[specialized_fn.id].clone();
    //     sig.subst(&mut ParamFolder { db: self.db, instantiation });
    //
    //     let instantation_str = instantiation
    //         .values()
    //         .map(|&ty| mangle::mangle_ty_name(self.db, ty))
    //         .collect::<Vec<_>>()
    //         .join("_");
    //
    //     sig.name = ustr(&format!("{}__{}", sig.name, instantation_str));
    //
    //     mir.fn_sigs.push_with_key(|id| {
    //         sig.id = id;
    //         sig
    //     })
    // }

    // #[must_use]
    // fn specialize_fn(
    //     &mut self,
    //     mir: &mut Mir,
    //     specialized_fn: SpecializedFn,
    //     instantiation: &Instantiation,
    // ) -> FnSigId {
    //     if let Some(target_value) =
    //         self.specialized_fns.get(&specialized_fn).copied()
    //     {
    //         return target_value;
    //     }
    //
    //     let old_sig_id = specialized_fn.id;
    //     let new_sig_id =
    //         self.specialize_fn_sig(mir, &specialized_fn, instantiation);
    //
    //     self.specialized_fns.insert(specialized_fn, new_sig_id);
    //
    //     let mut fun = mir.fns.get(&old_sig_id).expect("fn to exist").clone();
    //
    //     fun.sig = new_sig_id;
    //     fun.subst(&mut ParamFolder { db: self.db, instantiation });
    //
    //     mir.fns.insert(new_sig_id, fun);
    //     self.specialize_fn_instantations(mir, new_sig_id);
    //
    //     new_sig_id
    // }
}
