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

    fn run(mut self) {
        for sig in &mut self.mir.fn_sigs {
            // TODO: clone sig
            // TODO: fold ty
            // TODO: fold param tys
            todo!("specialize sig")
        }

        for fun in &mut self.mir.fns {
            todo!("specialize fn")
        }
    }

    fn monomorphize_fn(
        &mut self,
        mono_item: &MonoValue,
        instantiation: &Instantiation,
    ) -> FnSigId {
        todo!("monomorphize_fn")
        // if let Some(target_id) = self.mono_fns.get(mono_item).copied() {
        //     return target_id;
        // }
        //
        // let fun = self.hir.fns.iter().find(|f| f.def_id == mono_item.value);
        //
        // if let Some(fun) = fun {
        //     let mut new_fun = fun.clone();
        //     new_fun.subst(&mut ParamFolder { db: self.db, instantiation });
        //
        //     let name = self.mangled_fn_name(fun, instantiation);
        //     let sig = self.lower_fn_sig(
        //         &new_fun.sig,
        //         &new_fun.kind,
        //         name,
        //         mono_item.ty,
        //     );
        //
        //     self.mono_fns.insert(mono_item.clone(), sig);
        //     self.lower_fn_body(sig, &new_fun);
        //
        //     sig
        // } else {
        //     panic!(
        //         "function {} not found in hir.fns",
        //         self.db[mono_item.value].qpath
        //     );
        // }
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
