use std::{collections::VecDeque, mem};

use rustc_hash::FxHashSet;
use ustr::ustr;

use crate::{
    db::Db,
    hir::mangle,
    mir::*,
    subst::{ParamFolder, Subst},
    ty::{fold::TyFolder, Instantiation, Ty, TyKind},
};

pub fn specialize(db: &mut Db, mir: &mut Mir) {
    Specialize::new(db).run(mir);
    ExpandDestroys::new(db).run(mir);
}

struct Specialize<'db> {
    db: &'db mut Db,
    work: Work,
    used_fns: FxHashSet<FnSigId>,
    used_globals: FxHashSet<GlobalId>,
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
            work: Work::new(),
            used_fns: FxHashSet::default(),
            used_globals: FxHashSet::default(),
        }
    }

    fn run(mut self, mir: &mut Mir) {
        self.specialize_bodies(mir);
        self.retain_used_mir(mir);
    }

    fn specialize_bodies(&mut self, mir: &mut Mir) {
        let main_fn = mir.main_fn.expect("to have a main fn");
        self.work.push(Job { target: JobTarget::Fn(main_fn) });

        while let Some(job) = self.work.pop() {
            SpecializeBody::new(self, mir).run(mir, &job);
        }
    }

    fn retain_used_mir(&mut self, mir: &mut Mir) {
        mir.fn_sigs.inner_mut().retain(|id, _| self.used_fns.contains(id));
        mir.fns.retain(|id, _| self.used_fns.contains(id));
        mir.globals.inner_mut().retain(|id, _| self.used_globals.contains(id));
    }

    fn mark_used(&mut self, target: JobTarget) {
        match target {
            JobTarget::Fn(id) => {
                self.used_fns.insert(id);
            }
            JobTarget::Global(id) => {
                self.used_globals.insert(id);
            }
        }
    }
}

#[derive(Debug)]
struct Work {
    queue: VecDeque<Job>,
    done: FxHashSet<JobTarget>,
}

impl Work {
    fn new() -> Self {
        Self { queue: VecDeque::new(), done: FxHashSet::default() }
    }

    pub fn push(&mut self, job: Job) {
        if !self.done.contains(&job.target) {
            self.queue.push_back(job);
        }
    }

    pub fn pop(&mut self) -> Option<Job> {
        self.queue.pop_front()
    }

    pub fn mark_done(&mut self, target: JobTarget) {
        self.done.insert(target);
    }
}

#[derive(Debug)]
struct Job {
    target: JobTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum JobTarget {
    Fn(FnSigId),
    Global(GlobalId),
}

struct SpecializeBody<'db, 'cx> {
    cx: &'cx mut Specialize<'db>,
    specialized_mir: SpecializedMir,
}

impl<'db, 'cx> SpecializeBody<'db, 'cx> {
    fn new(cx: &'cx mut Specialize<'db>, mir: &Mir) -> Self {
        Self { cx, specialized_mir: SpecializedMir::new(mir) }
    }

    fn run(mut self, mir: &mut Mir, job: &Job) {
        self.cx.mark_used(job.target);
        self.specialize(mir, job.target);
        self.specialized_mir.merge_into(mir);
        self.cx.work.mark_done(job.target);
    }

    fn specialize(&mut self, mir: &mut Mir, target: JobTarget) {
        match target {
            JobTarget::Fn(id) => {
                let Some(fun) = mir.fns.get(&id) else {
                    debug_assert!(
                        mir.fn_sigs.contains_key(&id),
                        "function must have an existing signature"
                    );
                    return;
                };

                let body_subst = self.specialize_body(mir, &fun.body);
                body_subst.subst(&mut mir.fns.get_mut(&id).unwrap().body);
            }
            JobTarget::Global(id) => {
                let global = mir.globals.get(&id).expect("global to exist");
                if let GlobalKind::Static(StaticGlobal { body, .. }) =
                    &global.kind
                {
                    let body_subst = self.specialize_body(mir, body);
                    let body_mut =
                        &mut mir.globals[id].kind.as_static_mut().unwrap().body;
                    body_subst.subst(body_mut);
                }
            }
        }
    }

    fn specialize_body(&mut self, mir: &Mir, body: &Body) -> BodySubst {
        let mut body_subst = BodySubst::new();

        for value in body.values() {
            if let Some(instantiation) = body.instantation(value.id) {
                // This is a polymorphic value which requires specialization
                let new_kind =
                    self.specialize_value(mir, &value.kind, instantiation);
                body_subst.insert_value(value.id, new_kind);
            } else {
                // This is a monomorphic value, we can enqueue it safely
                match value.kind {
                    ValueKind::Fn(id) => {
                        self.cx.work.push(Job { target: JobTarget::Fn(id) });
                    }
                    ValueKind::Global(id) => {
                        self.cx
                            .work
                            .push(Job { target: JobTarget::Global(id) });
                    }
                    _ => (),
                }
            }
        }

        body_subst
    }

    fn specialize_value(
        &mut self,
        mir: &Mir,
        value_kind: &ValueKind,
        instantiation: &Instantiation,
    ) -> ValueKind {
        match value_kind {
            &ValueKind::Fn(id) => {
                let ty = ParamFolder { db: self.cx.db, instantiation }
                    .fold(mir.fn_sigs[id].ty);

                let specialized_fn = SpecializedFn { id, ty };

                let specialized_sig_id =
                    self.specialize_fn(mir, specialized_fn, instantiation);

                ValueKind::Fn(specialized_sig_id)
            }
            kind => unreachable!(
                "unexpected value kind in specialization: {kind:?}"
            ),
        }
    }

    #[must_use]
    fn specialize_fn(
        &mut self,
        mir: &Mir,
        specialized_fn: SpecializedFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        if let Some(target_value) =
            self.specialized_mir.specialized_fns.get(&specialized_fn).copied()
        {
            return target_value;
        }

        let old_sig_id = specialized_fn.id;
        let new_sig_id =
            self.specialize_fn_sig(mir, &specialized_fn, instantiation);

        self.specialized_mir.specialized_fns.insert(specialized_fn, new_sig_id);

        let mut fun = mir.fns.get(&old_sig_id).expect("fn to exist").clone();

        fun.sig = new_sig_id;
        fun.subst(&mut ParamFolder { db: self.cx.db, instantiation });

        self.specialized_mir.fns.insert(new_sig_id, fun);
        self.cx.work.push(Job { target: JobTarget::Fn(new_sig_id) });

        new_sig_id
    }

    #[must_use]
    fn specialize_fn_sig(
        &mut self,
        mir: &Mir,
        specialized_fn: &SpecializedFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        let mut sig = mir.fn_sigs[specialized_fn.id].clone();
        sig.subst(&mut ParamFolder { db: self.cx.db, instantiation });

        let instantation_str = instantiation
            .tys()
            .map(|ty| mangle::mangle_ty_name(self.cx.db, ty))
            .collect::<Vec<_>>()
            .join("_");

        sig.name = ustr(&format!("{}__{}", sig.name, instantation_str));

        self.specialized_mir.fn_sigs.insert_with_key(|id| {
            sig.id = id;
            sig
        })
    }
}

#[derive(Debug)]
struct BodySubst {
    value_subst: FxHashMap<ValueId, ValueKind>,
}

impl BodySubst {
    fn new() -> Self {
        Self { value_subst: FxHashMap::default() }
    }

    fn insert_value(&mut self, id: ValueId, kind: ValueKind) {
        self.value_subst.insert(id, kind);
    }

    fn subst(self, body: &mut Body) {
        for (id, kind) in self.value_subst {
            body.value_mut(id).kind = kind;
        }
    }
}

#[derive(Debug)]
struct SpecializedMir {
    fn_sigs: IdMap<FnSigId, FnSig>,
    fns: FxHashMap<FnSigId, Fn>,

    // Functions that have already been specialized and should be re-used
    specialized_fns: FxHashMap<SpecializedFn, FnSigId>,
}

impl SpecializedMir {
    fn new(mir: &Mir) -> Self {
        Self {
            fn_sigs: IdMap::new_with_counter(*mir.fn_sigs.counter()),
            fns: FxHashMap::default(),
            specialized_fns: FxHashMap::default(),
        }
    }

    fn merge_into(&mut self, mir: &mut Mir) {
        let fn_sigs_counter = *self.fn_sigs.counter();
        mir.fn_sigs.extend(mem::take(&mut self.fn_sigs));
        self.fn_sigs.set_counter(fn_sigs_counter);
        mir.fns.extend(mem::take(&mut self.fns));
    }
}

struct ExpandDestroys<'db> {
    db: &'db Db,
}

impl<'db> ExpandDestroys<'db> {
    fn new(db: &'db Db) -> Self {
        Self { db }
    }

    fn run(self, mir: &mut Mir) {
        for fun in mir.fns.values_mut() {
            self.remove_unused_destroys(&mut fun.body);
        }

        for global in mir.globals.values_mut() {
            if let GlobalKind::Static(StaticGlobal { body, .. }) =
                &mut global.kind
            {
                self.remove_unused_destroys(body);
            }
        }
    }

    fn remove_unused_destroys(&self, body: &mut Body) {
        let mut destroyed_values = FxHashSet::<ValueId>::default();
        let mut used_destroy_flags: FxHashMap<ValueId, bool> =
            body.destroy_flags.values().map(|flag| (*flag, false)).collect();

        for block_id in body.blocks().keys() {
            destroyed_values.extend(
                body.block(block_id).insts.iter().filter_map(
                    |inst| match inst {
                        Inst::Free { value, destroy_flag, .. } => {
                            let should_destroy =
                                self.should_destroy_value(body, *value);

                            if should_destroy {
                                if let Some(destroy_flag) = destroy_flag {
                                    used_destroy_flags
                                        .insert(*destroy_flag, true);
                                }

                                Some(*value)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                ),
            );
        }

        for block in body.blocks_mut() {
            block.insts.retain(|inst| match inst {
                Inst::Local { value, .. }
                | Inst::Store { target: value, .. } => {
                    used_destroy_flags.get(value).copied().unwrap_or(true)
                }
                Inst::Free { value, .. } => destroyed_values.contains(value),
                _ => true,
            });
        }
    }

    fn should_destroy_value(&self, body: &Body, value: ValueId) -> bool {
        self.should_destroy_ty(body.value(value).ty)
    }

    fn should_destroy_ty(&self, ty: Ty) -> bool {
        match ty.kind() {
            TyKind::Struct(sid) => self.db[*sid].kind.is_ref(),
            _ => false,
        }
    }
}
