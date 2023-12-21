use std::collections::VecDeque;

use rustc_hash::FxHashSet;
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
    work: Work,

    // A map of used function ids, pointing to their new function id
    used_fns: FxHashSet<FnSigId>,
    used_globals: FxHashSet<GlobalId>,
    // Functions that have already been specialized
    // specialized_fns: FxHashMap<SpecializedFn, FnSigId>,
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
            // specialized_fns: FxHashMap::default(),
        }
    }

    fn run(&mut self, mir: &mut Mir) {
        let main_fn = mir.main_fn.expect("to have a main fn");
        self.work.push(Job { target: JobTarget::Fn(main_fn) });

        while let Some(job) = self.work.pop() {
            self.do_job(mir, &job);
            self.work.mark_done(job.target);
        }

        mir.fn_sigs.inner_mut().retain(|id, _| self.used_fns.contains(id));
        mir.fns.retain(|id, _| self.used_fns.contains(id));
        mir.globals.inner_mut().retain(|id, _| self.used_globals.contains(id));
    }

    fn do_job(&mut self, mir: &mut Mir, job: &Job) {
        match job.target {
            JobTarget::Fn(id) => self.do_fn_job(mir, id),
            JobTarget::Global(id) => self.do_global_job(mir, id),
        }

        self.work.mark_done(job.target);
    }

    fn do_fn_job(&mut self, mir: &mut Mir, id: FnSigId) {
        self.used_fns.insert(id);

        let Some(fun) = mir.fns.get_mut(&id) else {
            debug_assert!(
                mir.fn_sigs.contains_key(&id),
                "function must have an existing signature"
            );
            return;
        };

        SpecializeBody { cx: self, body: &mut fun.body }.run();
    }

    fn do_global_job(&mut self, mir: &mut Mir, id: GlobalId) {
        self.used_globals.insert(id);
        let global = mir.globals.get_mut(&id).expect("global to exist");
        if let GlobalKind::Static(body, _) = &mut global.kind {
            SpecializeBody { cx: self, body }.run();
        }
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
    body: &'cx mut Body,
}

impl<'db, 'cx> SpecializeBody<'db, 'cx> {
    fn run(self) {
        for value in self.body.values() {
            match value.kind {
                ValueKind::Fn(id) => {
                    self.cx.work.push(Job { target: JobTarget::Fn(id) });
                }
                ValueKind::Global(id) => {
                    self.cx.work.push(Job { target: JobTarget::Global(id) });
                }
                _ => (),
            }
        }
    }

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
}
