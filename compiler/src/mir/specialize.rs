use std::{collections::VecDeque, mem};

use data_structures::index_vec::Key as _;
use rustc_hash::FxHashSet;
use ustr::ustr;

use crate::{
    db::{AdtField, AdtKind, Db, DefId, StructDef, UnionDef},
    mangle,
    middle::{Mutability, NamePat, Pat, Vis},
    mir::{
        BlockId, Body, Const, Fn, FnParam, FnSig, FnSigId, FxHashMap, GlobalId,
        GlobalKind, IdMap, Inst, Mir, StaticGlobal, ValueId, ValueKind,
    },
    span::{Span, Spanned as _},
    subst::{Subst, SubstTy},
    ty::{fold::TyFolder, FnTy, FnTyParam, Instantiation, Ty, TyKind},
    word::Word,
};

pub fn specialize(db: &Db, mir: &mut Mir) {
    Specialize::new(db).run(mir);
    ExpandDestroys::new(db, mir).run(mir);
}

struct Specialize<'db> {
    db: &'db Db,
    work: Work,
    used_fns: FxHashSet<FnSigId>,
    used_globals: FxHashSet<GlobalId>,

    // Functions that have already been specialized and should be re-used
    specialized_fns: FxHashMap<SpecializedFn, FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpecializedFn {
    pub id: FnSigId,
    pub ty: Ty,
}

impl<'db> Specialize<'db> {
    fn new(db: &'db Db) -> Self {
        Self {
            db,
            work: Work::new(),
            used_fns: FxHashSet::default(),
            used_globals: FxHashSet::default(),
            specialized_fns: FxHashMap::default(),
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
    smir: SpecializedMir,
}

impl<'db, 'cx> SpecializeBody<'db, 'cx> {
    fn new(cx: &'cx mut Specialize<'db>, mir: &Mir) -> Self {
        Self { cx, smir: SpecializedMir::new(mir) }
    }

    fn run(mut self, mir: &mut Mir, job: &Job) {
        self.cx.mark_used(job.target);
        self.specialize(mir, job.target);
        self.smir.merge_into(mir);
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
                if let Some(new_kind) =
                    self.specialize_value(mir, &value.kind, instantiation)
                {
                    body_subst.insert_value(value.id, new_kind);
                }
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
    ) -> Option<ValueKind> {
        match value_kind {
            &ValueKind::Fn(id) => {
                let ty = instantiation.fold(mir.fn_sigs[id].ty);

                let specialized_fn = SpecializedFn { id, ty };

                let specialized_sig_id =
                    self.specialize_fn(mir, specialized_fn, instantiation);

                Some(ValueKind::Fn(specialized_sig_id))
            }
            _ => {
                // This is a polymorphic type. Doesn't require specialization...
                None
            }
        }
    }

    #[must_use]
    fn specialize_fn(
        &mut self,
        mir: &Mir,
        specialized_fn: SpecializedFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        if let Some(sig_id) =
            self.cx.specialized_fns.get(&specialized_fn).copied()
        {
            return sig_id;
        }

        let old_sig_id = specialized_fn.id;
        let new_sig_id =
            self.specialize_fn_sig(mir, &specialized_fn, instantiation);

        self.cx.specialized_fns.insert(specialized_fn, new_sig_id);

        let mut fun = mir.fns.get(&old_sig_id).expect("fn to exist").clone();

        fun.sig = new_sig_id;
        fun.subst(&mut SpecializeParamFolder { instantiation });

        self.smir.fns.insert(new_sig_id, fun);
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
        sig.subst(&mut SpecializeParamFolder { instantiation });

        let instantation_str = instantiation
            .tys()
            .map(|ty| mangle::mangle_ty_name(self.cx.db, ty))
            .collect::<Vec<_>>()
            .join("_");

        sig.mangled_name =
            ustr(&format!("{}__{}", sig.mangled_name, instantation_str));

        self.smir.fn_sigs.insert_with_key(|id| {
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
}

impl SpecializedMir {
    fn new(mir: &Mir) -> Self {
        Self { fn_sigs: Self::fn_sigs_from_mir(mir), fns: FxHashMap::default() }
    }

    fn merge_into(&mut self, mir: &mut Mir) {
        mir.fn_sigs.extend(mem::take(&mut self.fn_sigs));
        mir.fns.extend(mem::take(&mut self.fns));
    }

    fn fn_sigs_from_mir(mir: &Mir) -> IdMap<FnSigId, FnSig> {
        IdMap::new_with_counter(*mir.fn_sigs.counter())
    }
}

#[derive(Debug)]
struct ExpandDestroys<'db> {
    db: &'db Db,
    smir: SpecializedMir,

    // Generated free functions for adt types
    adt_frees: FxHashMap<Ty, FnSigId>,
}

impl<'db> ExpandDestroys<'db> {
    fn new(db: &'db Db, mir: &Mir) -> Self {
        Self {
            db,
            smir: SpecializedMir::new(mir),
            adt_frees: FxHashMap::default(),
        }
    }

    fn run(mut self, mir: &mut Mir) {
        for fun in mir.fns.values_mut() {
            self.body(&mut fun.body);
        }

        for global in mir.globals.values_mut() {
            if let GlobalKind::Static(StaticGlobal { body, .. }) =
                &mut global.kind
            {
                self.body(body);
            }
        }

        self.smir.merge_into(mir);
    }

    fn body(&mut self, body: &mut Body) {
        self.remove_unused_destroys(body);
        self.expand_destroy_glue(body);
    }

    fn remove_unused_destroys(&self, body: &mut Body) {
        let value_tys: FxHashMap<ValueId, Ty> =
            body.values().iter().map(|v| (v.id, v.ty)).collect();

        for block in body.blocks_mut() {
            block.insts.retain_mut(|inst| match inst {
                Inst::Free { value, .. } => {
                    let ty = value_tys[&*value];

                    if !self.should_destroy_ty(ty) {
                        return false;
                    }

                    if ty.is_ref() {
                        *inst = Inst::DecRef { value: *value };
                    }

                    true
                }
                Inst::IncRef { value, .. } | Inst::DecRef { value, .. } => {
                    self.should_refcount_ty(value_tys[&*value])
                }
                _ => true,
            });
        }
    }

    fn should_destroy_ty(&self, ty: Ty) -> bool {
        ty.is_move(self.db) || self.should_refcount_ty(ty)
    }

    fn should_refcount_ty(&self, ty: Ty) -> bool {
        match ty.kind() {
            TyKind::Ref(ty, _) => ty.is_move(self.db),
            _ => false,
        }
    }

    fn expand_destroy_glue(&mut self, body: &mut Body) {
        let mut expanded: Vec<(BlockId, usize, FnSigId)> = vec![];

        for block in body.blocks() {
            for (idx, inst) in block.insts.iter().enumerate() {
                match inst {
                    Inst::Free { value, destroy_glue, .. } if *destroy_glue => {
                        let ty = body.value(*value).ty;
                        let free_fn = self.get_or_create_free_fn(ty);
                        expanded.push((block.id, idx, free_fn));
                    }
                    _ => (),
                }
            }
        }

        for (block, inst_idx, free_fn) in expanded {
            let free_fn_ty = self.smir.fn_sigs[free_fn].ty;
            let result = body.create_register(self.db.types.unit);
            let callee = body.create_value(free_fn_ty, ValueKind::Fn(free_fn));

            let Inst::Free { value, .. } = body.block(block).insts[inst_idx]
            else {
                unreachable!()
            };

            // TODO: location param
            body.block_mut(block).insts[inst_idx] =
                Inst::Call { value: result, callee, args: vec![value] };
        }
    }

    fn get_free_call_values(
        &mut self,
        body: &mut Body,
        ty: Ty,
    ) -> (ValueId, ValueId) {
        let free_fn = self.get_or_create_free_fn(ty);

        let free_fn_ty = self.smir.fn_sigs[free_fn].ty;
        let result = body.create_register(self.db.types.unit);
        let callee = body.create_value(free_fn_ty, ValueKind::Fn(free_fn));

        (result, callee)
    }

    fn get_or_create_free_fn(&mut self, ty: Ty) -> FnSigId {
        match ty.kind() {
            TyKind::Adt(..) => self.get_or_create_adt_free(ty),
            ty => unreachable!("unexpected ty {ty:?}"),
        }
    }

    fn get_or_create_adt_free(&mut self, ty: Ty) -> FnSigId {
        if let Some(sig_id) = self.adt_frees.get(&ty) {
            return *sig_id;
        }

        let sig_id = CreateAdtFree::new(self).create(ty);
        sig_id
    }
}

struct SpecializeParamFolder<'a> {
    pub instantiation: &'a Instantiation,
}

impl SubstTy for SpecializeParamFolder<'_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }
}

impl TyFolder for SpecializeParamFolder<'_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => match self.instantiation.get(p.var) {
                Some(ty) => ty,
                None => ty,
            },
            _ => self.super_fold(ty),
        }
    }
}

#[derive(Debug)]
struct CreateAdtFree<'cx, 'db> {
    cx: &'cx mut ExpandDestroys<'db>,
    body: Body,
}

impl<'cx, 'db> CreateAdtFree<'cx, 'db> {
    fn new(cx: &'cx mut ExpandDestroys<'db>) -> Self {
        Self { cx, body: Body::new() }
    }

    fn create(mut self, ty: Ty) -> FnSigId {
        let TyKind::Adt(adt_id, targs) = ty.kind() else { unreachable!() };

        let adt = &self.cx.db[*adt_id];
        let instantiation = adt.instantiation(targs);
        let adt_ty = instantiation.fold(adt.ty());

        let adt_name = mangle::mangle_adt(self.cx.db, adt, targs);
        let self_name = ustr("self");

        let params = vec![FnParam {
            pat: Pat::Name(NamePat {
                id: DefId::null(),
                word: Word::new(self_name, Span::unknown()),
                vis: Vis::Private,
                mutability: Mutability::Imm,
                ty: adt_ty,
            }),
            ty: adt_ty,
        }];

        let fn_ty = Ty::new(TyKind::Fn(FnTy {
            params: params
                .iter()
                .map(|p| FnTyParam {
                    name: Some(p.pat.name().unwrap()),
                    ty: p.ty,
                })
                .collect(),
            ret: self.cx.db.types.unit,
            is_c_variadic: false,
        }));

        let mangled_name = ustr(&format!("{adt_name}_destroy"));
        let display_name = ustr(
            &self.cx.db[adt.def_id]
                .qpath
                .clone()
                .child(ustr("$destroy"))
                .join(),
        );
        let sig = self.cx.smir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            mangled_name,
            display_name,
            params,
            ty: fn_ty,
            is_extern: false,
            is_c_variadic: false,
            span: adt.name.span(),
        });
        self.cx.adt_frees.insert(ty, sig);

        let self_value =
            self.body.create_value(adt_ty, ValueKind::UniqueName(self_name));

        let join_block = match &adt.kind {
            AdtKind::Union(union_def) => {
                self.lower_union_free(union_def, self_value, &instantiation)
            }
            AdtKind::Struct(struct_def) => {
                self.lower_struct_free(struct_def, self_value, &instantiation)
            }
        };

        let unit_value = self
            .body
            .create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit));
        // TODO: location param
        self.body
            .ins(join_block)
            .free(self_value, false, adt.name.span())
            .ret(unit_value);

        self.cx.smir.fns.insert(sig, Fn { sig, body: self.body });

        sig
    }

    fn lower_struct_free(
        &mut self,
        struct_def: &StructDef,
        self_value: ValueId,
        instantiation: &Instantiation,
    ) -> BlockId {
        let start_block = self.body.create_block("start");

        self.free_adt_fields(
            start_block,
            self_value,
            &struct_def.fields,
            instantiation,
        );

        start_block
    }

    fn lower_union_free(
        &mut self,
        union_def: &UnionDef,
        self_value: ValueId,
        instantiation: &Instantiation,
    ) -> BlockId {
        let start_block = self.body.create_block("start");
        let join_block = self.body.create_block("join");
        let self_ty = self.body.value(self_value).ty;

        let mut blocks = vec![];

        for &variant_id in &union_def.variants {
            let variant = &self.cx.db[variant_id];
            let name = variant.name.name();

            let variant_value = self
                .body
                .create_value(self_ty, ValueKind::Variant(self_value, name));

            let block = self.body.create_block(format!("case_{name}"));

            self.free_adt_fields(
                block,
                variant_value,
                &variant.fields,
                instantiation,
            );

            self.body.ins(block).br(join_block);
            blocks.push(block);
        }

        let uint = self.cx.db.types.uint;
        let tag_field = self
            .body
            .create_value(uint, ValueKind::Field(self_value, ustr("tag")));
        self.body.ins(start_block).switch(tag_field, blocks);

        join_block
    }

    fn free_adt_fields(
        &mut self,
        block: BlockId,
        self_value: ValueId,
        fields: &[AdtField],
        instantiation: &Instantiation,
    ) {
        for field in fields {
            let ty = instantiation.fold(field.ty);
            let field_value = self.body.create_value(
                field.ty,
                ValueKind::Field(self_value, field.name.name()),
            );

            match ty.kind() {
                TyKind::Adt(..) => {
                    let (result, callee) =
                        self.cx.get_free_call_values(&mut self.body, ty);

                    // TODO: location param
                    self.body.ins(block).call(
                        result,
                        callee,
                        vec![field_value],
                    );
                }
                TyKind::Ref(..) if self.cx.should_refcount_ty(ty) => {
                    self.body.ins(block).decref(field_value);
                }
                _ => (),
            }
        }
    }
}
