use std::{collections::VecDeque, mem};

use data_structures::index_vec::Key;
use rustc_hash::FxHashSet;
use ustr::{ustr, Ustr};

use crate::{
    db::{AdtField, AdtKind, Db, DefId, StructDef, UnionDef},
    mangle,
    middle::{BinOp, CallConv, CmpOp, Mutability, NamePat, Pat},
    mir::{
        BlockId, Body, Const, Fn, FnParam, FnSig, FnSigId, FxHashMap, GlobalId, GlobalKind, IdMap,
        Inst, Mir, StaticGlobal, ValueId, ValueKind,
    },
    span::{Span, Spanned as _},
    subst::{Subst, SubstTy},
    sym,
    ty::{fold::TyFolder, FnTy, FnTyFlags, FnTyParam, Instantiation, Ty, TyKind},
    word::Word,
};

pub fn monomorphize(db: &Db, mir: &mut Mir) {
    Monomorphize::new(db).run(mir);
    ExpandDestroys::new(db, mir).run(mir);
}

struct Monomorphize<'db> {
    db: &'db Db,
    work: Work,
    used_fns: FxHashSet<FnSigId>,
    used_globals: FxHashSet<GlobalId>,

    // Functions that have already been monomorphized and should be re-used
    monomorphized_fns: FxHashMap<MonoFn, FnSigId>,

    // Functions that are a result of monomorphization, and should never be monomorphized again
    dont_monomorphize: FxHashSet<FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoFn {
    pub id: FnSigId,
    pub ty: Ty,
}

impl<'db> Monomorphize<'db> {
    fn new(db: &'db Db) -> Self {
        Self {
            db,
            work: Work::new(),
            used_fns: FxHashSet::default(),
            used_globals: FxHashSet::default(),
            monomorphized_fns: FxHashMap::default(),
            dont_monomorphize: FxHashSet::default(),
        }
    }

    fn run(mut self, mir: &mut Mir) {
        self.monomorphize_bodies(mir);
        self.retain_used_mir(mir);
    }

    fn monomorphize_bodies(&mut self, mir: &mut Mir) {
        let main_fn = mir.main_fn.expect("to have a main fn");
        self.work.push(Job { target: JobTarget::Fn(main_fn) });

        while let Some(job) = self.work.pop() {
            MonomorphizeBody::new(self, mir).run(mir, &job);
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

struct MonomorphizeBody<'db, 'cx> {
    cx: &'cx mut Monomorphize<'db>,
    mono_mir: MonoMir,
}

impl<'db, 'cx> MonomorphizeBody<'db, 'cx> {
    fn new(cx: &'cx mut Monomorphize<'db>, mir: &Mir) -> Self {
        Self { cx, mono_mir: MonoMir::new(mir) }
    }

    fn run(mut self, mir: &mut Mir, job: &Job) {
        self.cx.mark_used(job.target);
        self.monomorphize(mir, job.target);
        self.mono_mir.merge_into(mir);
        self.cx.work.mark_done(job.target);
    }

    fn monomorphize(&mut self, mir: &mut Mir, target: JobTarget) {
        match target {
            JobTarget::Fn(id) => {
                let Some(fun) = mir.fns.get(&id) else {
                    debug_assert!(
                        mir.fn_sigs.contains_key(&id),
                        "function must have an existing signature"
                    );
                    return;
                };

                let body_subst = self.monomorphize_body(mir, &fun.body);
                body_subst.subst(&mut mir.fns.get_mut(&id).unwrap().body);
            }
            JobTarget::Global(id) => {
                let global = mir.globals.get(&id).expect("global to exist");

                if let GlobalKind::Static(StaticGlobal { body, .. }) = &global.kind {
                    let body_subst = self.monomorphize_body(mir, body);
                    let body_mut = &mut mir.globals[id].kind.as_static_mut().unwrap().body;
                    body_subst.subst(body_mut);
                }
            }
        }
    }

    fn monomorphize_body(&mut self, mir: &Mir, body: &Body) -> BodySubst {
        let mut body_subst = BodySubst::new();

        for value in body.values() {
            if let Some(instantiation) = body.instantation(value.id) {
                // This is a polymorphic value which requires specialization
                if let Some(new_kind) = self.monomorphize_value(mir, &value.kind, instantiation) {
                    body_subst.insert_value(value.id, new_kind);
                }
            } else {
                // This is a monomorphic value, we can enqueue it safely
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

        body_subst
    }

    fn monomorphize_value(
        &mut self,
        mir: &Mir,
        value_kind: &ValueKind,
        instantiation: &Instantiation,
    ) -> Option<ValueKind> {
        match value_kind {
            &ValueKind::Fn(id) => {
                // We ignore builtin functions, as they're expanded during mir::lower
                if self.cx.db.builtins.contains_key(&mir.fn_sigs[id].def_id) {
                    return None;
                }

                if self.cx.dont_monomorphize.contains(&id) {
                    return None;
                }

                let ty = instantiation.fold(mir.fn_sigs[id].ty);
                let monomorphized_fn = MonoFn { id, ty };

                let monomorphized_sig_id =
                    self.monomorphize_fn(mir, monomorphized_fn, instantiation);

                Some(ValueKind::Fn(monomorphized_sig_id))
            }
            _ => {
                // This is a polymorphic type. Doesn't require specialization...
                None
            }
        }
    }

    #[must_use]
    fn monomorphize_fn(
        &mut self,
        mir: &Mir,
        monomorphized_fn: MonoFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        if let Some(sig_id) = self.cx.monomorphized_fns.get(&monomorphized_fn).copied() {
            return sig_id;
        }

        let old_sig_id = monomorphized_fn.id;
        let new_sig_id = self.monomorphize_fn_sig(mir, &monomorphized_fn, instantiation);

        self.cx.monomorphized_fns.insert(monomorphized_fn, new_sig_id);
        self.cx.dont_monomorphize.insert(new_sig_id);

        let mut fun = mir.fns.get(&old_sig_id).expect("fn to exist").clone();

        fun.sig = new_sig_id;
        fun.subst(&mut MonoParamFolder { instantiation });

        self.mono_mir.fns.insert(new_sig_id, fun);
        self.cx.work.push(Job { target: JobTarget::Fn(new_sig_id) });

        new_sig_id
    }

    #[must_use]
    fn monomorphize_fn_sig(
        &mut self,
        mir: &Mir,
        monomorphized_fn: &MonoFn,
        instantiation: &Instantiation,
    ) -> FnSigId {
        let mut sig = mir.fn_sigs[monomorphized_fn.id].clone();
        sig.subst(&mut MonoParamFolder { instantiation });

        let instantation_str = instantiation
            .tys()
            .map(|ty| mangle::mangle_ty_name(self.cx.db, ty))
            .collect::<Vec<_>>()
            .join("_");

        sig.mangled_name = ustr(&format!("{}$${}", sig.mangled_name, instantation_str));

        self.mono_mir.fn_sigs.insert_with_key(|id| {
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
struct MonoMir {
    fn_sigs: IdMap<FnSigId, FnSig>,
    fns: FxHashMap<FnSigId, Fn>,
}

impl MonoMir {
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
    mono_mir: MonoMir,

    // Generated free functions for adt types
    free_fns: FxHashMap<Ty, FnSigId>,
}

impl<'db> ExpandDestroys<'db> {
    fn new(db: &'db Db, mir: &Mir) -> Self {
        Self { db, mono_mir: MonoMir::new(mir), free_fns: FxHashMap::default() }
    }

    fn run(mut self, mir: &mut Mir) {
        for fun in mir.fns.values_mut() {
            self.body(&mut fun.body);
        }

        for global in mir.globals.values_mut() {
            if let GlobalKind::Static(StaticGlobal { body, .. }) = &mut global.kind {
                self.body(body);
            }
        }

        self.mono_mir.merge_into(mir);
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
                Inst::Destroy { value, destroy_glue, .. } => {
                    let ty = value_tys[&*value];

                    if !self.should_destroy_ty(ty, *destroy_glue) {
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

    fn should_destroy_ty(&self, ty: Ty, destroy_glue: bool) -> bool {
        if self.should_refcount_ty(ty) {
            return true;
        }

        if !ty.is_rc(self.db) && !destroy_glue {
            return false;
        }

        ty.needs_free(self.db)
    }

    fn should_refcount_ty(&self, ty: Ty) -> bool {
        matches!(ty.kind(), TyKind::Ref(ty, _) if ty.is_rc(self.db))
    }

    fn expand_destroy_glue(&mut self, body: &mut Body) {
        let mut expanded: Vec<(BlockId, usize, Option<FnSigId>)> = vec![];

        for block in body.blocks() {
            for (idx, inst) in block.insts.iter().enumerate() {
                if let Inst::Destroy { value, destroy_glue, .. } = inst {
                    let free_fn = if *destroy_glue {
                        let ty = body.value(*value).ty;
                        Some(self.get_or_create_free_fn(ty))
                    } else {
                        None
                    };
                    expanded.push((block.id, idx, free_fn));
                }
            }
        }

        for (block, inst_idx, free_fn) in expanded {
            let Inst::Destroy { value, span, .. } = body.block(block).insts[inst_idx] else {
                unreachable!()
            };

            if let Some(free_fn) = free_fn {
                let free_fn_ty = self.mono_mir.fn_sigs[free_fn].ty;
                let result = body.create_register(self.db.types.unit);
                let callee = body.create_value(free_fn_ty, ValueKind::Fn(free_fn));

                body.block_mut(block).insts[inst_idx] =
                    Inst::Call { value: result, callee, args: vec![value], span };
            } else {
                body.block_mut(block).insts[inst_idx] = Inst::Free { value, traced: true, span };
            }
        }
    }

    fn get_free_call_values(&mut self, body: &mut Body, ty: Ty) -> (ValueId, ValueId) {
        let free_fn = self.get_or_create_free_fn(ty);

        let free_fn_ty = self.mono_mir.fn_sigs[free_fn].ty;
        let result = body.create_register(self.db.types.unit);
        let callee = body.create_value(free_fn_ty, ValueKind::Fn(free_fn));

        (result, callee)
    }

    fn get_or_create_free_fn(&mut self, ty: Ty) -> FnSigId {
        if let Some(sig_id) = self.free_fns.get(&ty) {
            return *sig_id;
        }

        match ty.kind() {
            TyKind::Adt(..) => CreateAdtFree::new(self).create(ty),
            TyKind::Slice(..) => CreateSliceFree::new(self).create(ty),
            ty => unreachable!("unexpected ty {ty:?}"),
        }
    }
}

struct MonoParamFolder<'a> {
    pub instantiation: &'a Instantiation,
}

impl SubstTy for MonoParamFolder<'_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }
}

impl TyFolder for MonoParamFolder<'_> {
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
        let adt_span = adt.name.span();

        let adt_name = mangle::mangle_adt(self.cx.db, adt, targs);
        let mangled_name = ustr(&format!("{adt_name}_destroy"));
        let display_name =
            ustr(&self.cx.db[adt.def_id].qpath.clone().child(ustr("$destroy")).join());
        let sig = create_destroy_sig(self.cx, adt_ty, mangled_name, display_name, adt_span);

        let self_value = self.body.create_value(adt_ty, ValueKind::Param(DefId::null(), 0));

        let join_block = match &adt.kind {
            AdtKind::Union(union_def) => {
                self.lower_union_free(union_def, self_value, &instantiation, adt_span)
            }
            AdtKind::Struct(struct_def) => {
                self.lower_struct_free(struct_def, self_value, &instantiation, adt_span)
            }
        };

        if adt.is_ref() {
            self.body.ins(join_block).free(self_value, false, adt_span);
        }

        let unit_value =
            self.body.create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit));
        self.body.ins(join_block).ret(unit_value);

        self.cx.mono_mir.fns.insert(sig, Fn { sig, body: self.body });

        sig
    }

    fn lower_struct_free(
        &mut self,
        struct_def: &StructDef,
        self_value: ValueId,
        instantiation: &Instantiation,
        span: Span,
    ) -> BlockId {
        let start_block = self.body.create_block("start");
        self.free_adt_fields(start_block, self_value, &struct_def.fields, instantiation, span);
        start_block
    }

    fn lower_union_free(
        &mut self,
        union_def: &UnionDef,
        self_value: ValueId,
        instantiation: &Instantiation,
        span: Span,
    ) -> BlockId {
        let start_block = self.body.create_block("start");
        let join_block = self.body.create_block("join");
        let self_ty = self.body.value(self_value).ty;

        let mut blocks = vec![];

        for &variant_id in &union_def.variants {
            let variant = &self.cx.db[variant_id];

            let variant_value =
                self.body.create_value(self_ty, ValueKind::Variant(self_value, variant.id));

            let block = self.body.create_block(format!("case_{}", variant.name));

            self.free_adt_fields(block, variant_value, &variant.fields, instantiation, span);

            self.body.ins(block).br(join_block);
            blocks.push(block);
        }

        let uint = self.cx.db.types.uint;
        let tag_field = self.body.create_value(uint, ValueKind::Field(self_value, ustr("tag")));
        self.body.ins(start_block).switch(tag_field, blocks);

        join_block
    }

    fn free_adt_fields(
        &mut self,
        block: BlockId,
        self_value: ValueId,
        fields: &[AdtField],
        instantiation: &Instantiation,
        span: Span,
    ) {
        for field in fields {
            let ty = instantiation.fold(field.ty);
            let field_value =
                self.body.create_value(field.ty, ValueKind::Field(self_value, field.name.name()));

            match ty.kind() {
                TyKind::Adt(..) => {
                    let (result, callee) = self.cx.get_free_call_values(&mut self.body, ty);

                    self.body.ins(block).call(result, callee, vec![field_value], span);
                }
                TyKind::Ref(..) if self.cx.should_refcount_ty(ty) => {
                    self.body.ins(block).decref(field_value);
                }
                _ => (),
            }
        }
    }
}

#[derive(Debug)]
struct CreateSliceFree<'cx, 'db> {
    cx: &'cx mut ExpandDestroys<'db>,
    body: Body,
}

impl<'cx, 'db> CreateSliceFree<'cx, 'db> {
    fn new(cx: &'cx mut ExpandDestroys<'db>) -> Self {
        Self { cx, body: Body::new() }
    }

    fn create(mut self, ty: Ty) -> FnSigId {
        let &TyKind::Slice(elem_ty) = ty.kind() else { unreachable!() };

        let main_span = Span::uniform(self.cx.db.main_source_id(), 0);

        let tyname = mangle::mangle_ty_name(self.cx.db, ty);
        let mangled_name = ustr(&format!("{tyname}_destroy"));
        let display_name = ustr(&format!("{}$destroy", ty.display(self.cx.db)));
        let sig = create_destroy_sig(self.cx, ty, mangled_name, display_name, main_span);

        let slice = self.body.create_value(ty, ValueKind::Param(DefId::null(), 0));

        let start = self.body.create_block("start");

        let end_block = if self.cx.should_destroy_ty(elem_ty, true) {
            self.free_slice_elems(start, slice, elem_ty, main_span)
        } else {
            start
        };

        let unit_value =
            self.body.create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit));
        self.body.ins(end_block).free(slice, false, main_span).ret(unit_value);

        self.cx.mono_mir.fns.insert(sig, Fn { sig, body: self.body });

        sig
    }

    // Generates a loop which goes over all slice elements, destroying them.
    // The generated code in psuedo-code:
    //
    // ```rust
    // let i = 0
    // loop {
    //   if i == slice.len { break }
    //   free(slice[i])
    // }
    // ```
    fn free_slice_elems(
        &mut self,
        start: BlockId,
        slice: ValueId,
        elem_ty: Ty,
        span: Span,
    ) -> BlockId {
        let uint = self.cx.db.types.uint;

        let loop_start = self.body.create_block("loop_start");
        let loop_end = self.body.create_block("loop_end");

        // uint i = 0;
        let index = self.body.create_register(uint);
        let zero = self.body.create_value(uint, ValueKind::Const(Const::Int(0)));
        self.body.ins(start).stackalloc(index, zero).br(loop_start);

        // slice.len
        let slice_len = self.body.create_value(uint, ValueKind::Field(slice, ustr(sym::LEN)));

        // i == slice.len
        let cond = self.body.create_register(self.cx.db.types.bool);
        self.body.ins(loop_start).binary_unchecked(cond, index, slice_len, BinOp::Cmp(CmpOp::Eq));

        self.body.ins(loop_start).brif(cond, loop_end, None);
        self.free_elem(loop_start, slice, index, elem_ty, span);

        // i += 1
        let next_index = self.body.create_register(uint);
        let one = self.body.create_value(uint, ValueKind::Const(Const::Int(1)));
        self.body
            .ins(loop_start)
            .binary_unchecked(next_index, index, one, BinOp::Add)
            .store(next_index, index);

        self.body.ins(loop_start).br(loop_start);

        loop_end
    }

    fn free_elem(
        &mut self,
        block: BlockId,
        slice: ValueId,
        index: ValueId,
        elem_ty: Ty,
        span: Span,
    ) {
        let elem = self.body.create_register(elem_ty);
        self.body.ins(block).slice_index_unchecked(elem, slice, index);

        match elem_ty.kind() {
            TyKind::Adt(..) | TyKind::Slice(..) => {
                let (result, callee) = self.cx.get_free_call_values(&mut self.body, elem_ty);

                self.body.ins(block).call(result, callee, vec![elem], span);
            }
            TyKind::Ref(..) if self.cx.should_refcount_ty(elem_ty) => {
                self.body.ins(block).decref(elem);
            }
            ty => unreachable!("{ty:?}"),
        }
    }
}

fn create_destroy_sig(
    cx: &mut ExpandDestroys,
    ty: Ty,
    mangled_name: Ustr,
    display_name: Ustr,
    span: Span,
) -> FnSigId {
    let params = vec![FnParam {
        pat: Pat::Name(NamePat {
            id: DefId::null(),
            word: Word::new(ustr("self"), Span::unknown()),
            mutability: Mutability::Imm,
            ty,
        }),
        ty,
    }];

    let fn_ty_params =
        params.iter().map(|p| FnTyParam { name: Some(p.pat.name().unwrap()), ty: p.ty }).collect();

    let fn_ty = Ty::new(TyKind::Fn(FnTy {
        params: fn_ty_params,
        ret: cx.db.types.unit,
        callconv: CallConv::default(),
        flags: FnTyFlags::empty(),
    }));

    let sig = cx.mono_mir.fn_sigs.insert_with_key(|id| FnSig {
        id,
        def_id: DefId::null(),
        mangled_name,
        display_name,
        params,
        ty: fn_ty,
        is_inline: true,
        span,
    });

    cx.free_fns.insert(ty, sig);

    sig
}
