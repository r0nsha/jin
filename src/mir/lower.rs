use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    hir::{const_eval::Const, FnKind, Hir},
    index_vec::IndexVecExt,
    mir::*,
    subst::{ParamFolder, Subst},
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

pub fn lower(db: &mut Db, hir: &Hir) -> Mir {
    LowerCx::new(db, hir).lower_all()
}

struct LowerCx<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    mir: Mir,

    // Maps functions to their lowered signatures
    fn_map: FxHashMap<DefId, FnSigId>,

    // Already monomorphized functions
    mono_fns: FxHashMap<MonoItem, FnSigId>,

    // Maps global lets to their lowered globals
    globals_map: FxHashMap<DefId, GlobalId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoItem {
    pub id: DefId,
    pub ty: Ty,
}

impl<'db> LowerCx<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir) -> Self {
        Self {
            db,
            hir,
            mir: Mir::new(),
            fn_map: FxHashMap::default(),
            mono_fns: FxHashMap::default(),
            globals_map: FxHashMap::default(),
        }
    }

    fn lower_all(mut self) -> Mir {
        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let def = &self.db[f.id];
                let is_extern = f.kind.is_extern();
                let name = if is_extern { def.name } else { def.qpath.join_with("_").into() };
                let sig = self.lower_fn_sig(&f.sig, name, def.ty, is_extern);
                self.fn_map.insert(f.id, sig);
            }
        }

        for let_ in &self.hir.lets {
            if !let_.pat.any(|name| self.globals_map.get(&name.id).is_some()) {
                self.lower_global_let(let_);
            }
        }

        for let_ in &self.hir.extern_lets {
            let id = self.mir.globals.push_with_key(|id| Global {
                id,
                def_id: let_.id,
                name: let_.word.name(),
                ty: self.db[let_.id].ty,
                kind: GlobalKind::Extern,
            });

            self.globals_map.insert(let_.id, id);
        }

        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let sig = self.fn_map[&f.id];
                self.lower_fn_body(sig, f);
            }
        }

        self.mir
    }

    fn lower_global(&mut self, def_id: DefId) -> GlobalId {
        if let Some(target_id) = self.globals_map.get(&def_id).copied() {
            return target_id;
        }

        let let_ = self.hir.lets.iter().find(|let_| match &let_.pat {
            hir::Pat::Name(n) => n.id == def_id,
            hir::Pat::Discard(_) => false,
        });

        if let Some(let_) = let_ {
            self.lower_global_let(let_).expect("to output a GlobalId")
        } else {
            panic!("global let {} not found in hir.lets", self.db[def_id].qpath);
        }
    }

    fn lower_global_let(&mut self, let_: &hir::Let) -> Option<GlobalId> {
        LowerBodyCx::new(self).lower_global(let_)
    }

    fn monomorphize_fn(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> FnSigId {
        if let Some(target_id) = self.mono_fns.get(mono_item).copied() {
            return target_id;
        }

        let fun = self.hir.fns.iter().find(|f| f.id == mono_item.id);

        if let Some(fun) = fun {
            let mut new_fun = fun.clone();
            new_fun.subst(&mut ParamFolder { db: self.db, instantiation });

            let name = {
                let args_str = instantiation
                    .values()
                    .map(|t| t.to_string(self.db))
                    .collect::<Vec<String>>()
                    .join("_");

                let def = &self.db[fun.id];

                let name = def
                    .qpath
                    .clone()
                    .with_name(ustr(&format!("{}_{}", def.name, args_str)))
                    .join_with("_");

                ustr(&name)
            };

            let sig = self.lower_fn_sig(&new_fun.sig, name, mono_item.ty, false);

            self.mono_fns.insert(mono_item.clone(), sig);
            self.lower_fn_body(sig, &new_fun);

            sig
        } else {
            panic!("function {} not found in hir.fns", self.db[mono_item.id].qpath);
        }
    }

    fn lower_fn_sig(&mut self, sig: &hir::FnSig, name: Ustr, ty: Ty, is_extern: bool) -> FnSigId {
        self.mir.fn_sigs.push_with_key(|id| FnSig {
            id,
            name,
            params: sig.params.iter().map(|p| FnParam { def_id: p.id, ty: p.ty }).collect(),
            ret: ty.as_fn().unwrap().ret,
            ty,
            is_extern,
        })
    }

    fn lower_fn_body(&mut self, sig: FnSigId, f: &hir::Fn) {
        if f.kind.is_extern() {
            return;
        }

        assert!(
            !self.db[f.id].ty.is_polymorphic(),
            "lowering polymorphic functions to mir is not allowed"
        );

        LowerBodyCx::new(self).lower_fn(sig, f);
    }
}

struct LowerBodyCx<'cx, 'db> {
    cx: &'cx mut LowerCx<'db>,
    body: Body,
    def_to_local: FxHashMap<DefId, ValueId>,
}

impl<'cx, 'db> LowerBodyCx<'cx, 'db> {
    fn new(cx: &'cx mut LowerCx<'db>) -> Self {
        Self { cx, body: Body::new(), def_to_local: FxHashMap::default() }
    }

    fn lower_fn(mut self, sig: FnSigId, f: &hir::Fn) {
        match &f.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(f.id) {
                    self.cx.mir.main_fn = Some(sig);
                }

                self.body.push_block("start");

                for param in self.cx.mir.fn_sigs[sig].params.clone() {
                    let value = self.push_inst_with(param.ty, |value| Inst::Load {
                        value,
                        kind: LoadKind::Local(param.def_id),
                    });
                    self.def_to_local.insert(param.def_id, value);
                }

                let last_value = self.lower_expr(body);

                if !self.body.is_terminating() {
                    let fn_ty = self.cx.mir.fn_sigs[sig].ty.as_fn().unwrap();

                    let ret_value =
                        if fn_ty.ret.is_unit() && !self.body.value(last_value).ty.is_unit() {
                            self.push_unit_lit()
                        } else {
                            last_value
                        };

                    self.push_inst(Inst::Return { value: ret_value });
                }

                self.cx.mir.fns.push(Fn { def_id: f.id, sig, body: self.body });
            }
            FnKind::Extern => unreachable!(),
        }
    }

    fn lower_global(self, let_: &hir::Let) -> Option<GlobalId> {
        let value = self
            .cx
            .db
            .const_storage
            .expr(let_.value.id)
            .cloned()
            .expect("global value must be a constant. static globals are not supported yet");

        match &let_.pat {
            hir::Pat::Name(name) => {
                let full_name = self.cx.db[name.id].qpath.join_with("_");
                let ty = self.cx.db[name.id].ty;

                let id = self.cx.mir.globals.push_with_key(|id| Global {
                    id,
                    def_id: name.id,
                    name: full_name.into(),
                    ty,
                    kind: GlobalKind::Const(value),
                });

                self.cx.globals_map.insert(name.id, id);

                Some(id)
            }
            hir::Pat::Discard(_) => None,
        }
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ValueId {
        let value = if let Some(val) = self.cx.db.const_storage.expr(expr.id).cloned() {
            self.lower_const(&val, expr.ty)
        } else {
            match &expr.kind {
                hir::ExprKind::Let(let_) => {
                    let init = self.lower_expr(&let_.value);

                    match &let_.pat {
                        hir::Pat::Name(name) => {
                            let value = self.push_inst_with(let_.ty, |value| Inst::StackAlloc {
                                value,
                                id: name.id,
                                init,
                            });
                            self.def_to_local.insert(name.id, value);
                        }
                        hir::Pat::Discard(_) => (),
                    }

                    self.push_unit_lit()
                }
                hir::ExprKind::If(if_) => {
                    todo!()
                    // ExprKind::If {
                    //                 cond: self.lower_expr(&if_.cond),
                    //                 then: self.lower_expr(&if_.then),
                    //                 otherwise: self.lower_expr(&if_.otherwise),
                    //             }
                }
                hir::ExprKind::Block(blk) => {
                    let mut result: Option<ValueId> = None;

                    for expr in &blk.exprs {
                        result = Some(self.lower_expr(expr));
                    }

                    // TODO: is this necessary now?
                    // // NOTE: If the block ty is (), we must always return a () value.
                    // // A situation where we don't return a () value can occur
                    // // when the expected type of the block is unit, but the last expression doesn't
                    // // return ().
                    // if expr.ty.is_unit() {
                    //     exprs.push(self.create_expr(ExprKind::Const(Const::Unit), expr.ty));
                    // }

                    result.unwrap_or_else(|| self.push_unit_lit())
                }
                hir::ExprKind::Return(ret) => {
                    todo!()
                    // ExprKind::Return { value: self.lower_expr(&ret.expr) }
                }
                hir::ExprKind::Call(call) => {
                    let callee = self.lower_expr(&call.callee);

                    // NOTE: We evaluate args in passing order, and then sort them to the actual
                    // required parameter order
                    let mut args: Vec<_> = call
                        .args
                        .iter()
                        .map(|arg| {
                            (
                                arg.index.expect("arg index to be resolved"),
                                self.lower_expr(&arg.expr),
                            )
                        })
                        .collect();

                    args.sort_by_key(|(idx, _)| *idx);

                    self.push_inst_with(expr.ty, |value| Inst::Call {
                        value,
                        callee,
                        args: args.into_iter().map(|(_, arg)| arg).collect(),
                    })
                }
                hir::ExprKind::Unary(un) => {
                    let inner = self.lower_expr(&un.expr);
                    self.push_inst_with(expr.ty, |value| Inst::Unary { value, inner, op: un.op })
                }
                hir::ExprKind::Binary(bin) => {
                    let lhs = self.lower_expr(&bin.lhs);
                    let rhs = self.lower_expr(&bin.rhs);
                    self.push_inst_with(expr.ty, |value| Inst::Binary {
                        value,
                        lhs,
                        rhs,
                        op: bin.op,
                    })
                }
                hir::ExprKind::Cast(cast) => {
                    let inner = self.lower_expr(&cast.expr);

                    self.push_inst_with(cast.target, |value| Inst::Cast {
                        value,
                        inner,
                        target: cast.target,
                    })
                }
                hir::ExprKind::Member(access) => {
                    let inner = self.lower_expr(&access.expr);

                    self.push_inst_with(expr.ty, |value| Inst::Member {
                        value,
                        inner,
                        member: access.member.name(),
                    })
                }
                hir::ExprKind::Name(name) => {
                    match self.cx.db[name.id].kind.as_ref() {
                        DefKind::Fn(_) => {
                            let id = if name.instantiation.is_empty() {
                                self.cx.fn_map[&name.id]
                            } else {
                                let mut folder = ParamFolder {
                                    db: self.cx.db,
                                    instantiation: &name.instantiation,
                                };

                                let ty = folder.fold(expr.ty);

                                // let instantiation: Instantiation = name
                                //     .instantiation
                                //     .iter()
                                //     .map(|(var, ty)| (*var, folder.fold(*ty)))
                                //     .collect();

                                self.cx.monomorphize_fn(
                                    &MonoItem { id: name.id, ty },
                                    &name.instantiation,
                                )
                            };

                            self.push_inst_with(self.cx.mir.fn_sigs[id].ty, |value| Inst::Load {
                                value,
                                kind: LoadKind::Fn(id),
                            })
                        }
                        DefKind::ExternGlobal | DefKind::Global => {
                            let id = self.cx.lower_global(name.id);

                            self.push_inst_with(self.cx.mir.globals[id].ty, |value| Inst::Load {
                                value,
                                kind: LoadKind::Global(id),
                            })
                        }
                        DefKind::Variable => self.def_to_local[&name.id],
                        DefKind::Ty(_) => unreachable!(),
                    }
                }
                hir::ExprKind::Lit(lit) => match lit {
                    hir::Lit::Str(lit) => {
                        self.push_inst_with(expr.ty, |value| Inst::StrLit { value, lit: *lit })
                    }
                    #[allow(clippy::cast_possible_wrap)]
                    hir::Lit::Int(lit) => self
                        .push_inst_with(expr.ty, |value| Inst::IntLit { value, lit: *lit as i128 }),
                    hir::Lit::Bool(lit) => {
                        self.push_inst_with(expr.ty, |value| Inst::BoolLit { value, lit: *lit })
                    }
                },
            }
        };

        if let Some(coercions) = self.cx.db.coercions.get(&expr.id) {
            self.apply_coercions(&coercions.clone(), value)
        } else {
            value
        }
    }

    fn lower_const(&mut self, value: &Const, ty: Ty) -> ValueId {
        match value {
            Const::Str(lit) => self.push_inst_with(ty, |value| Inst::StrLit { value, lit: *lit }),
            Const::Int(lit) => self.push_inst_with(ty, |value| Inst::IntLit { value, lit: *lit }),
            Const::Bool(lit) => self.push_bool_lit(*lit),
            Const::Unit => self.push_unit_lit(),
        }
    }

    pub fn push_bool_lit(&mut self, lit: bool) -> ValueId {
        self.push_inst_with(Ty::new(TyKind::Bool), |value| Inst::BoolLit { value, lit })
    }

    pub fn push_unit_lit(&mut self) -> ValueId {
        self.push_inst_with(Ty::new(TyKind::Unit), |value| Inst::UnitLit { value })
    }

    pub fn push_inst_with(&mut self, value_ty: Ty, f: impl FnOnce(ValueId) -> Inst) -> ValueId {
        let value = self.body.push_value(value_ty);
        self.push_inst(f(value));
        value
    }

    pub fn push_inst(&mut self, inst: Inst) {
        self.body.last_block_mut().push_inst(inst);
    }

    pub fn apply_coercions(
        &mut self,
        coercions: &Coercions,
        mut coerced_value: ValueId,
    ) -> ValueId {
        for coercion in coercions.iter() {
            coerced_value = match coercion.kind {
                CoercionKind::NeverToAny => coerced_value,
                CoercionKind::IntPromotion => self.push_inst_with(coercion.target, |value| {
                    Inst::Cast { value, inner: coerced_value, target: coercion.target }
                }),
            };

            self.body.value_mut(coerced_value).ty = coercion.target;
        }

        coerced_value
    }
}
