mod coerce;
mod env;
mod error;
mod instantiate;
mod normalize;
mod passes;
mod subst;
mod unify;

use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;
use rustc_hash::FxHashSet;
use ustr::{Ustr, UstrMap};

use crate::{
    ast::{self, Ast},
    common::{Counter, Word},
    db::{Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    diagnostics::Diagnostic,
    hir,
    hir::{ExprId, Hir},
    macros::create_bool_enum,
    name_resolution::{
        env::{BuiltinTys, Env, GlobalScope, ScopeKind},
        error::ResolveError,
        normalize::NormalizeTy,
    },
    span::{Span, Spanned},
    ty::{InferTy, Instantiation, IntVar, Ty, TyKind, TyVar},
};

pub type ResolveResult<T> = Result<T, ResolveError>;

pub fn resolve(db: &mut Db, ast: &Ast) -> Result<Hir, Diagnostic> {
    resolve_inner(db, ast).map_err(|err| err.into_diagnostic(db))
}

fn resolve_inner(db: &mut Db, ast: &Ast) -> ResolveResult<Hir> {
    Resolver::new(db, ast).run()
}

pub struct Resolver<'db> {
    db: &'db mut Db,
    ast: &'db Ast,
    hir: Hir,
    global_scope: GlobalScope,
    builtin_tys: BuiltinTys,
    completed_items: FxHashSet<ast::ItemId>,
    storage: RefCell<TyStorage>,
    expr_id: Counter<ExprId>,
}

#[derive(Debug)]
pub struct TyStorage {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl TyStorage {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }
}

impl<'db> Resolver<'db> {
    fn new(db: &'db mut Db, ast: &'db Ast) -> Self {
        Self {
            builtin_tys: BuiltinTys::new(db),
            global_scope: GlobalScope::new(ast),
            db,
            ast,
            hir: Hir::new(),
            completed_items: FxHashSet::default(),
            storage: RefCell::new(TyStorage::new()),
            expr_id: Counter::new(),
        }
    }

    fn run(mut self) -> ResolveResult<Hir> {
        for module in &self.ast.modules {
            let mut env = Env::new(module.id.expect("ModuleId to be resolved"));

            for (item_id, item) in module.items.iter().enumerate() {
                if !self.completed_items.contains(&item_id.into()) {
                    match self.resolve_item(&mut env, item)? {
                        ItemResult::Let(let_) => self.hir.lets.push(let_),
                        ItemResult::Unit(_) => (),
                    }
                }
            }
        }

        Ok(self.hir)
    }

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::TyVar(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.storage.borrow_mut().ty_unification_table.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::IntVar(
            self.storage.borrow_mut().int_unification_table.new_key(None),
        )))
    }

    #[inline]
    pub fn normalize(&self, ty: Ty) -> Ty {
        ty.normalize(&mut self.storage.borrow_mut())
    }

    fn find_and_resolve_global_item(
        &mut self,
        module_id: ModuleId,
        name: Ustr,
    ) -> ResolveResult<Option<DefId>> {
        if let Some(item_id) = self.global_scope.get_item(module_id, name) {
            let item = &self.ast.modules[module_id].items[item_id];

            let mut env = Env::new(module_id);

            let id = match self.resolve_item(&mut env, item)? {
                ItemResult::Let(let_) => {
                    let id = match &let_.pat {
                        hir::Pat::Name(name) => name.id,
                        hir::Pat::Discard(_) => unreachable!(),
                    };

                    self.hir.lets.push(let_);

                    id
                }
                ItemResult::Unit(id) => id,
            };

            self.completed_items.insert(item_id);

            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    fn define_global_def(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
    ) -> ResolveResult<DefId> {
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let qpath = self.db[module_id].name.clone().child(name.name());

        let id = DefInfo::alloc(self.db, qpath, scope, kind, self.db.types.unknown, name.span());

        if let Some(prev_id) = self.global_scope.insert_def(module_id, name.name(), id) {
            let def = &self.db[prev_id];
            return Err(ResolveError::MultipleItems {
                name: def.qpath.name(),
                prev_span: def.span,
                dup_span: name.span(),
            });
        }

        Ok(id)
    }

    fn define_local_def(&mut self, env: &mut Env, kind: DefKind, name: Word) -> DefId {
        let id = DefInfo::alloc(
            self.db,
            env.scope_path(self.db).child(name.name()),
            ScopeInfo { module_id: env.module_id(), level: env.scope_level(), vis: Vis::Private },
            kind,
            self.db.types.unknown,
            name.span(),
        );

        env.current_mut().insert(name.name(), id);

        id
    }

    fn define_def(
        &mut self,
        env: &mut Env,
        vis: Vis,
        kind: DefKind,
        name: Word,
    ) -> ResolveResult<DefId> {
        if env.in_global_scope() {
            self.define_global_def(env.module_id(), vis, kind, name)
        } else {
            Ok(self.define_local_def(env, kind, name))
        }
    }

    fn define_pat(
        &mut self,
        env: &mut Env,
        vis: Vis,
        kind: DefKind,
        pat: &ast::Pat,
    ) -> ResolveResult<hir::Pat> {
        match pat {
            ast::Pat::Name(name) => Ok(hir::Pat::Name(hir::NamePat {
                id: self.define_def(env, vis, kind, name.word)?,
                word: name.word,
            })),
            ast::Pat::Discard(span) => Ok(hir::Pat::Discard(*span)),
        }
    }

    fn lookup_def(&mut self, env: &Env, word: Word) -> ResolveResult<DefId> {
        if let Some(id) = env.lookup(word.name()).copied() {
            Ok(id)
        } else if let Some(id) = self.lookup_global_def(env.module_id(), word.name())? {
            Ok(id)
        } else {
            Err(ResolveError::NameNotFound(word))
        }
    }

    fn lookup_global_def(
        &mut self,
        module_id: ModuleId,
        name: Ustr,
    ) -> ResolveResult<Option<DefId>> {
        let id = if let Some(id) = self.global_scope.get_def(module_id, name) {
            Some(id)
        } else if let Some(id) = self.find_and_resolve_global_item(module_id, name)? {
            Some(id)
        } else {
            self.builtin_tys.get(name)
        };

        Ok(id)
    }

    fn resolve_item(&mut self, env: &mut Env, item: &ast::Item) -> ResolveResult<ItemResult> {
        match item {
            ast::Item::Fn(fun) => {
                let f = self.resolve_fn(env, fun)?;
                let id = f.id;
                self.hir.fns.push(f);
                Ok(ItemResult::Unit(id))
            }
            ast::Item::Let(let_) => {
                let let_ = self.resolve_let(env, let_)?;
                Ok(ItemResult::Let(let_))
            }
            ast::Item::ExternLet(let_) => {
                let let_ = self.resolve_extern_let(env, let_)?;
                let id = let_.id;
                self.hir.extern_lets.push(let_);
                Ok(ItemResult::Unit(id))
            }
        }
    }

    fn resolve_fn(&mut self, env: &mut Env, fun: &ast::Fn) -> ResolveResult<hir::Fn> {
        let id = self.define_def(env, Vis::Private, DefKind::Fn(FnInfo::Bare), fun.sig.word)?;

        for attr in &fun.attrs {
            if let Some(value) = &attr.value {
                self.resolve_expr(env, value)?;
            }
        }

        let attrs = self.resolve_attrs(env, &fun.attrs)?;

        let (sig, kind) = env.with_scope(fun.sig.word.name(), ScopeKind::Fn, |env| {
            let sig = self.resolve_sig(env, &fun.sig)?;

            let kind = match &fun.kind {
                ast::FnKind::Bare { body } => {
                    hir::FnKind::Bare { body: self.resolve_expr(env, body)? }
                }
                ast::FnKind::Extern => hir::FnKind::Extern,
            };

            Ok((sig, kind))
        })?;

        Ok(hir::Fn { module_id: env.module_id(), id, attrs, sig, kind, span: fun.span })
    }

    fn resolve_sig(&mut self, env: &mut Env, sig: &ast::FnSig) -> ResolveResult<hir::FnSig> {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        let ty_params = self.resolve_ty_params(env, &sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = UstrMap::<Span>::default();

        for p in &sig.params {
            let id = self.define_local_def(env, DefKind::Variable, p.name);

            let ty_annot = self.resolve_ty_expr(env, &p.ty_annot, AllowTyHole::No)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                return Err(ResolveError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }

            params.push(hir::FnParam { id, ty_annot, span: p.span, ty: self.db.types.unknown });
        }

        let ret = sig
            .ret
            .as_ref()
            .map(|ret| self.resolve_ty_expr(env, ret, AllowTyHole::No))
            .transpose()?;

        Ok(hir::FnSig { ty_params, params, ret })
    }

    fn resolve_let(&mut self, env: &mut Env, let_: &ast::Let) -> ResolveResult<hir::Let> {
        let pat = self.define_pat(env, Vis::Private, DefKind::Variable, &let_.pat)?;

        env.with_anon_scope(ScopeKind::Initializer, |env| {
            if let Some(ty) = &let_.ty_annot {
                self.resolve_ty_expr(env, ty, AllowTyHole::Yes)?;
            }

            self.resolve_expr(env, &let_.value)
        })?;

        let attrs = self.resolve_attrs(env, &let_.attrs)?;

        let ty_annot = let_
            .ty_annot
            .as_ref()
            .map(|t| self.resolve_ty_expr(env, t, AllowTyHole::Yes))
            .transpose()?;

        let value = self.resolve_expr(env, &let_.value)?;

        Ok(hir::Let {
            module_id: env.module_id(),
            attrs,
            pat,
            ty_annot,
            value: Box::new(value),
            span: let_.span,
        })
    }

    fn resolve_extern_let(
        &mut self,
        env: &mut Env,
        let_: &ast::ExternLet,
    ) -> ResolveResult<hir::ExternLet> {
        let id = self.define_def(env, Vis::Private, DefKind::ExternGlobal, let_.word)?;

        let attrs = self.resolve_attrs(env, &let_.attrs)?;
        let ty_annot = self.resolve_ty_expr(env, &let_.ty_annot, AllowTyHole::No)?;

        Ok(hir::ExternLet {
            module_id: env.module_id(),
            id,
            attrs,
            word: let_.word,
            ty_annot,
            span: let_.span,
        })
    }

    fn resolve_attrs(&mut self, env: &mut Env, attrs: &ast::Attrs) -> ResolveResult<hir::Attrs> {
        attrs
            .iter()
            .map(|attr| {
                Ok(hir::Attr {
                    kind: attr.kind,
                    value: attr.value.as_ref().map(|v| self.resolve_expr(env, v)).transpose()?,
                    span: attr.span,
                })
            })
            .try_collect()
    }

    fn resolve_expr(&mut self, env: &mut Env, expr: &ast::Expr) -> ResolveResult<hir::Expr> {
        match expr {
            ast::Expr::Item(item) => match self.resolve_item(env, item)? {
                ItemResult::Let(let_) => Ok(self.expr(hir::ExprKind::Let(let_), item.span())),
                ItemResult::Unit(_) => Ok(self.unit(item.span())),
            },
            ast::Expr::Return { expr, span } => {
                let expr = if let Some(expr) = expr {
                    self.resolve_expr(env, expr)?
                } else {
                    self.unit(*span)
                };

                Ok(self.expr(hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }), *span))
            }
            ast::Expr::If { cond, then, otherwise, span } => {
                let cond = self.resolve_expr(env, cond)?;
                let then = self.resolve_expr(env, then)?;

                let otherwise = if let Some(otherwise) = otherwise {
                    Some(Box::new(self.resolve_expr(env, otherwise)?))
                } else {
                    None
                };

                Ok(self.expr(
                    hir::ExprKind::If(hir::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        otherwise,
                    }),
                    *span,
                ))
            }
            ast::Expr::Block { exprs, span } => env.with_anon_scope(ScopeKind::Block, |env| {
                let exprs = exprs.iter().map(|e| self.resolve_expr(env, e)).try_collect()?;
                Ok(self.expr(hir::ExprKind::Block(hir::Block { exprs }), *span))
            }),
            ast::Expr::Call { callee, args, span } => {
                let callee = self.resolve_expr(env, callee)?;

                let mut new_args = vec![];

                for arg in args {
                    new_args.push(match arg {
                        ast::CallArg::Named(name, expr) => hir::CallArg {
                            name: Some(*name),
                            expr: self.resolve_expr(env, expr)?,
                            index: None,
                        },
                        ast::CallArg::Positional(expr) => hir::CallArg {
                            name: None,
                            expr: self.resolve_expr(env, expr)?,
                            index: None,
                        },
                    });
                }

                Ok(self.expr(
                    hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args: new_args }),
                    *span,
                ))
            }
            ast::Expr::Unary { expr, op, span } => {
                let expr = self.resolve_expr(env, expr)?;

                Ok(self.expr(
                    hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                    *span,
                ))
            }
            ast::Expr::Member { expr, member, span } => {
                let expr = self.resolve_expr(env, expr)?;

                Ok(self.expr(
                    hir::ExprKind::Member(hir::Member { expr: Box::new(expr), member: *member }),
                    *span,
                ))
            }
            ast::Expr::Binary { lhs, rhs, op, span } => {
                let lhs = self.resolve_expr(env, lhs)?;
                let rhs = self.resolve_expr(env, rhs)?;

                Ok(self.expr(
                    hir::ExprKind::Binary(hir::Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    *span,
                ))
            }
            ast::Expr::Cast { expr, ty, span } => {
                let expr = self.resolve_expr(env, expr)?;
                let target = self.resolve_ty_expr(env, ty, AllowTyHole::Yes)?;

                Ok(self
                    .expr(hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }), *span))
            }
            ast::Expr::Name { word, args, span } => {
                let id = self.lookup_def(env, *word)?;

                let args = if let Some(args) = args {
                    let mut new_args = vec![];

                    for arg in args {
                        new_args.push(self.resolve_ty_expr(env, arg, AllowTyHole::Yes)?);
                    }

                    Some(new_args)
                } else {
                    None
                };

                Ok(self.expr(
                    hir::ExprKind::Name(hir::Name {
                        id,
                        args,
                        instantiation: Instantiation::default(),
                    }),
                    *span,
                ))
            }
            ast::Expr::Group { expr, span } => {
                let mut expr = self.resolve_expr(env, expr)?;
                expr.span = *span;
                Ok(expr)
            }
            ast::Expr::Lit { kind, span } => Ok(self.expr(
                hir::ExprKind::Lit(match kind {
                    ast::LitKind::Str(v) => hir::Lit::Str(*v),
                    ast::LitKind::Int(v) => hir::Lit::Int(*v),
                    ast::LitKind::Bool(v) => hir::Lit::Bool(*v),
                    ast::LitKind::Unit => hir::Lit::Unit,
                }),
                *span,
            )),
        }
    }

    fn resolve_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &[ast::TyParam],
    ) -> ResolveResult<Vec<hir::TyParam>> {
        let mut new_ty_params = vec![];
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            let id = self.define_local_def(env, DefKind::Ty(ty), tp.name);

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                return Err(ResolveError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }

            new_ty_params.push(hir::TyParam { id, span: tp.name.span() });
        }

        Ok(new_ty_params)
    }

    fn resolve_ty_expr(
        &mut self,
        env: &Env,
        ty: &ast::TyExpr,
        allow_hole: AllowTyHole,
    ) -> ResolveResult<hir::TyExpr> {
        match ty {
            ast::TyExpr::RawPtr(pointee, span) => {
                let pointee = self.resolve_ty_expr(env, pointee, allow_hole)?;
                Ok(hir::TyExpr::RawPtr(Box::new(pointee), *span))
            }
            ast::TyExpr::Name(name) => {
                let id = self.lookup_def(env, name.word)?;

                let args = name
                    .args
                    .iter()
                    .map(|a| self.resolve_ty_expr(env, a, AllowTyHole::Yes))
                    .try_collect()?;

                Ok(hir::TyExpr::Name(hir::TyName { id, args, span: name.span }))
            }
            ast::TyExpr::Hole(span) => {
                if allow_hole.into() {
                    Ok(hir::TyExpr::Hole(*span))
                } else {
                    Err(ResolveError::InvalidInferTy(*span))
                }
            }
            ast::TyExpr::Unit(span) => Ok(hir::TyExpr::Unit(*span)),
        }
    }

    fn expr(&mut self, kind: hir::ExprKind, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.next(), kind, span, ty: self.db.types.unknown }
    }

    fn unit(&mut self, span: Span) -> hir::Expr {
        self.expr(hir::ExprKind::Lit(hir::Lit::Unit), span)
    }
}

enum ItemResult {
    Let(hir::Let),
    Unit(DefId),
}

create_bool_enum!(AllowTyHole);
