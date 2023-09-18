use crate::{
    ast,
    db::{Db, ModuleId},
    hir::{
        Attr, Attrs, Binary, Block, Call, CallArg, Cast, Expr, ExprId, ExprKind, ExternLet, Fn,
        FnKind, FnParam, FnSig, Hir, If, Let, Lit, MemberAccess, Name, NamePat, Pat, Return, Ty,
        TyName, TyParam, Unary,
    },
    span::{Span, Spanned},
    ty::Instantiation,
};

pub fn lower(db: &mut Db, ast: ast::Ast) -> Hir {
    let mut hir = Hir::new();

    for module in ast.modules {
        LowerCtxt { db, hir: &mut hir, module_id: module.id.expect("to be resolved"), id: 0 }
            .lower_module(module);
    }

    hir
}

struct LowerCtxt<'db> {
    db: &'db mut Db,
    hir: &'db mut Hir,
    module_id: ModuleId,
    id: usize,
}

impl<'db> LowerCtxt<'db> {
    fn lower_module(&mut self, module: ast::Module) {
        for item in module.items {
            match item {
                ast::Item::Fn(f) => {
                    let f = f.lower(self);
                    self.hir.fns.push(f);
                }
                ast::Item::ExternLet(let_) => {
                    let let_ = let_.lower(self);
                    self.hir.extern_lets.push(let_);
                }
                ast::Item::Let(let_) => {
                    let let_ = let_.lower(self);
                    self.hir.lets.push(let_);
                }
            }
        }
    }

    fn expr(&mut self, kind: ExprKind, span: Span) -> Expr {
        Expr { id: self.next_id(), kind, span, ty: self.db.types.unknown }
    }

    fn next_id(&mut self) -> ExprId {
        self.id += 1;
        self.id.into()
    }
}

trait Lower<'db, T> {
    fn lower(self, cx: &mut LowerCtxt<'db>) -> T;
}

impl Lower<'_, Fn> for ast::Fn {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Fn {
        Fn {
            module_id: cx.module_id,
            id: self.id.expect("to be resolved"),
            attrs: self.attrs.lower(cx),
            sig: self.sig.lower(cx),
            kind: match self.kind {
                ast::FnKind::Bare { body } => FnKind::Bare { body: body.lower(cx) },
                ast::FnKind::Extern => FnKind::Extern,
            },
            span: self.span,
        }
    }
}

impl Lower<'_, Attrs> for ast::Attrs {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Attrs {
        self.into_iter()
            .map(|attr| Attr {
                kind: attr.kind,
                value: attr.value.map(|v| v.lower(cx)),
                span: attr.span,
            })
            .collect()
    }
}

impl Lower<'_, FnSig> for ast::FnSig {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> FnSig {
        FnSig {
            ty_params: self
                .ty_params
                .into_iter()
                .map(|p| TyParam { id: p.id.expect("to be resolved"), span: p.name.span() })
                .collect::<Vec<_>>(),
            params: self
                .params
                .into_iter()
                .map(|p| FnParam {
                    id: p.id.expect("to be resolved"),
                    ty_annot: p.ty.lower(cx),
                    span: p.span,
                    ty: cx.db.types.unknown,
                })
                .collect::<Vec<_>>(),
            ret: self.ret.map(|r| r.lower(cx)),
        }
    }
}

impl Lower<'_, Expr> for ast::Expr {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Expr {
        match self {
            Self::Item(item) => match item {
                ast::Item::Fn(f) => {
                    let span = f.span;
                    let f = f.lower(cx);
                    cx.hir.fns.push(f);
                    cx.expr(ExprKind::Lit(Lit::Unit), span)
                }
                ast::Item::ExternLet(let_) => {
                    let span = let_.span;
                    let let_ = let_.lower(cx);
                    cx.hir.extern_lets.push(let_);
                    cx.expr(ExprKind::Lit(Lit::Unit), span)
                }
                ast::Item::Let(let_) => {
                    let span = let_.span;
                    let let_ = let_.lower(cx);
                    cx.expr(ExprKind::Let(let_), span)
                }
            },
            Self::Return(ret) => {
                let span = ret.span;
                let expr = if let Some(expr) = ret.expr {
                    Box::new(expr.lower(cx))
                } else {
                    Box::new(cx.expr(ExprKind::Lit(Lit::Unit), span))
                };
                cx.expr(ExprKind::Return(Return { expr }), span)
            }
            Self::If(ast::If { cond, then, otherwise, span }) => {
                let kind = ExprKind::If(If {
                    cond: Box::new(cond.lower(cx)),
                    then: Box::new(then.lower(cx)),
                    otherwise: otherwise.map(|o| Box::new(o.lower(cx))),
                });
                cx.expr(kind, span)
            }
            Self::Block(blk) => {
                let span = blk.span;
                let kind = ExprKind::Block(blk.lower(cx));
                cx.expr(kind, span)
            }
            Self::Call(ast::Call { callee, args, span }) => {
                let kind = ExprKind::Call(Call {
                    callee: Box::new(callee.lower(cx)),
                    args: args.into_iter().map(|arg| arg.lower(cx)).collect(),
                });
                cx.expr(kind, span)
            }
            Self::Unary(ast::Unary { expr, op, span }) => {
                let kind = ExprKind::Unary(Unary { expr: Box::new(expr.lower(cx)), op });
                cx.expr(kind, span)
            }
            Self::Binary(ast::Binary { lhs, rhs, op, span }) => {
                let kind = ExprKind::Binary(Binary {
                    lhs: Box::new(lhs.lower(cx)),
                    rhs: Box::new(rhs.lower(cx)),
                    op,
                });
                cx.expr(kind, span)
            }
            Self::Cast(ast::Cast { expr, ty, span }) => {
                let kind =
                    ExprKind::Cast(Cast { expr: Box::new(expr.lower(cx)), target: ty.lower(cx) });
                cx.expr(kind, span)
            }
            Self::MemberAccess(ast::MemberAccess { expr, member, span }) => {
                let kind =
                    ExprKind::MemberAccess(MemberAccess { expr: Box::new(expr.lower(cx)), member });
                cx.expr(kind, span)
            }
            Self::Name(ast::Name { id, word: _, args, span }) => {
                let kind = ExprKind::Name(Name {
                    id: id.expect("to be resolved"),
                    args: args.map(|args| args.into_iter().map(|arg| arg.lower(cx)).collect()),
                    instantiation: Instantiation::new(),
                });
                cx.expr(kind, span)
            }
            Self::Lit(ast::Lit { kind, span }) => cx.expr(
                ExprKind::Lit(match kind {
                    ast::LitKind::Str(v) => Lit::Str(v),
                    ast::LitKind::Int(v) => Lit::Int(v),
                    ast::LitKind::Bool(v) => Lit::Bool(v),
                    ast::LitKind::Unit => Lit::Unit,
                }),
                span,
            ),
        }
    }
}

impl Lower<'_, ExternLet> for ast::ExternLet {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> ExternLet {
        ExternLet {
            module_id: cx.module_id,
            id: self.id.expect("to be resolved"),
            attrs: self.attrs.lower(cx),
            word: self.word,
            ty_annot: self.ty_annot.lower(cx),
            span: self.span,
        }
    }
}

impl Lower<'_, Let> for ast::Let {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Let {
        Let {
            module_id: cx.module_id,
            attrs: self.attrs.lower(cx),
            pat: self.pat.lower(cx),
            ty_annot: self.ty_annot.map(|t| t.lower(cx)),
            value: Box::new(self.value.lower(cx)),
            span: self.span,
        }
    }
}

impl Lower<'_, Pat> for ast::Pat {
    fn lower(self, _cx: &mut LowerCtxt<'_>) -> Pat {
        match self {
            ast::Pat::Name(name) => {
                Pat::Name(NamePat { id: name.id.expect("to be resolved"), word: name.word })
            }
            ast::Pat::Ignore(span) => Pat::Ignore(span),
        }
    }
}

impl Lower<'_, CallArg> for ast::CallArg {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> CallArg {
        match self {
            Self::Positional(expr) => CallArg { name: None, expr: expr.lower(cx), index: None },
            Self::Named(name, expr) => {
                CallArg { name: Some(name), expr: expr.lower(cx), index: None }
            }
        }
    }
}

impl Lower<'_, Block> for ast::Block {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Block {
        Block { exprs: self.exprs.into_iter().map(|e| e.lower(cx)).collect() }
    }
}

#[allow(clippy::complexity)]
impl Lower<'_, Ty> for ast::Ty {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Ty {
        match self {
            ast::Ty::RawPtr(pointee, span) => Ty::RawPtr(Box::new(pointee.lower(cx)), span),
            ast::Ty::Name(name) => Ty::Name(TyName {
                id: name.id.expect("to be resolved"),
                args: name.args.into_iter().map(|a| a.lower(cx)).collect(),
                span: name.span,
            }),
            ast::Ty::Unit(span) => Ty::Unit(span),
            ast::Ty::Hole(span) => Ty::Hole(span),
        }
    }
}
