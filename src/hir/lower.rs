use crate::{
    ast,
    db::Db,
    hir::{
        BinOp, Block, Call, CallArg, Cast, Expr, ExprId, ExprKind, Fn, FnParam, FnSig, Hir, If,
        Item, ItemKind, Lit, LitKind, Name, Return, Ty, TyName, TyParam, UnaryOp,
    },
    span::{Span, Spanned},
    ty::Instantiation,
};

pub fn lower(db: &mut Db, ast: ast::Ast) -> Hir {
    let mut cx = LowerCtxt { db, hir: Hir::new(), id: 0 };

    for module in ast.modules {
        cx.lower_module(module);
    }

    cx.hir
}

struct LowerCtxt<'db> {
    db: &'db mut Db,
    hir: Hir,
    id: usize,
}

impl<'db> LowerCtxt<'db> {
    fn lower_module(&mut self, module: ast::Module) {
        let items: Vec<_> = module.items.into_iter().map(|item| item.lower(self)).collect();
        self.hir.items.extend(items);
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

impl Lower<'_, Item> for ast::Item {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Item {
        Item {
            kind: match self {
                Self::Fn(fun) => ItemKind::Fn(fun.lower(cx)),
            },
            ty: cx.db.types.unknown,
        }
    }
}

impl Lower<'_, Fn> for ast::Fn {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Fn {
        let body_span = self.body.span;
        let body = ExprKind::Block(self.body.lower(cx));

        Fn {
            id: self.id.expect("to be resolved"),
            sig: self.sig.lower(cx),
            body: cx.expr(body, body_span),
            span: self.span,
            ty: cx.db.types.unknown,
        }
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
            Self::Item(item) => {
                let span = item.span();
                let item = item.lower(cx);
                cx.hir.items.push(item);
                // TODO: We need to create a dummy value because we must return an expression.
                // Maybe we can avoid this since we know that this must be a block statement?
                cx.expr(ExprKind::Lit(Lit { kind: LitKind::Unit }), span)
            }
            Self::Return(ret) => {
                let span = ret.span;
                let expr = if let Some(expr) = ret.expr {
                    Box::new(expr.lower(cx))
                } else {
                    Box::new(cx.expr(ExprKind::Lit(Lit { kind: LitKind::Unit }), span))
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
            Self::UnaryOp(ast::UnaryOp { expr, op, span }) => {
                let kind = ExprKind::UnaryOp(UnaryOp { expr: Box::new(expr.lower(cx)), op });
                cx.expr(kind, span)
            }
            Self::BinOp(ast::BinOp { lhs, rhs, op, span }) => {
                let kind = ExprKind::BinOp(BinOp {
                    lhs: Box::new(lhs.lower(cx)),
                    rhs: Box::new(rhs.lower(cx)),
                    op,
                });
                cx.expr(kind, span)
            }
            Self::Cast(ast::Cast { expr, ty, span }) => {
                let kind =
                    ExprKind::Cast(Cast { expr: Box::new(expr.lower(cx)), ty: ty.lower(cx) });
                cx.expr(kind, span)
            }
            Self::Name(ast::Name { id, name: _, args, span }) => {
                let kind = ExprKind::Name(Name {
                    id: id.expect("to be resolved"),
                    args: args.map(|args| args.into_iter().map(|arg| arg.lower(cx)).collect()),
                    instantiation: Instantiation::new(),
                });
                cx.expr(kind, span)
            }
            Self::Lit(ast::Lit { kind, span }) => cx.expr(
                ExprKind::Lit(Lit {
                    kind: match kind {
                        ast::LitKind::Int(v) => LitKind::Int(v),
                        ast::LitKind::Bool(v) => LitKind::Bool(v),
                        ast::LitKind::Unit => LitKind::Unit,
                    },
                }),
                span,
            ),
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
            ast::Ty::Name(name) => Ty::Name(TyName {
                id: name.id.expect("to be resolved"),
                args: name.args.into_iter().map(|a| a.lower(cx)).collect(),
                span: name.span,
            }),
            ast::Ty::Unit(span) => Ty::Unit(span),
            ast::Ty::Never(span) => Ty::Never(span),
            ast::Ty::Infer(span) => Ty::Infer(span),
        }
    }
}
