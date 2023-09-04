use crate::{
    ast,
    db::Db,
    hir::{
        BinOp, Block, Call, CallArg, Expr, ExprKind, Fn, FnParam, FnSig, Hir, If, Item, ItemKind,
        Lit, LitKind, Name, Return, Ty, TyName, TyParam,
    },
    span::{Span, Spanned},
    ty::{tcx::TyCtxt, Instantiation},
};

pub fn lower(db: &mut Db, tcx: &TyCtxt, ast: ast::Ast) -> Hir {
    let mut cx = LowerCtxt { _db: db, tcx, hir: Hir::new() };

    for module in ast.modules {
        cx.lower_module(module);
    }

    cx.hir
}

struct LowerCtxt<'db> {
    _db: &'db mut Db,
    tcx: &'db TyCtxt,
    hir: Hir,
}

impl<'db> LowerCtxt<'db> {
    fn lower_module(&mut self, module: ast::Module) {
        let items: Vec<_> = module.items.into_iter().map(|item| item.lower(self)).collect();
        self.hir.items.extend(items);
    }

    fn expr(&mut self, span: Span, kind: ExprKind) -> Expr {
        Expr { kind, span, ty: self.tcx.types.unknown }
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
            ty: cx.tcx.types.unknown,
        }
    }
}

impl Lower<'_, Fn> for ast::Fn {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Fn {
        Fn {
            id: self.id.expect("to be resolved"),
            sig: self.sig.lower(cx),
            body: cx.expr(self.body.span, ExprKind::Block(self.body.lower(cx))),
            span: self.span,
            ty: cx.tcx.types.unknown,
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
                    ty: cx.tcx.types.unknown,
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
                cx.expr(item.span(), ExprKind::Lit(Lit { kind: LitKind::Unit }))
            }
            Self::Return(ret) => cx.expr(
                ret.span,
                ExprKind::Return(Return {
                    expr: ret.expr.map_or_else(
                        || Box::new(cx.expr(ret.span, ExprKind::Lit(Lit { kind: LitKind::Unit }))),
                        |v| Box::new(v.lower(cx)),
                    ),
                }),
            ),
            Self::If(if_) => cx.expr(
                if_.span,
                ExprKind::If(If {
                    cond: Box::new(if_.cond.lower(cx)),
                    then: Box::new(if_.then.lower(cx)),
                    otherwise: if_.otherwise.map(|o| Box::new(o.lower(cx))),
                }),
            ),
            Self::Block(blk) => cx.expr(blk.span, ExprKind::Block(blk.lower(cx))),
            Self::Call(call) => cx.expr(
                call.span,
                ExprKind::Call(Call {
                    callee: Box::new(call.callee.lower(cx)),
                    args: call.args.into_iter().map(|arg| arg.lower(cx)).collect(),
                }),
            ),
            Self::Bin(bin) => cx.expr(
                bin.span,
                ExprKind::Bin(BinOp {
                    lhs: Box::new(bin.lhs.lower(cx)),
                    rhs: Box::new(bin.rhs.lower(cx)),
                    op: bin.op,
                }),
            ),
            Self::Name(name) => cx.expr(
                name.span,
                ExprKind::Name(Name {
                    id: name.id.expect("to be resolved"),
                    args: name.args.map(|args| args.into_iter().map(|arg| arg.lower(cx)).collect()),
                    instantiation: Instantiation::new(),
                }),
            ),
            Self::Lit(lit) => cx.expr(
                lit.span,
                ExprKind::Lit(Lit {
                    kind: match lit.kind {
                        ast::LitKind::Int(v) => LitKind::Int(v),
                        ast::LitKind::Bool(v) => LitKind::Bool(v),
                        ast::LitKind::Unit => LitKind::Unit,
                    },
                }),
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
