use crate::{
    ast,
    db::Db,
    tast::{
        Binary, Block, Call, CallArg, Expr, Function, FunctionParam, FunctionSig, If, Item,
        ItemKind, Lit, LitKind, Name, Return, TypedAst,
    },
    ty::{Type, TypeKind},
};

pub fn lower(db: &mut Db, ast: ast::Ast) -> TypedAst {
    TypedAst {
        items: ast
            .modules
            .into_iter()
            .flat_map(|module| LowerCtxt { _db: db }.run(module))
            .collect(),
    }
}

struct LowerCtxt<'db> {
    _db: &'db mut Db,
}

impl<'db> LowerCtxt<'db> {
    fn run(&mut self, module: ast::Module) -> Vec<Item> {
        module.items.into_iter().map(|item| item.lower(self)).collect()
    }
}

trait Lower<'db, T> {
    fn lower(self, cx: &mut LowerCtxt<'db>) -> T;
}

impl Lower<'_, Item> for ast::Item {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Item {
        Item {
            kind: match self {
                Self::Function(fun) => ItemKind::Function(fun.lower(cx)),
            },
            ty: Type::new(TypeKind::Unknown),
        }
    }
}

impl Lower<'_, Function> for ast::Function {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Function {
        Function {
            id: self.id.expect("to be resolved"),
            sig: self.sig.lower(cx),
            body: self.body.lower(cx),
            span: self.span,
            ty: Type::new(TypeKind::Unknown),
        }
    }
}

impl Lower<'_, FunctionSig> for ast::FunctionSig {
    fn lower(self, _cx: &mut LowerCtxt<'_>) -> FunctionSig {
        FunctionSig {
            params: self
                .params
                .into_iter()
                .map(|p| FunctionParam {
                    id: p.id.expect("to be resolved"),
                    span: p.span,
                    ty: Type::new(TypeKind::Unknown),
                })
                .collect::<Vec<_>>(),
        }
    }
}

impl Lower<'_, Expr> for ast::Expr {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Expr {
        match self {
            Self::Item(item) => Expr::Item(item.lower(cx)),
            Self::Return(ret) => Expr::Return(Return {
                expr: ret.expr.map_or_else(
                    || {
                        Box::new(Expr::Lit(Lit {
                            kind: LitKind::Unit,
                            span: ret.span,
                            ty: Type::new(TypeKind::Unknown),
                        }))
                    },
                    |v| Box::new(v.lower(cx)),
                ),
                span: ret.span,
                ty: Type::new(TypeKind::Unknown),
            }),
            Self::If(if_) => Expr::If(If {
                cond: Box::new(if_.cond.lower(cx)),
                then: Box::new(if_.then.lower(cx)),
                otherwise: if_.otherwise.map(|o| Box::new(o.lower(cx))),
                span: if_.span,
                ty: Type::new(TypeKind::Unknown),
            }),
            Self::Block(blk) => Expr::Block(blk.lower(cx)),
            Self::Call(call) => Expr::Call(Call {
                callee: Box::new(call.callee.lower(cx)),
                args: call.args.into_iter().map(|arg| arg.lower(cx)).collect(),
                span: call.span,
                ty: Type::new(TypeKind::Unknown),
            }),
            Self::Binary(bin) => Expr::Binary(Binary {
                lhs: Box::new(bin.lhs.lower(cx)),
                rhs: Box::new(bin.rhs.lower(cx)),
                op: bin.op,
                span: bin.span,
                ty: Type::new(TypeKind::Unknown),
            }),
            Self::Name(name) => Expr::Name(Name {
                id: name.id.expect("to be resolved"),
                span: name.span,
                ty: Type::new(TypeKind::Unknown),
            }),
            Self::Lit(lit) => Expr::Lit(Lit {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => LitKind::Int(v),
                    ast::LitKind::Bool(v) => LitKind::Bool(v),
                    ast::LitKind::Unit => LitKind::Unit,
                },
                span: lit.span,
                ty: Type::new(TypeKind::Unknown),
            }),
        }
    }
}

impl Lower<'_, CallArg> for ast::CallArg {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> CallArg {
        match self {
            Self::Positional(expr) => CallArg::Positional(expr.lower(cx)),
            Self::Named(name, expr) => CallArg::Named(name, expr.lower(cx)),
        }
    }
}
impl Lower<'_, Block> for ast::Block {
    fn lower(self, cx: &mut LowerCtxt<'_>) -> Block {
        Block {
            exprs: self.exprs.into_iter().map(|e| e.lower(cx)).collect(),
            span: self.span,
            ty: Type::new(TypeKind::Unknown),
        }
    }
}
