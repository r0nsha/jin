use super::{
    Block, Call, Expr, Function, FunctionParam, Hir, Item, ItemKind, Lit, LitKind, Module,
    ModuleId, Name, Return, TypeId,
};
use crate::{
    ast,
    db::{self, Database},
    hir::{Binary, CallArg, If},
};

pub fn lower(db: &mut Database, lib: ast::Library) -> Hir {
    Hir {
        modules: lib
            .modules
            .into_iter()
            .map(|module| {
                let id =
                    db::ModuleInfo::alloc(db, module.source, module.name.clone(), module.is_main());
                Cx { db, id }.run(module)
            })
            .collect(),
    }
}

struct Cx<'db> {
    #[allow(unused)]
    db: &'db mut Database,
    id: ModuleId,
}

impl<'db> Cx<'db> {
    fn run(&mut self, module: ast::Module) -> Module {
        Module { id: self.id, items: module.items.into_iter().map(|tl| tl.lower(self)).collect() }
    }
}

trait Lower<'db, T> {
    fn lower(self, cx: &mut Cx<'db>) -> T;
}

impl Lower<'_, Item> for ast::Item {
    fn lower(self, cx: &mut Cx<'_>) -> Item {
        match self {
            Self::Function(fun) => Item {
                id: None,
                name: fun.name,
                span: fun.span,
                kind: ItemKind::Function(fun.lower(cx)),
                ty: TypeId::null(),
            },
        }
    }
}

impl Lower<'_, Function> for ast::Function {
    fn lower(self, cx: &mut Cx<'_>) -> Function {
        let params = self
            .params
            .into_iter()
            .map(|p| FunctionParam { id: None, name: p.name, span: p.span, ty: TypeId::null() })
            .collect::<Vec<_>>();

        let body = self.body.lower(cx);

        Function { id: None, name: self.name, body, params, span: self.span, ty: TypeId::null() }
    }
}

impl Lower<'_, Expr> for ast::Expr {
    fn lower(self, cx: &mut Cx<'_>) -> Expr {
        match self {
            Self::Item(item) => Expr::Item(item.lower(cx)),
            Self::Return(ret) => Expr::Return(Return {
                expr: ret.expr.map_or_else(
                    || {
                        Box::new(Expr::Lit(Lit {
                            kind: LitKind::Unit,
                            span: ret.span,
                            ty: TypeId::null(),
                        }))
                    },
                    |v| Box::new(v.lower(cx)),
                ),
                span: ret.span,
                ty: TypeId::null(),
            }),
            Self::If(if_) => Expr::If(If {
                cond: Box::new(if_.cond.lower(cx)),
                then: Box::new(if_.then.lower(cx)),
                otherwise: if_.otherwise.map(|o| Box::new(o.lower(cx))),
                span: if_.span,
                ty: TypeId::null(),
            }),
            Self::Block(blk) => Expr::Block(blk.lower(cx)),
            Self::Call(call) => Expr::Call(Call {
                callee: Box::new(call.callee.lower(cx)),
                args: call.args.into_iter().map(|arg| arg.lower(cx)).collect(),
                span: call.span,
                ty: TypeId::null(),
            }),
            Self::Binary(bin) => Expr::Binary(Binary {
                lhs: Box::new(bin.lhs.lower(cx)),
                rhs: Box::new(bin.rhs.lower(cx)),
                op: bin.op,
                span: bin.span,
                ty: TypeId::null(),
            }),
            Self::Name(name) => Expr::Name(Name { id: None, name: name.name, ty: TypeId::null() }),
            Self::Lit(lit) => Expr::Lit(Lit {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => LitKind::Int(v),
                    ast::LitKind::Bool(v) => LitKind::Bool(v),
                    ast::LitKind::Unit => LitKind::Unit,
                },
                span: lit.span,
                ty: TypeId::null(),
            }),
        }
    }
}

impl Lower<'_, CallArg> for ast::CallArg {
    fn lower(self, cx: &mut Cx<'_>) -> CallArg {
        match self {
            Self::Positional(expr) => CallArg::Positional(expr.lower(cx)),
            Self::Named(name, expr) => CallArg::Named(name, expr.lower(cx)),
        }
    }
}
impl Lower<'_, Block> for ast::Block {
    fn lower(self, cx: &mut Cx<'_>) -> Block {
        Block {
            exprs: self.exprs.into_iter().map(|e| e.lower(cx)).collect(),
            span: self.span,
            ty: TypeId::null(),
        }
    }
}
