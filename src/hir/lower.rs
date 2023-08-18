use super::{
    Block, Call, Definition, DefinitionKind, Expr, Function, FunctionParam, Hir, IndexMap, Lit,
    LitKind, Module, ModuleId, Name, Return, Spanned, TyId,
};
use crate::{
    ast::{self, Ast},
    db::{self, Database},
    hir::Binary,
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
        Module {
            id: self.id,
            definitions: module.top_levels.into_iter().map(|tl| tl.lower(self)).collect(),
        }
    }
}

trait Lower<'db, T> {
    fn lower(self, cx: &mut Cx<'db>) -> T;
}

impl Lower<'_, Definition> for ast::TopLevel {
    fn lower(self, cx: &mut Cx<'_>) -> Definition {
        match self {
            Self::Function(fun) => {
                let name = fun.name;
                let span = fun.span;

                Definition {
                    id: None,
                    name,
                    kind: DefinitionKind::Function(fun.lower(cx)),
                    span,
                    ty: TyId::null(),
                }
            }
        }
    }
}

impl Lower<'_, Function> for ast::Function {
    fn lower(self, cx: &mut Cx<'_>) -> Function {
        let params = self
            .params
            .into_iter()
            .map(|p| {
                (p.name, FunctionParam { id: None, name: p.name, span: p.span, ty: TyId::null() })
            })
            .collect::<IndexMap<_, _>>();

        let body = self.body.lower(cx);

        let body = if let Expr::Block(blk) = body {
            blk
        } else {
            let span = body.span();
            Block { exprs: vec![body], span, ty: TyId::null() }
        };

        Function { id: None, name: self.name, body, params, span: self.span, ty: TyId::null() }
    }
}

impl Lower<'_, Expr> for Ast {
    fn lower(self, cx: &mut Cx<'_>) -> Expr {
        match self {
            Self::Block(blk) => Expr::Block(blk.lower(cx)),
            Self::Call(call) => Expr::Call(Call {
                callee: Box::new(call.callee.lower(cx)),
                span: call.span,
                ty: TyId::null(),
            }),
            Self::Binary(bin) => Expr::Binary(Binary {
                left: Box::new(bin.left.lower(cx)),
                right: Box::new(bin.right.lower(cx)),
                op: bin.op,
                span: bin.span,
                ty: TyId::null(),
            }),
            Self::Name(name) => {
                Expr::Name(Name { id: None, name: name.name, span: name.span, ty: TyId::null() })
            }
            Self::Lit(lit) => Expr::Lit(Lit {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => LitKind::Int(v),
                    ast::LitKind::Unit => LitKind::Unit,
                },
                span: lit.span,
                ty: TyId::null(),
            }),
        }
    }
}

impl Lower<'_, Expr> for ast::Statement {
    fn lower(self, cx: &mut Cx<'_>) -> Expr {
        match self {
            Self::Return(ret) => Expr::Return(Return {
                expr: ret.expr.map_or_else(
                    || {
                        Box::new(Expr::Lit(Lit {
                            kind: LitKind::Unit,
                            span: ret.span,
                            ty: TyId::null(),
                        }))
                    },
                    |v| Box::new(v.lower(cx)),
                ),
                span: ret.span,
                ty: TyId::null(),
            }),
            Self::Expr(expr) => expr.lower(cx),
        }
    }
}

impl Lower<'_, Block> for ast::Block {
    fn lower(self, cx: &mut Cx<'_>) -> Block {
        Block {
            exprs: self.stmts.into_iter().map(|stmt| stmt.lower(cx)).collect(),
            span: self.span,
            ty: TyId::null(),
        }
    }
}
