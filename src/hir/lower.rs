use crate::{
    ast::{self, Ast},
    db::{self, Database},
};

use super::{
    Block, Call, Definition, DefinitionKind, Function, FunctionParam, Hir, IndexMap, Lit, LitKind,
    Module, ModuleId, Name, Expr, Return, Spanned, TyId,
};

pub fn lower(db: &mut Database, lib: ast::Library) -> Hir {
    Hir {
        modules: lib
            .modules
            .into_iter()
            .map(|module| {
                let id =
                    db::ModuleInfo::alloc(db, module.source, module.name.clone(), module.is_main());
                Lower { db, id }.run(module)
            })
            .collect(),
    }
}

struct Lower<'db> {
    #[allow(unused)]
    db: &'db mut Database,
    id: ModuleId,
}

impl<'db> Lower<'db> {
    fn run(&mut self, module: ast::Module) -> Module {
        Module {
            id: self.id,
            definitions: module.top_levels.into_iter().map(|tl| self.lower_top_level(tl)).collect(),
        }
    }

    fn lower_top_level(&mut self, tl: ast::TopLevel) -> Definition {
        match tl {
            ast::TopLevel::Function(fun) => {
                let name = fun.name;
                let span = fun.span;

                Definition {
                    id: None,
                    name,
                    kind: DefinitionKind::Function(self.lower_fun(fun)),
                    span,
                    ty: TyId::null(),
                }
            }
        }
    }

    fn lower_fun(&mut self, fun: ast::Function) -> Function {
        let params = fun
            .params
            .into_iter()
            .map(|p| {
                (p.name, FunctionParam { id: None, name: p.name, span: p.span, ty: TyId::null() })
            })
            .collect::<IndexMap<_, _>>();

        let body = self.lower_ast(*fun.body);

        let body = if let Expr::Block(blk) = body {
            blk
        } else {
            let span = body.span();
            Block { exprs: vec![body], span, ty: TyId::null() }
        };

        Function { id: None, name: fun.name, body, params, span: fun.span, ty: TyId::null() }
    }

    fn lower_ast(&mut self, ast: Ast) -> Expr {
        match ast {
            Ast::Block(blk) => Expr::Block(self.lower_block(blk)),
            Ast::Call(call) => Expr::Call(Call {
                callee: Box::new(self.lower_ast(*call.callee)),
                span: call.span,
                ty: TyId::null(),
            }),
            Ast::Binary(bin) => {
                todo!()
            }
            Ast::Name(name) => {
                Expr::Name(Name { id: None, name: name.name, span: name.span, ty: TyId::null() })
            }
            Ast::Lit(lit) => Expr::Lit(Lit {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => LitKind::Int(v),
                    ast::LitKind::Unit => LitKind::Unit,
                },
                span: lit.span,
                ty: TyId::null(),
            }),
        }
    }

    fn lower_stmt(&mut self, stmt: ast::Statement) -> Expr {
        match stmt {
            ast::Statement::Return(ret) => Expr::Return(Return {
                expr: ret.expr.map_or_else(
                    || {
                        Box::new(Expr::Lit(Lit {
                            kind: LitKind::Unit,
                            span: ret.span,
                            ty: TyId::null(),
                        }))
                    },
                    |v| Box::new(self.lower_ast(*v)),
                ),
                span: ret.span,
                ty: TyId::null(),
            }),
            ast::Statement::Expr(expr) => self.lower_ast(expr),
        }
    }

    fn lower_block(&mut self, blk: ast::Block) -> Block {
        Block {
            exprs: blk.stmts.into_iter().map(|e| self.lower_stmt(e)).collect(),
            span: blk.span,
            ty: TyId::null(),
        }
    }
}
