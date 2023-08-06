use crate::{
    db::{self, Database},
    parse::ast::{self, Ast},
};

use super::*;

pub(crate) fn lower(db: &mut Database, modules: Vec<ast::Module>) -> Vec<Module> {
    modules
        .into_iter()
        .map(|module| {
            let id = db::Module::alloc(db, module.source, module.name.clone(), module.is_main);
            Lower { db, id }.run(module)
        })
        .collect()
}

struct Lower<'db> {
    db: &'db mut Database,
    id: ModuleId,
}

impl<'db> Lower<'db> {
    fn run(&mut self, module: ast::Module) -> Module {
        Module {
            id: self.id,
            definitions: module
                .top_level
                .into_iter()
                .map(|binding| self.lower_top_level(binding))
                .collect(),
        }
    }

    fn lower_top_level(&mut self, tl: ast::TopLevel) -> Definition {
        match tl {
            ast::TopLevel::Function(fun) => {
                let name = fun.name;
                let span = fun.span;

                Definition {
                    id: SymbolId::null(),
                    name,
                    kind: DefinitionKind::Function(self.lower_fun(fun)),
                    span,
                    ty: TyId::null(),
                }
            }
        }
    }

    fn lower_fun(&mut self, fun: ast::Function) -> Function {
        let body = self.lower_ast(*fun.body);

        let body = if let Hir::Block(block) = body {
            block
        } else {
            let span = body.span();

            Block {
                exprs: vec![body],
                span,
                ty: TyId::null(),
            }
        };

        Function {
            id: FunctionId::null(),
            name: fun.name,
            body,
            span: fun.span,
            ty: TyId::null(),
        }
    }

    fn lower_ast(&mut self, ast: Ast) -> Hir {
        match ast {
            Ast::Block(block) => Hir::Block(self.lower_block(block)),
            Ast::Ret(ret) => Hir::Return(Return {
                expr: ret.expr.map(|v| Box::new(self.lower_ast(*v))),
                span: ret.span,
                ty: TyId::null(),
            }),
            Ast::Lit(lit) => Hir::Lit(Lit {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => LitKind::Int(v),
                    ast::LitKind::Unit => LitKind::Unit,
                },
                span: lit.span,
                ty: TyId::null(),
            }),
        }
    }

    fn lower_block(&mut self, block: ast::Block) -> Block {
        Block {
            exprs: block.exprs.into_iter().map(|e| self.lower_ast(e)).collect(),
            span: block.span,
            ty: TyId::null(),
        }
    }
}
