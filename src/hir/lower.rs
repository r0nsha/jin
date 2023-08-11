use crate::{
    db::{self, Database},
    parse::ast::{self, Ast},
};

use super::*;

pub(crate) fn lower(db: &mut Database, modules: Vec<ast::Module>) -> Hir {
    Hir {
        modules: modules
            .into_iter()
            .map(|module| {
                let id = db::Module::alloc(
                    db,
                    module.source,
                    module.name.clone(),
                    module.is_main,
                );
                Lower { db, id }.run(module)
            })
            .collect(),
    }
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
                .map(|tl| self.lower_top_level(tl))
                .collect(),
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
        let body = self.lower_ast(*fun.body);

        let body = if let Node::Block(mut blk) = body {
            // Automatically insert a `return` statement if the function's block is empty
            if blk.exprs.is_empty() {
                blk.exprs.push(Node::Return(Return {
                    expr: None,
                    span: blk.span,
                    ty: TyId::null(),
                }));

                blk
            } else {
                blk.fix_function_return()
            }
        } else {
            let span = body.span();

            Block { exprs: vec![body], span, ty: TyId::null() }
                .fix_function_return()
        };

        Function {
            id: FunctionId::null(),
            name: fun.name,
            body,
            span: fun.span,
            ty: TyId::null(),
        }
    }

    fn lower_ast(&mut self, ast: Ast) -> Node {
        match ast {
            Ast::Block(blk) => Node::Block(self.lower_block(blk)),
            Ast::Name(name) => Node::Name(Name {
                id: None,
                name: name.name,
                span: name.span,
                ty: TyId::null(),
            }),
            Ast::Lit(lit) => Node::Lit(Lit {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => LitKind::Int(v),
                    ast::LitKind::Unit => LitKind::Unit,
                },
                span: lit.span,
                ty: TyId::null(),
            }),
        }
    }

    fn lower_stmt(&mut self, stmt: ast::Statement) -> Node {
        match stmt {
            ast::Statement::Return(ret) => Node::Return(Return {
                expr: ret.expr.map(|v| Box::new(self.lower_ast(*v))),
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

impl Block {
    // Automatically turn the last statement in a function's block into a `return` statement, if it
    // isn't one already
    fn fix_function_return(mut self) -> Self {
        match self.exprs.last_mut() {
            Some(Node::Return(_)) => (),
            Some(last_expr) => {
                let span = last_expr.span();

                let expr = last_expr.clone();

                *last_expr = Node::Return(Return {
                    expr: Some(Box::new(expr)),
                    span,
                    ty: TyId::null(),
                })
            }
            _ => (),
        }

        self
    }
}
