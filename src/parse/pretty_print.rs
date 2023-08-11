use std::io;

use super::ast::*;

pub(super) fn print_module(module: &Module) -> io::Result<()> {
    let mut p = PrettyPrint {
        builder: ptree::TreeBuilder::new(module.name.standard_full_name()),
    };

    for tl in &module.top_levels {
        p.print_top_level(tl);
    }

    let tree = p.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct PrettyPrint {
    builder: ptree::TreeBuilder,
}

impl PrettyPrint {
    fn print_ast(&mut self, ast: &Ast) {
        match ast {
            Ast::Block(blk) => {
                self.builder.begin_child("block".to_string());

                for stmt in &blk.stmts {
                    self.print_stmt(stmt);
                }

                self.builder.end_child();
            }
            Ast::Name(name) => {
                self.builder.add_empty_child(name.name.to_string());
            }
            Ast::Call(call) => {
                self.builder.begin_child("call".to_string());
                self.print_ast(&call.callee);
                self.builder.end_child();
            }
            Ast::Lit(lit) => match lit.kind {
                LitKind::Int(value) => {
                    self.builder.add_empty_child(format!("int: {value}"));
                }
                LitKind::Unit => {
                    self.builder.add_empty_child("()".to_string());
                }
            },
        }
    }

    fn print_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Return(ret) => {
                self.builder.begin_child("return".to_string());

                if let Some(value) = ret.expr.as_ref() {
                    self.print_ast(value);
                }

                self.builder.end_child();
            }
            Statement::Expr(expr) => self.print_ast(expr),
        }
    }

    fn print_top_level(&mut self, tl: &TopLevel) {
        match tl {
            TopLevel::Function(fun) => self.print_fun(fun),
        }
    }

    fn print_fun(&mut self, fun: &Function) {
        self.builder.begin_child(format!("fn {}", fun.name));
        self.print_ast(&fun.body);
        self.builder.end_child();
    }
}
