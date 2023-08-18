use std::io;

use super::{Ast, Function, LitKind, Module, Statement, TopLevel};

pub(super) fn print_module(module: &Module) -> io::Result<()> {
    let mut cx = Cx { builder: ptree::TreeBuilder::new(module.name.standard_full_name()) };

    for tl in &module.top_levels {
        tl.pretty_print(&mut cx);
    }

    let tree = cx.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct Cx {
    builder: ptree::TreeBuilder,
}

trait PrettyPrint {
    fn pretty_print(&self, cx: &mut Cx);
}

impl PrettyPrint for Ast {
    fn pretty_print(&self, cx: &mut Cx) {
        match self {
            Self::If(if_) => {
                cx.builder.begin_child("if".to_string());

                cx.builder.begin_child("cond".to_string());
                if_.cond.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.begin_child("then".to_string());
                if_.then.pretty_print(cx);
                cx.builder.end_child();

                if let Some(otherwise) = &if_.otherwise {
                    cx.builder.begin_child("else".to_string());
                    otherwise.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            Self::Block(blk) => {
                cx.builder.begin_child("block".to_string());

                for stmt in &blk.stmts {
                    stmt.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::Name(name) => {
                cx.builder.add_empty_child(name.name.to_string());
            }
            Self::Call(call) => {
                cx.builder.begin_child("call".to_string());
                call.callee.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Binary(bin) => {
                cx.builder.begin_child(bin.op.to_string());
                bin.lhs.pretty_print(cx);
                bin.rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Lit(lit) => match lit.kind {
                LitKind::Int(value) => {
                    cx.builder.add_empty_child(format!("int: {value}"));
                }
                LitKind::Bool(value) => {
                    cx.builder.add_empty_child(format!("bool: {value}"));
                }
                LitKind::Unit => {
                    cx.builder.add_empty_child("()".to_string());
                }
            },
        }
    }
}

impl PrettyPrint for Statement {
    fn pretty_print(&self, cx: &mut Cx) {
        match self {
            Self::Function(fun) => fun.pretty_print(cx),
            Self::Return(ret) => {
                cx.builder.begin_child("return".to_string());

                if let Some(value) = ret.expr.as_ref() {
                    value.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::Expr(expr) => expr.pretty_print(cx),
        }
    }
}

impl PrettyPrint for TopLevel {
    fn pretty_print(&self, cx: &mut Cx) {
        match self {
            Self::Function(fun) => fun.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child(format!("fn {}", self.name));

        if !self.params.is_empty() {
            cx.builder.begin_child("params".to_string());

            for param in &self.params {
                cx.builder.add_empty_child(format!("{}", param.name));
            }

            cx.builder.end_child();
        }

        self.body.pretty_print(cx);

        cx.builder.end_child();
    }
}
