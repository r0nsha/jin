use std::io;

use crate::{
    db::Db,
    hir::{Expr, ExprKind, Fn, Hir, Item, ItemKind, LitKind},
};

pub(super) fn print(db: &Db, hir: &Hir) -> io::Result<()> {
    let mut cx = PPCtxt { db, builder: ptree::TreeBuilder::new("Hir".to_string()) };

    for item in &hir.items {
        item.pretty_print(&mut cx);
    }

    let tree = cx.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct PPCtxt<'db> {
    db: &'db Db,
    builder: ptree::TreeBuilder,
}

trait PrettyPrint {
    fn pretty_print(&self, cx: &mut PPCtxt);
}

impl PrettyPrint for Expr {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        match &self.kind {
            ExprKind::If(if_) => {
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
            ExprKind::Block(blk) => {
                cx.builder.begin_child("block".to_string());

                for expr in &blk.exprs {
                    expr.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            ExprKind::Return(ret) => {
                cx.builder.begin_child("return".to_string());
                ret.expr.pretty_print(cx);
                cx.builder.end_child();
            }
            ExprKind::Call(call) => {
                cx.builder.begin_child(format!("call (result: {})", self.ty.display(cx.db)));
                call.callee.pretty_print(cx);

                if !call.args.is_empty() {
                    cx.builder.begin_child("args".to_string());

                    for arg in &call.args {
                        if let Some(name) = arg.name {
                            cx.builder.begin_child(name.to_string());
                            arg.expr.pretty_print(cx);
                            cx.builder.end_child();
                        } else {
                            arg.expr.pretty_print(cx);
                        }
                    }

                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            ExprKind::Bin(bin) => {
                cx.builder.begin_child(format!("{} (result: {})", bin.op, self.ty.display(cx.db)));
                bin.lhs.pretty_print(cx);
                bin.rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            ExprKind::Name(inner) => {
                let ref this = inner;
                let cx: &mut PPCtxt = cx;
                cx.builder.add_empty_child(format!(
                    "`{}` (type: {})",
                    cx.db[this.id].qpath,
                    self.ty.display(cx.db)
                ));
            }
            ExprKind::Lit(lit) => {
                let value_str = match &lit.kind {
                    LitKind::Int(v) => v.to_string(),
                    LitKind::Bool(v) => v.to_string(),
                    LitKind::Unit => "()".to_string(),
                };

                cx.builder
                    .add_empty_child(format!("{value_str} (type: {})", self.ty.display(cx.db)));
            }
        }
    }
}

impl PrettyPrint for Item {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        match &self.kind {
            ItemKind::Fn(fun) => fun.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Fn {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        cx.builder.begin_child(format!(
            "fn {} (returns: {})",
            cx.db[self.id].qpath,
            self.ty.as_fn().expect("to be a function").ret.display(cx.db)
        ));

        if !self.sig.params.is_empty() {
            cx.builder.begin_child("params".to_string());

            for param in &self.sig.params {
                cx.builder.add_empty_child(format!(
                    "{} (type: {})",
                    cx.db[param.id].name,
                    param.ty.display(cx.db)
                ));
            }

            cx.builder.end_child();
        }

        self.body.pretty_print(cx);

        cx.builder.end_child();
    }
}
