use std::io;

use crate::{
    db::Db,
    hir::{Const, Expr, ExprKind, Fn, Hir},
};

pub(super) fn print(db: &Db, hir: &Hir, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PPCtxt { db, builder: ptree::TreeBuilder::new("Hir".to_string()) };

    for _ in &hir.lets {
        todo!("global variables")
    }

    for f in &hir.fns {
        cx.pp_fn(f);
    }

    let tree = cx.builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PPCtxt<'db> {
    db: &'db Db,
    builder: ptree::TreeBuilder,
}

impl PPCtxt<'_> {
    fn pp_fn(&mut self, f: &Fn) {
        self.builder.begin_child(format!(
            "fn {} (returns: {})",
            self.db[f.id].qpath,
            self.db[f.id].ty.as_fn().expect("to be a function").ret.display(self.db)
        ));

        if !f.sig.params.is_empty() {
            self.builder.begin_child("params".to_string());

            for param in &f.sig.params {
                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    self.db[param.id].name,
                    param.ty.display(self.db)
                ));
            }

            self.builder.end_child();
        }

        self.pp_expr(&f.body);

        self.builder.end_child();
    }

    fn pp_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Let(let_) => {
                self.builder.begin_child(format!("let (type: {})", let_.value.ty.display(self.db)));
                self.pp_expr(&let_.value);
                self.builder.end_child();
            }
            ExprKind::If(if_) => {
                self.builder.begin_child("if".to_string());

                self.builder.begin_child("cond".to_string());
                self.pp_expr(&if_.cond);
                self.builder.end_child();

                self.builder.begin_child("then".to_string());
                self.pp_expr(&if_.then);
                self.builder.end_child();

                if let Some(otherwise) = &if_.otherwise {
                    self.builder.begin_child("else".to_string());
                    self.pp_expr(otherwise);
                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Block(blk) => {
                self.builder.begin_child("block".to_string());

                for expr in &blk.exprs {
                    self.pp_expr(expr);
                }

                self.builder.end_child();
            }
            ExprKind::Return(ret) => {
                self.builder.begin_child("return".to_string());
                self.pp_expr(&ret.expr);
                self.builder.end_child();
            }
            ExprKind::Call(call) => {
                self.builder.begin_child(format!("call (result: {})", expr.ty.display(self.db)));
                self.pp_expr(&call.callee);

                if !call.args.is_empty() {
                    self.builder.begin_child("args".to_string());

                    for arg in &call.args {
                        if let Some(name) = arg.name {
                            self.builder.begin_child(name.to_string());
                            self.pp_expr(&arg.expr);
                            self.builder.end_child();
                        } else {
                            self.pp_expr(&arg.expr);
                        }
                    }

                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Unary(un) => {
                self.builder.begin_child(format!(
                    "{} (result: {})",
                    un.op,
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&un.expr);
                self.builder.end_child();
            }
            ExprKind::Binary(bin) => {
                self.builder.begin_child(format!(
                    "{} (result: {})",
                    bin.op,
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&bin.lhs);
                self.pp_expr(&bin.rhs);
                self.builder.end_child();
            }
            ExprKind::Cast(cast) => {
                self.builder.begin_child(format!("cast (to: {})", expr.ty.display(self.db)));
                self.pp_expr(&cast.expr);
                self.builder.end_child();
            }
            ExprKind::Name(name) => {
                self.builder.add_empty_child(format!(
                    "`{}` (type: {})",
                    self.db[name.id].qpath,
                    expr.ty.display(self.db)
                ));
            }
            ExprKind::Const(value) => {
                let value_str = match value {
                    Const::Int(v) => v.to_string(),
                    Const::Bool(v) => v.to_string(),
                    Const::Unit => "()".to_string(),
                };

                self.builder
                    .add_empty_child(format!("{value_str} (type: {})", expr.ty.display(self.db)));
            }
        }
    }
}
