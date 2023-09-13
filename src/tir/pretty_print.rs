use std::io;

use crate::{db::Db, tir::*};

pub(super) fn print(db: &Db, tir: &Tir, w: &mut impl io::Write) -> io::Result<()> {
    let mut builder = ptree::TreeBuilder::new("Tir".to_string());

    for f in tir.functions.iter() {
        PPCtxt { builder: &mut builder, db, tir, f }.pp_fn(f);
    }

    let tree = builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PPCtxt<'db> {
    builder: &'db mut ptree::TreeBuilder,
    db: &'db Db,
    tir: &'db Tir,
    f: &'db Fn,
}

impl PPCtxt<'_> {
    fn pp_fn(&mut self, f: &Fn) {
        self.builder.begin_child(format!(
            "fn {} (returns: {})",
            self.tir.sigs[f.sig].name,
            self.tir.sigs[f.sig].ty.as_fn().expect("to be a function").ret.display(self.db)
        ));

        let sig = &self.tir.sigs[f.sig];

        if !sig.params.is_empty() {
            self.builder.begin_child("params".to_string());

            for param in &sig.params {
                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    // TODO: LocalId
                    self.db[param.def_id].name,
                    param.ty.display(self.db)
                ));
            }

            self.builder.end_child();
        }

        self.pp_expr(f.body);

        self.builder.end_child();
    }

    fn pp_expr(&mut self, expr: ExprId) {
        let expr = self.f.expr(expr);

        match &expr.kind {
            ExprKind::Let { def_id, value } => {
                self.builder.begin_child(format!(
                    "let {} (type: {})",
                    // TODO: LocalId
                    self.db[*def_id].qpath.standard_full_name(),
                    self.f.expr(*value).ty.display(self.db)
                ));
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::If { cond, then, otherwise } => {
                self.builder.begin_child("if".to_string());

                self.builder.begin_child("cond".to_string());
                self.pp_expr(*cond);
                self.builder.end_child();

                self.builder.begin_child("then".to_string());
                self.pp_expr(*then);
                self.builder.end_child();

                if let Some(otherwise) = *otherwise {
                    self.builder.begin_child("else".to_string());
                    self.pp_expr(otherwise);
                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Block { exprs } => {
                self.builder.begin_child("block".to_string());

                for expr in exprs {
                    self.pp_expr(*expr);
                }

                self.builder.end_child();
            }
            ExprKind::Return { value } => {
                self.builder.begin_child("return".to_string());
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Call { callee, args } => {
                self.builder.begin_child(format!("call (result: {})", expr.ty.display(self.db)));
                self.pp_expr(*callee);

                if !args.is_empty() {
                    self.builder.begin_child("args".to_string());
                    for arg in args {
                        self.pp_expr(*arg);
                    }
                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Binary { lhs, rhs, op } => {
                self.builder.begin_child(format!("{} (result: {})", op, expr.ty.display(self.db)));
                self.pp_expr(*lhs);
                self.pp_expr(*rhs);
                self.builder.end_child();
            }
            ExprKind::Unary { value, op } => {
                self.builder.begin_child(format!("{} (result: {})", op, expr.ty.display(self.db)));
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Cast { value, target } => {
                self.builder.begin_child(format!("cast (to: {})", target.display(self.db)));
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Id { id } => match id {
                Id::Fn(fid) => {
                    self.builder.add_empty_child(format!(
                        "`{}` (type: {})",
                        self.tir.functions[*fid].name,
                        expr.ty.display(self.db)
                    ));
                }
                Id::Local(lid) => {
                    self.builder.add_empty_child(format!(
                        "`{}` (type: {})",
                        self.db[*lid].name,
                        expr.ty.display(self.db)
                    ));
                }
            },
            ExprKind::IntLit { value } => {
                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    value,
                    expr.ty.display(self.db)
                ));
            }
            ExprKind::BoolLit { value } => {
                self.builder.add_empty_child(value.to_string());
            }
            ExprKind::UnitLit => {
                self.builder.add_empty_child("()".to_string());
            }
        }
    }
}
