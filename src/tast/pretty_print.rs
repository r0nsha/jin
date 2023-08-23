use std::io;

use crate::{
    db::Database,
    tast::{
        Binary, Block, Call, CallArg, Expr, Function, If, Item, ItemKind, Lit, LitKind, Name,
        Return, TypedAst,
    },
};

pub(super) fn print(db: &Database, tast: &TypedAst) -> io::Result<()> {
    let mut cx = Cx { db, builder: ptree::TreeBuilder::new("Typed Ast".to_string()) };

    for item in &tast.items {
        item.pretty_print(&mut cx);
    }

    let tree = cx.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct Cx<'db> {
    db: &'db Database,
    builder: ptree::TreeBuilder,
}

trait PrettyPrint {
    fn pretty_print(&self, cx: &mut Cx);
}

impl PrettyPrint for Expr {
    fn pretty_print(&self, cx: &mut Cx) {
        match self {
            Self::Item(inner) => inner.pretty_print(cx),
            Self::If(inner) => inner.pretty_print(cx),
            Self::Block(inner) => inner.pretty_print(cx),
            Self::Return(inner) => inner.pretty_print(cx),
            Self::Call(inner) => inner.pretty_print(cx),
            Self::Binary(inner) => inner.pretty_print(cx),
            Self::Name(inner) => inner.pretty_print(cx),
            Self::Lit(inner) => inner.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Item {
    fn pretty_print(&self, cx: &mut Cx) {
        match &self.kind {
            ItemKind::Function(fun) => fun.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child(format!(
            "fn {} (returns: {})",
            cx.db[self.id].qname,
            cx.db[self.ty].as_function().expect("to be a function").ret.display(cx.db)
        ));

        if !self.sig.params.is_empty() {
            cx.builder.begin_child("params".to_string());

            for param in &self.sig.params {
                cx.builder.add_empty_child(format!(
                    "{} (type: {})",
                    cx.db[param.id].name,
                    cx.db[param.ty].display(cx.db)
                ));
            }

            cx.builder.end_child();
        }

        self.body.pretty_print(cx);

        cx.builder.end_child();
    }
}

impl PrettyPrint for If {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child("if".to_string());

        cx.builder.begin_child("cond".to_string());
        self.cond.pretty_print(cx);
        cx.builder.end_child();

        cx.builder.begin_child("then".to_string());
        self.then.pretty_print(cx);
        cx.builder.end_child();

        if let Some(otherwise) = &self.otherwise {
            cx.builder.begin_child("else".to_string());
            otherwise.pretty_print(cx);
            cx.builder.end_child();
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for Block {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child("block".to_string());

        for expr in &self.exprs {
            expr.pretty_print(cx);
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for Return {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child("return".to_string());
        self.expr.pretty_print(cx);
        cx.builder.end_child();
    }
}

impl PrettyPrint for Call {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child(format!("call (result: {})", cx.db[self.ty].display(cx.db)));
        self.callee.pretty_print(cx);

        if !self.args.is_empty() {
            cx.builder.begin_child("args".to_string());

            for arg in &self.args {
                match arg {
                    CallArg::Positional(expr) => expr.pretty_print(cx),
                    CallArg::Named(name, expr) => {
                        cx.builder.begin_child(name.to_string());
                        expr.pretty_print(cx);
                        cx.builder.end_child();
                    }
                }
            }

            cx.builder.end_child();
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for Binary {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child(format!("{} (result: {})", self.op, cx.db[self.ty].display(cx.db)));
        self.lhs.pretty_print(cx);
        self.rhs.pretty_print(cx);
        cx.builder.end_child();
    }
}

impl PrettyPrint for Name {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.add_empty_child(format!(
            "`{}` (type: {})",
            cx.db[self.id].qname,
            cx.db[self.ty].display(cx.db)
        ));
    }
}

impl PrettyPrint for Lit {
    fn pretty_print(&self, cx: &mut Cx) {
        let value_str = match &self.kind {
            LitKind::Int(v) => v.to_string(),
            LitKind::Bool(v) => v.to_string(),
            LitKind::Unit => "()".to_string(),
        };

        cx.builder
            .add_empty_child(format!("{value_str} (type: {})", cx.db[self.ty].display(cx.db)));
    }
}
