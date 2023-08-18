use std::io;

use super::{Block, Call, Def, DefKind, Expr, Function, Lit, LitKind, Module, Name, Return};
use crate::{
    db::Database,
    hir::{Binary, If},
};

pub(super) fn print_module(db: &Database, module: &Module) -> io::Result<()> {
    let mut cx =
        Cx { db, builder: ptree::TreeBuilder::new(db[module.id].name.standard_full_name()) };

    for def in &module.definitions {
        def.pretty_print(&mut cx);
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
            Self::Def(inner) => inner.pretty_print(cx),
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

impl PrettyPrint for Def {
    fn pretty_print(&self, cx: &mut Cx) {
        match &self.kind {
            DefKind::Function(fun) => fun.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child(format!(
            "fn {} (type: {})",
            self.name,
            cx.db[self.ty].display(cx.db)
        ));

        if !self.params.is_empty() {
            cx.builder.begin_child("params".to_string());

            for param in self.params.values() {
                cx.builder.add_empty_child(format!("{}", param.name));
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
            "{} (type: {})",
            self.id.map_or(self.name.to_string(), |id| {
                cx.db[id].qualified_name.standard_full_name()
            }),
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
