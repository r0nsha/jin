use std::io;

use crate::db::Database;

use super::{Block, Call, Definition, DefinitionKind, Function, Lit, LitKind, Module, Name, Node, Return};

pub(super) fn print_module(db: &Database, module: &Module) -> io::Result<()> {
    let mut cx = Cx { db, builder: ptree::TreeBuilder::new(db[module.id].name.standard_full_name()) };

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

impl PrettyPrint for Node {
    fn pretty_print(&self, cx: &mut Cx) {
        match self {
            Self::Function(x) => x.pretty_print(cx),
            Self::Block(x) => x.pretty_print(cx),
            Self::Return(x) => x.pretty_print(cx),
            Self::Call(x) => x.pretty_print(cx),
            Self::Name(x) => x.pretty_print(cx),
            Self::Lit(x) => x.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Definition {
    fn pretty_print(&self, cx: &mut Cx) {
        match &self.kind {
            DefinitionKind::Function(fun) => fun.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.begin_child(format!("fn {} (type: {})", self.name, cx.db[self.ty].display(cx.db)));

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

        if let Some(expr) = self.expr.as_ref() {
            expr.pretty_print(cx);
        }

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

impl PrettyPrint for Name {
    fn pretty_print(&self, cx: &mut Cx) {
        cx.builder.add_empty_child(format!(
            "{} (type: {})",
            self.id.map_or(self.name.to_string(), |id| { cx.db[id].qualified_name.standard_full_name() }),
            cx.db[self.ty].display(cx.db)
        ));
    }
}

impl PrettyPrint for Lit {
    fn pretty_print(&self, cx: &mut Cx) {
        match &self.kind {
            LitKind::Int(v) => {
                cx.builder.add_empty_child(format!("{v} (type: {})", cx.db[self.ty].display(cx.db)));
            }
            LitKind::Unit => {
                cx.builder.add_empty_child("()".to_string());
            }
        }
    }
}
