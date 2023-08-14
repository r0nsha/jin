use std::io;

use pretty::RcDoc;

use crate::{
    db::{Database, TyId},
    ty::Ty,
};

use super::*;

pub(super) fn print_module(db: &Database, module: &Module) -> io::Result<()> {
    let mut cx = Cx {
        db,
        builder: ptree::TreeBuilder::new(
            db[module.id].name.standard_full_name(),
        ),
    };

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
            Node::Function(x) => x.pretty_print(cx),
            Node::Block(x) => x.pretty_print(cx),
            Node::Return(x) => x.pretty_print(cx),
            Node::Call(x) => x.pretty_print(cx),
            Node::Name(x) => x.pretty_print(cx),
            Node::Lit(x) => x.pretty_print(cx),
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
        // TODO: types
        cx.builder.begin_child(format!("fn {}", self.name));

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
        // TODO: type
        cx.builder.begin_child("call".to_string());
        self.callee.pretty_print(cx);
        cx.builder.end_child();
    }
}

impl PrettyPrint for Name {
    fn pretty_print(&self, cx: &mut Cx) {
        // TODO: type
        cx.builder.add_empty_child(
            self.id.map_or(self.name.to_string(), |id| {
                cx.db[id].qualified_name.standard_full_name()
            }),
        );
    }
}

impl PrettyPrint for Lit {
    fn pretty_print(&self, cx: &mut Cx) {
        match &self.kind {
            LitKind::Int(v) => {
                cx.builder.add_empty_child(format!(
                    "{v} {}",
                    cx.db[self.ty].display(&cx.db)
                ));
            }
            LitKind::Unit => {
                cx.builder.add_empty_child("()".to_string());
            }
        }
    }
}
