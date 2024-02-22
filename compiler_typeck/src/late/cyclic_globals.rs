use compiler_core::{
    db::{Db, DefId, DefKind},
    diagnostics::{Diagnostic, Label},
    hir::{visit::Visitor, Expr, Hir, Name},
    middle::Pat,
    span::Span,
};
use compiler_data_structures::index_vec::Key as _;
use graph_cycles::Cycles;
use petgraph::{stable_graph::NodeIndex, Graph};
use rustc_hash::FxHashMap;

pub fn cyclic_globals(db: &mut Db, hir: &Hir) {
    let mut cx = CyclicGlobals {
        db,
        indices: FxHashMap::default(),
        graph: Graph::new(),
        curr_id: DefId::null(),
    };
    cx.run(hir);
}

struct CyclicGlobals<'db> {
    db: &'db mut Db,
    indices: FxHashMap<DefId, NodeIndex>,
    graph: Graph<DefId, Use>,
    curr_id: DefId,
}

#[derive(Debug, Clone, Copy)]
struct Use(Span);

impl CyclicGlobals<'_> {
    fn run(&mut self, hir: &Hir) {
        for let_ in &hir.lets {
            match &let_.pat {
                Pat::Name(name) => {
                    let ix = self.graph.add_node(name.id);
                    self.indices.insert(name.id, ix);
                }
                Pat::Discard(_) => (),
            }
        }

        for let_ in &hir.lets {
            match &let_.pat {
                Pat::Name(name) => {
                    self.curr_id = name.id;
                    self.visit_expr(&let_.value);
                }
                Pat::Discard(_) => (),
            }
        }

        self.graph.visit_all_cycles(|g, cycle| match cycle {
            &[ix] => {
                let id = g[ix];
                let use_ = g.edges_connecting(ix, ix).next().unwrap().weight();
                self.db.diagnostics.add(
                    Diagnostic::error(format!(
                        "global variable `{}` references itself",
                        self.db[id].name
                    ))
                    .with_label(Label::primary(use_.0, "cycle here"))
                    .with_label(Label::secondary(self.db[id].span, "defined here")),
                );
            }
            cycle => {
                let root_ix = cycle[0];
                let root_id = g[root_ix];

                let mut use_num = 0;
                let use_labels: Vec<_> = cycle
                    .windows(2)
                    .map(|w| {
                        use_num += 1;
                        let (from, to) = (w[0], w[1]);
                        Self::get_use_label(self.db, g, from, to, use_num)
                    })
                    .collect();

                let last_use_label =
                    Self::get_use_label(self.db, g, *cycle.last().unwrap(), root_ix, use_num + 1);

                self.db.diagnostics.add(
                    Diagnostic::error(format!(
                        "global variable `{}` is cyclic",
                        self.db[root_id].name
                    ))
                    .with_label(Label::primary(self.db[root_id].span, "defined here"))
                    .with_labels(use_labels)
                    .with_label(last_use_label),
                );
            }
        });
    }

    fn get_use_label(
        db: &Db,
        graph: &Graph<DefId, Use>,
        from: NodeIndex,
        to: NodeIndex,
        use_num: usize,
    ) -> Label {
        let (by, used) = (db[graph[from]].name, db[graph[to]].name);
        let use_span = graph.edges_connecting(from, to).next().unwrap().weight().0;
        let msg = format!("{use_num}. `{used}` used by `{by}`...");
        Label::secondary(use_span, msg)
    }

    #[track_caller]
    fn curr_ix(&self) -> NodeIndex {
        self.indices[&self.curr_id]
    }

    #[track_caller]
    fn ix(&self, id: DefId) -> NodeIndex {
        self.indices[&id]
    }
}

impl Visitor for CyclicGlobals<'_> {
    fn visit_name(&mut self, expr: &Expr, name: &Name) {
        if let DefKind::Global = self.db[name.id].kind.as_ref() {
            self.graph.add_edge(self.curr_ix(), self.ix(name.id), Use(expr.span));
        }
    }
}
