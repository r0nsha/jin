use crate::{db::Database, hir};

use super::{builder::FunctionBuilder, *};

pub(crate) fn lower(db: &Database, hir: Vec<hir::Module>) -> Mir {
    let mut mir = Mir::new();

    for module in &hir {
        for def in &module.definitions {
            match &def.kind {
                hir::DefinitionKind::Function(fun) => {
                    let fun = LowerCx::new(db, fun.id).lower(fun).unwrap();
                    mir.add_function(fun);
                }
            }
        }
    }

    mir
}

struct LowerCx<'db> {
    db: &'db Database,
    builder: FunctionBuilder,
}

impl<'db> LowerCx<'db> {
    fn new(db: &'db Database, fun_id: FunctionId) -> Self {
        Self {
            db,
            builder: FunctionBuilder::new(fun_id),
        }
    }

    fn lower(mut self, fun: &hir::Function) -> Result<Function, String> {
        let blk_start = self.builder.create_block();
        self.builder.position_at(blk_start);

        self.lower_block(&fun.body);

        self.builder.finish()
    }

    fn lower_block(&mut self, blk: &hir::Block) {
        todo!()
    }
}
