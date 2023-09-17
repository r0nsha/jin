use crate::{
    hir::{Attr, AttrKind},
    typeck::{
        tcx::{Env, TyCtxt},
        TypeckResult,
    },
};

impl<'db> TyCtxt<'db> {
    pub fn typeck_attrs(&mut self, attrs: &mut [Attr], env: &mut Env) -> TypeckResult<()> {
        for attr in attrs {
            if let Some(value) = &mut attr.value {
                self.typeck_expr(value, env, None)?;
            }

            match attr.kind {
                AttrKind::Lib => {}
            }
        }

        Ok(())
    }
}
