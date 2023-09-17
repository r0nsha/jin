use crate::{
    db::ExternLib,
    hir::{const_eval::Const, Attr, AttrKind},
    typeck::{
        error::TypeckError,
        tcx::{Env, TyCtxt},
        unify::Obligation,
        TypeckResult,
    },
};

impl<'db> TyCtxt<'db> {
    pub fn typeck_attrs(&mut self, attrs: &mut [Attr], env: &mut Env) -> TypeckResult<()> {
        for attr in attrs {
            let (value, value_ty, value_span) =
                if let Some(value) = &mut attr.value {
                    self.typeck_expr(value, env, None)?;
                    let const_ =
                        self.db.const_storage.expr(value.id).cloned().ok_or(
                            TypeckError::NonConstAttrValue { ty: value.ty, span: value.span },
                        )?;
                    (const_, value.ty, value.span)
                } else {
                    (Const::Bool(true), self.db.types.bool, attr.span)
                };

            match attr.kind {
                AttrKind::Lib => {
                    self.at(Obligation::obvious(value_span)).eq(self.db.types.str, value_ty)?;
                    let path = *value.as_str().unwrap();
                    let lib = ExternLib::try_from_str(
                        &path,
                        self.db.sources.borrow()[self.db[env.module_id()].source_id].path(),
                    )
                    .ok_or(TypeckError::PathNotFound { path, span: value_span })?;
                    self.db.extern_libs.insert(lib);
                }
            }
        }

        Ok(())
    }
}
