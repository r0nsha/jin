use crate::{
    ast::AttrKind,
    db::{ExternLib, ModuleId},
    hir::{const_eval::Const, Attr},
    typeck::{
        error::TypeckError,
        tcx::{Env, TyCx},
        unify::Obligation,
        TypeckResult,
    },
};

impl<'db> TyCx<'db> {
    // TODO: check that attrs are valid on the item they're placed on
    pub fn typeck_attrs(&mut self, module_id: ModuleId, attrs: &mut [Attr]) -> TypeckResult<()> {
        for attr in attrs {
            let (value, value_ty, value_span) =
                if let Some(value) = &mut attr.value {
                    self.typeck_expr(value, &mut Env::new(module_id, None), None)?;

                    let const_ =
                        self.db.const_storage.expr(value.id).cloned().ok_or(
                            TypeckError::NonConstAttrValue { ty: value.ty, span: value.span },
                        )?;
                    (const_, value.ty, value.span)
                } else {
                    (Const::Bool(true), self.db.types.bool, attr.span)
                };

            match attr.kind {
                AttrKind::Link => {
                    self.at(Obligation::obvious(value_span)).eq(self.db.types.str, value_ty)?;

                    let lib = {
                        let path = *value.as_str().unwrap();
                        let sources = &self.db.sources.borrow();
                        let relative_to =
                            sources[self.db[module_id].source_id].path().parent().unwrap();
                        ExternLib::try_from_str(&path, relative_to)
                            .ok_or(TypeckError::PathNotFound { path, span: value_span })?
                    };

                    self.db.extern_libs.insert(lib);
                }
            }
        }

        Ok(())
    }

    pub fn validate_attrs(attrs: &[Attr], placement: AttrsPlacement) -> TypeckResult<()> {
        for attr in attrs {
            match (attr.kind, placement) {
                (AttrKind::Link, AttrsPlacement::ExternFn | AttrsPlacement::ExternLet) => (),
                (kind, _) => {
                    return Err(TypeckError::InvalidAttrPlacement { kind, span: attr.span })
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AttrsPlacement {
    Fn,
    ExternFn,
    Let,
    ExternLet,
}
