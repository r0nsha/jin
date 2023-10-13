use crate::{
    ast::{self, AttrKind},
    db::{ExternLib, ModuleId},
    hir::const_eval::Const,
    sema::{env::Env, error::CheckError, unify::Obligation, CheckResult, Sema},
    span::Span,
    ty::Ty,
};

impl<'db> Sema<'db> {
    pub fn check_attrs(
        &mut self,
        module_id: ModuleId,
        attrs: &ast::Attrs,
        placement: AttrsPlacement,
    ) -> CheckResult<()> {
        for attr in attrs {
            let attr_value =
                if let Some(value) = &attr.value {
                    let value = self.check_expr(&mut Env::new(module_id), value, None)?;

                    let const_ =
                        self.db.const_storage.expr(value.id).cloned().ok_or(
                            CheckError::NonConstAttrValue { ty: value.ty, span: value.span },
                        )?;

                    AttrValue { value: const_, ty: value.ty, span: value.span }
                } else {
                    AttrValue { value: Const::Bool(true), ty: self.db.types.bool, span: attr.span }
                };

            match attr.kind {
                AttrKind::Link => {
                    self.at(Obligation::obvious(attr_value.span))
                        .eq(self.db.types.str, attr_value.ty)?;

                    let lib = {
                        let path = *attr_value.value.as_str().unwrap();
                        let sources = &self.db.sources.borrow();
                        let relative_to =
                            sources[self.db[module_id].source_id].path().parent().unwrap();

                        ExternLib::try_from_str(&path, relative_to)
                            .ok_or(CheckError::PathNotFound { path, span: attr_value.span })?
                    };

                    self.db.extern_libs.insert(lib);
                }
            }
        }

        for attr in attrs {
            match (attr.kind, placement) {
                (AttrKind::Link, AttrsPlacement::ExternFn | AttrsPlacement::ExternLet) => (),
                (kind, _) => {
                    return Err(CheckError::InvalidAttrPlacement { kind, span: attr.span })
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

#[derive(Debug)]
struct AttrValue {
    value: Const,
    ty: Ty,
    span: Span,
}
