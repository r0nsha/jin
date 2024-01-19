use crate::{
    ast::{self},
    typeck::{errors::invalid_attr_placement, Typeck, TypeckResult},
};

impl<'db> Typeck<'db> {
    pub fn check_attrs(
        &mut self,
        attrs: &ast::Attrs,
        placement: AttrsPlacement,
    ) -> TypeckResult<()> {
        for attr in attrs {
            match attr.kind {
                ast::AttrKind::Intrinsic => match placement {
                    AttrsPlacement::ExternFn => todo!(),
                    _ => return Err(invalid_attr_placement(attr, "fn extern")),
                },
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
