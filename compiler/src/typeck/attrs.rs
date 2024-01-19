use crate::{
    ast::{self},
    typeck::{errors::invalid_attr_placement, Typeck, TypeckResult},
};

#[derive(Debug, Clone, Copy)]
pub enum AttrsPlacement {
    Fn,
    ExternFn,
    Let,
    ExternLet,
}

impl<'db> Typeck<'db> {
    #[allow(clippy::unused_self)]
    pub fn check_attrs(
        &self,
        attrs: &ast::Attrs,
        placement: AttrsPlacement,
    ) -> TypeckResult<()> {
        for attr in attrs {
            validate_attr_placement(attr, placement).map_err(|applies_to| {
                invalid_attr_placement(attr, applies_to)
            })?;
        }

        Ok(())
    }
}

fn validate_attr_placement(
    attr: &ast::Attr,
    placement: AttrsPlacement,
) -> Result<(), &'static str> {
    match attr.kind {
        ast::AttrKind::Intrinsic => match placement {
            AttrsPlacement::ExternFn => Ok(()),
            _ => Err("fn extern"),
        },
    }
}
