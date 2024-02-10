use crate::{
    ast::{self},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
};

pub fn validate(attrs: &ast::Attrs, placement: Placement) -> DiagnosticResult<()> {
    for attr in attrs.iter() {
        validate_placement(attr, placement)
            .map_err(|applies_to| invalid_placement(attr, applies_to))?;
    }

    Ok(())
}

/// Validates that `attr` is valid in `placement`.
/// Returns an error string with the valid placements.
fn validate_placement(attr: &ast::Attr, placement: Placement) -> Result<(), &'static str> {
    match attr.id {
        ast::AttrId::Intrinsic => match placement {
            Placement::ExternFn => Ok(()),
            _ => Err("fn extern"),
        },
    }
}

fn invalid_placement(attr: &ast::Attr, applies_to: &str) -> Diagnostic {
    Diagnostic::error(format!("attribute `{}` should be applied to {}", attr.id, applies_to))
        .with_label(Label::primary(attr.span, "invalid attribute placement"))
}

#[derive(Debug, Clone, Copy)]
pub enum Placement {
    Fn,
    ExternFn,
    Let,
    ExternLet,
    Struct,
    Union,
    Import,
    ExternImport,
}

impl From<&ast::FnKind> for Placement {
    fn from(value: &ast::FnKind) -> Self {
        match value {
            ast::FnKind::Bare { .. } => Self::Fn,
            ast::FnKind::Extern { .. } => Self::ExternFn,
        }
    }
}
