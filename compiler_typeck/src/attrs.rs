use compiler_ast::{self as ast};
use compiler_core::diagnostics::{Diagnostic, Label};

use crate::Typeck;

pub fn validate(cx: &mut Typeck, attrs: &ast::Attrs, placement: Placement) {
    for attr in attrs.iter() {
        if let Err(applies_to) = validate_placement(attr, placement) {
            cx.db.diagnostics.add(
                Diagnostic::error(format!(
                    "attribute `{}` should be applied to {}",
                    attr.id, applies_to
                ))
                .with_label(Label::primary(attr.span, "invalid attribute placement")),
            );
        }
    }
}

/// Validates that `attr` is valid in `placement`.
/// Returns an error string with the valid placements.
fn validate_placement(attr: &ast::Attr, placement: Placement) -> Result<(), &'static str> {
    match attr.id {
        ast::AttrId::Builtin => match placement {
            Placement::ExternFn => Ok(()),
            _ => Err("fn extern"),
        },
    }
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
