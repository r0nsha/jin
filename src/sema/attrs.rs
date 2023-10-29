use crate::{
    ast::{self},
    db::ModuleId,
    sema::{CheckResult, Sema},
};

impl<'db> Sema<'db> {
    // TODO: remove attrs when we implement some attribute
    #[allow(unused, clippy::unnecessary_wraps, clippy::unused_self)]
    pub fn check_attrs(
        &mut self,
        module_id: ModuleId,
        attrs: &ast::Attrs,
        placement: AttrsPlacement,
    ) -> CheckResult<()> {
        // for attr in attrs {
        //     let attr_value =
        //         if let Some(value) = &attr.value {
        //             let value = self.check_expr(&mut Env::new(module_id), value, None)?;
        //
        //             let const_ =
        //                 self.db.const_storage.expr(value.id).cloned().ok_or(
        //                     CheckError::NonConstAttrValue { ty: value.ty, span: value.span },
        //                 )?;
        //
        // Self::NonConstAttrValue { ty, span } => {
        //     Diagnostic::error("check::non_const_attr_value")
        //         .with_message(format!(
        //     "value of type `{}` must resolve to a const, because it is passed to an attribute",
        //     ty.display(db),
        // ))
        //         .with_label(Label::primary(span).with_message("not const"))
        // }
        //
        //             AttrValue { value: const_, ty: value.ty, span: value.span }
        //         } else {
        //             AttrValue { value: Const::Bool(true), ty: self.db.types.bool, span: attr.span }
        //         };
        //
        //     match attr.kind {}
        // }

        // for attr in attrs {
        //     match (attr.kind, placement) {
        //         (AttrKind::Link, AttrsPlacement::ExternFn | AttrsPlacement::ExternLet) => (),
        //         (kind, _) => {
        //             return Err(CheckError::InvalidAttrPlacement { kind, span: attr.span })
        //
        // Self::InvalidAttrPlacement { kind, span } => {
        //     Diagnostic::error("check::invalid_attr_placement")
        //         .with_message(format!("attribute `{kind}` cannot be placed here"))
        //         .with_label(Label::primary(span).with_message("invalid attribute"))
        // }
        //         }
        //     }
        // }

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

// #[derive(Debug)]
// struct AttrValue {
//     value: Const,
//     ty: Ty,
//     span: Span,
// }
