use crate::{
    db::{Db, DefId, ModuleId},
    diagnostics::{Diagnostic, Label},
    span::{Span, Spanned},
    ty::Ty,
    typeck::env::{FnCandidate, FnQuery},
    word::Word,
};

pub fn field_not_found(
    db: &Db,
    expr_ty: Ty,
    expr_span: Span,
    field: Word,
) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "no field `{}` on type `{}`",
            field,
            expr_ty.display(db)
        ))
        .with_label(Label::primary(field.span()).with_message("unknown field"))
        .with_label(Label::secondary(expr_span).with_message(format!(
            "expression has type `{}`",
            expr_ty.display(db)
        )))
}

pub fn name_not_found(
    db: &Db,
    from_module: ModuleId,
    in_module: ModuleId,
    word: Word,
) -> Diagnostic {
    Diagnostic::error()
        .with_message(if from_module == in_module {
            format!("cannot find `{word}` in this scope")
        } else {
            format!(
                "cannot find `{}` in module `{}`",
                word, db[in_module].qpath
            )
        })
        .with_label(Label::primary(word.span()).with_message("not found"))
}

pub fn fn_not_found(db: &Db, query: &FnQuery) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "cannot find a function matching `{}`",
            query.display(db)
        ))
        .with_label(
            Label::primary(query.word.span())
                .with_message("no matching function"),
        )
}

pub fn private_access_violation(
    db: &Db,
    accessed: DefId,
    span: Span,
) -> Diagnostic {
    let def = &db[accessed];
    let module_name = db[def.scope.module_id].qpath.join();

    Diagnostic::error()
        .with_message(format!(
            "`{}` is private to module `{}`",
            def.name, module_name
        ))
        .with_label(
            Label::primary(span)
                .with_message(format!("private to `{module_name}`")),
        )
}

pub fn named_param_not_found(name: Word) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "cannot find parameter with the name `{}`",
            name.name()
        ))
        .with_label(
            Label::primary(name.span())
                .with_message(format!("found argument `{}` here", name.name())),
        )
}

pub fn arg_mismatch(expected: usize, found: usize, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "function takes {expected} argument(s), but {found} were supplied"
        ))
        .with_label(Label::primary(span).with_message(format!(
            "expected {expected} arguments, found {found}"
        )))
}

pub fn ty_mismatch(expected: &str, found: &str, span: Span) -> Diagnostic {
    generic_expected_found(
        &format!("type `{expected}`"),
        &format!("type `{found}`"),
        span,
    )
}

pub fn generic_expected_found(
    expected: &str,
    found: &str,
    span: Span,
) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("expected {expected}, found {found}"))
        .with_label(
            Label::primary(span)
                .with_message(format!("expected {expected} here")),
        )
}

pub fn multiple_item_def_err(prev_span: Span, dup_name: Word) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("item `{dup_name}` is defined multiple times"))
        .with_label(
            Label::primary(dup_name.span())
                .with_message(format!("`{dup_name}` defined again here")),
        )
        .with_label(
            Label::secondary(prev_span)
                .with_message(format!("first definition of `{dup_name}`")),
        )
        .with_note(
            "you can only define items once in a module (except functions)",
        )
}

pub fn multiple_fn_def_err(
    db: &Db,
    in_module: ModuleId,
    prev_span: Span,
    candidate: &FnCandidate,
) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "function `{}` is already defined in module `{}`",
            candidate.display(db),
            db[in_module].qpath
        ))
        .with_label(
            Label::primary(candidate.word.span())
                .with_message("defined again here"),
        )
        .with_label(
            Label::secondary(prev_span)
                .with_message("previous definition here"),
        )
        .with_note(
            "functions may be overloaded by their parameters' types and names",
        )
}
