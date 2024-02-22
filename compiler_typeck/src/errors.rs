use ustr::Ustr;

use compiler_core::{
    db::{Adt, AdtField, AdtId, Db, ModuleId},
    diagnostics::{Diagnostic, Label},
    middle::{BinOp, UnOp},
    span::{Span, Spanned},
    ty::Ty,
    word::Word,
};

use crate::lookup::{FnCandidate, FnQuery, Query};

pub fn field_not_found(db: &Db, ty: Ty, span: Span, field: Word) -> Diagnostic {
    Diagnostic::error(format!("no field `{}` in type `{}`", field, ty.display(db)))
        .with_label(Label::primary(field.span(), format!("unknown field `{field}`")))
        .with_label(Label::secondary(span, format!("has type `{}`", ty.display(db))))
}

pub fn variant_not_found(db: &Db, adt_id: AdtId, span: Span, variant: Word) -> Diagnostic {
    let adt_ty = db[adt_id].ty();
    Diagnostic::error(format!("no variant `{}` in type `{}`", variant, adt_ty.display(db)))
        .with_label(Label::primary(variant.span(), format!("unknown variant `{variant}`")))
        .with_label(Label::secondary(span, format!("has type `{}`", adt_ty.display(db))))
}

pub fn name_not_found(
    db: &Db,
    from_module: ModuleId,
    in_module: ModuleId,
    word: Word,
) -> Diagnostic {
    Diagnostic::error(if from_module == in_module {
        format!("cannot find `{word}` in this scope")
    } else {
        format!("cannot find `{}` in module `{}`", word, db[in_module].qpath)
    })
    .with_label(Label::primary(word.span(), "not found"))
}

pub fn assoc_name_not_found(db: &Db, ty: Ty, query: &Query) -> Diagnostic {
    let msg = match query {
        Query::Name(word) => {
            format!("cannot find associated name `{}` in type `{}`", word, ty.display(db))
        }
        Query::Fn(fn_query) => {
            format!(
                "cannot find a matching associated function `{}` in type `{}`",
                fn_query.display(db),
                ty.display(db)
            )
        }
    };

    Diagnostic::error(msg).with_label(Label::primary(query.span(), "not found"))
}

pub fn fn_not_found(db: &Db, query: &FnQuery) -> Diagnostic {
    Diagnostic::error(format!("cannot find a function matching `{}`", query.display(db)))
        .with_label(Label::primary(query.word.span(), "no matching function"))
}

pub fn private_access_violation(
    db: &Db,
    module_id: ModuleId,
    name: Ustr,
    span: Span,
) -> Diagnostic {
    let module_name = db[module_id].qpath.join();
    Diagnostic::error(format!("`{name}` is private to module `{module_name}`"))
        .with_label(Label::primary(span, format!("private to `{module_name}`")))
}

pub fn named_param_not_found(name: Word) -> Diagnostic {
    Diagnostic::error(format!("cannot find parameter with the name `{}`", name.name()))
        .with_label(Label::primary(name.span(), format!("found argument `{}` here", name.name())))
}

pub fn arg_mismatch(expected: usize, found: usize, span: Span) -> Diagnostic {
    Diagnostic::error(format!("function takes {expected} argument(s), but {found} were supplied"))
        .with_label(Label::primary(span, format!("expected {expected} arguments, found {found}")))
}

pub fn ty_mismatch(expected: &str, found: &str, span: Span) -> Diagnostic {
    generic_expected_found(&format!("type `{expected}`"), &format!("type `{found}`"), span)
}

pub fn expected_module(found: impl core::fmt::Display, span: Span) -> Diagnostic {
    Diagnostic::error(format!("expected a module, {found}"))
        .with_label(Label::primary(span, "not a module"))
}

pub fn expected_named_ty(ty: impl core::fmt::Display, span: Span) -> Diagnostic {
    Diagnostic::error(format!("expected a named type, found type `{ty}`"))
        .with_label(Label::primary(span, "expected a named type"))
}

pub fn generic_expected_found(expected: &str, found: &str, span: Span) -> Diagnostic {
    Diagnostic::error(format!("expected {expected}, found {found}"))
        .with_label(Label::primary(span, format!("expected {expected} here")))
}

pub fn multiple_item_def_err(prev_span: Span, dup_name: Word) -> Diagnostic {
    Diagnostic::error(format!("item `{dup_name}` is defined multiple times"))
        .with_label(Label::primary(dup_name.span(), format!("`{dup_name}` defined here")))
        .with_label(Label::secondary(prev_span, format!("`{dup_name}` also defined here")))
        .with_note("you can only define items once in a module (except functions)")
}

pub fn multiple_fn_def_err(
    db: &Db,
    in_module: ModuleId,
    prev_span: Span,
    candidate: &FnCandidate,
) -> Diagnostic {
    Diagnostic::error(format!(
        "function `{}` is already defined in module `{}`",
        candidate.display(db),
        db[in_module].qpath
    ))
    .with_label(Label::primary(candidate.word.span(), "defined here"))
    .with_label(Label::secondary(prev_span, "also defined here"))
    .with_note("functions may be overloaded by their parameters' types and names")
}

pub fn ty_arg_mismatch(expected: usize, found: usize, span: Span) -> Diagnostic {
    Diagnostic::error(format!("expected {expected} type argument(s), but {found} were supplied"))
        .with_label(Label::primary(
            span,
            format!("expected {expected} type arguments, found {found}"),
        ))
}

pub fn adt_ty_arg_mismatch(db: &Db, adt_id: AdtId, targ_len: usize, span: Span) -> Diagnostic {
    let adt = &db[adt_id];
    let expected = adt.ty_params.len();

    Diagnostic::error(format!(
        "type `{}` expects {} type argument(s), but {} were supplied",
        adt.name, expected, targ_len
    ))
    .with_label(Label::primary(
        span,
        format!("expected {expected} type arguments, found {targ_len}"),
    ))
}

pub fn infinitely_sized_adt(adt: &Adt, field: &AdtField) -> Diagnostic {
    Diagnostic::error(format!("type `{}` is infinitely sized", adt.name))
        .with_label(Label::primary(adt.name.span(), "defined here"))
        .with_label(Label::secondary(
            field.name.span(),
            format!("field has type `{}` without indirection", adt.name),
        ))
}

pub fn invalid_bin_op(db: &Db, op: BinOp, ty: Ty, span: Span) -> Diagnostic {
    Diagnostic::error(format!("cannot use `{}` on `{}`", op, ty.display(db)))
        .with_label(Label::primary(span, format!("invalid use of `{op}`")))
}

pub fn invalid_un_op(db: &Db, op: UnOp, ty: Ty, span: Span) -> Diagnostic {
    Diagnostic::error(format!("cannot use `{}` on type `{}`", op, ty.display(db)))
        .with_label(Label::primary(span, format!("invalid use of `{op}`")))
}

// pub fn cyclic_def(ast: &Ast, origin_span: Span, err: CyclicItemErr) ->
// Diagnostic {     let reference_span = ast.find_item(err.causee).expect("item
// to exist").span();
//
//     Diagnostic::error("cycle detected while checking definition")
//         .with_label(Label::primary(origin_span, "definition here"))
//         .with_label(Label::secondary(reference_span, "cyclic reference
// here")) }

pub fn name_defined_twice(kind: &str, name: Word, prev_span: Span) -> Diagnostic {
    Diagnostic::error(format!("the name `{name}` is already used as a {kind} name"))
        .with_label(Label::primary(name.span(), format!("`{name}` used again here")))
        .with_label(Label::secondary(prev_span, format!("first use of `{name}`")))
}
