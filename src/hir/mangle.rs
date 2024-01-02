use std::iter;

use ustr::{ustr, Ustr};

use crate::{
    db::Db,
    hir,
    ty::{Ty, TyKind},
};

pub fn mangle_fn_name(db: &Db, fun: &hir::Fn) -> Ustr {
    let sig_str = {
        let params_str = fun.sig.params.iter().map(|param| {
            format!("{}_{}", param.pat, mangle_ty_name(db, param.ty))
        });

        params_str.collect::<Vec<String>>().join("_")
    };

    let def = &db[fun.def_id];

    let mangle_name = if sig_str.is_empty() {
        def.name.to_string()
    } else {
        format!("{}_{}", def.name, sig_str)
    };
    let qualified_name =
        def.qpath.clone().with_name(ustr(&mangle_name)).join_with("_");

    ustr(&qualified_name)
}

pub fn mangle_ty_name(db: &Db, ty: Ty) -> String {
    match ty.kind() {
        TyKind::Fn(f) => iter::once("fn".to_string())
            .chain(f.params.iter().map(|p| {
                let ty_name = mangle_ty_name(db, p.ty);
                if let Some(name) = p.name {
                    format!("{name}_{}", ty_name)
                } else {
                    ty_name
                }
            }))
            .chain(iter::once(mangle_ty_name(db, f.ret)))
            .collect::<Vec<String>>()
            .join("_"),
        TyKind::Adt(adt_id, _) => {
            // TODO: todo!("include ty args");
            db.adt_def(*adt_id).unwrap().qpath.join_with("_")
        }
        TyKind::Ref(inner, mutability) => {
            format!("ref_{}_{}", mutability, mangle_ty_name(db, *inner))
        }
        TyKind::RawPtr(pointee) => {
            format!("ptr_{}", mangle_ty_name(db, *pointee))
        }
        TyKind::Unit => "unit".to_string(),
        TyKind::Param(p) => p.name.to_string(),
        TyKind::Int(_)
        | TyKind::Uint(_)
        | TyKind::Float(_)
        | TyKind::Str
        | TyKind::Bool
        | TyKind::Never => ty.to_string(db),
        _ => unreachable!("unexpected ty {ty:?}"),
    }
}
