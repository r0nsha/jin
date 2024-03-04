use std::{hash::Hasher as _, iter};

use crate::{
    db::{Adt, Db},
    hir,
    ty::{Ty, TyKind},
};

pub fn mangle_fn_name(db: &Db, fun: &hir::Fn) -> String {
    let mut hasher = rustc_hash::FxHasher::default();
    let def = &db[fun.def_id];

    hasher.write(def.qpath.join().as_bytes());

    for param in &fun.sig.params {
        hasher.write(param.pat.to_string().as_bytes());
        hasher.write(mangle_ty_name(db, param.ty).as_bytes());
    }

    mangle_hyphens(format!("{}${:x}", def.name, hasher.finish()))
}

pub fn mangle_ty_name(db: &Db, ty: Ty) -> String {
    match ty.kind() {
        TyKind::Fn(f) => iter::once("fn".to_string())
            .chain(f.params.iter().map(|p| {
                let ty_name = mangle_ty_name(db, p.ty);
                if let Some(name) = p.name {
                    format!("{name}_{ty_name}")
                } else {
                    ty_name
                }
            }))
            .chain(iter::once(mangle_ty_name(db, f.ret)))
            .collect::<Vec<String>>()
            .join("_"),
        TyKind::Adt(adt_id, targs) => mangle_adt(db, &db[*adt_id], targs),
        TyKind::Slice(inner) => {
            format!("s{}", mangle_ty_name(db, *inner))
        }
        TyKind::Ref(inner, mutability) => {
            format!("r{}_{}", mutability, mangle_ty_name(db, *inner))
        }
        TyKind::RawPtr(pointee) => {
            format!("p{}", mangle_ty_name(db, *pointee))
        }
        TyKind::Unit => "unit".to_string(),
        TyKind::Param(p) => p.name.to_string(),
        TyKind::Int(_)
        | TyKind::Uint(_)
        | TyKind::Float(_)
        | TyKind::Str
        | TyKind::Char
        | TyKind::Bool
        | TyKind::Never => ty.to_string(db),
        _ => unreachable!("unexpected ty {ty:?}"),
    }
}

pub fn mangle_adt(db: &Db, adt: &Adt, targs: &[Ty]) -> String {
    let mut hasher = rustc_hash::FxHasher::default();
    let def = &db[adt.def_id];

    hasher.write(def.qpath.join().as_bytes());

    for &ty in targs {
        hasher.write(mangle_ty_name(db, ty).as_bytes());
    }

    mangle_hyphens(format!("{}${:x}", def.name, hasher.finish()))
}

#[inline]
fn mangle_hyphens(s: String) -> String {
    s.replace('-', "_")
}
