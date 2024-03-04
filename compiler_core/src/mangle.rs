use std::fmt::Write;
use std::{hash::Hasher as _, iter};

use crate::{
    db::{Adt, Db},
    hir,
    ty::{Ty, TyKind},
};

pub fn fn_name(db: &Db, fun: &hir::Fn) -> String {
    let mut hasher = rustc_hash::FxHasher::default();
    let def = &db[fun.def_id];

    hasher.write(def.qpath.join().as_bytes());

    for param in &fun.sig.params {
        hasher.write(param.pat.to_string().as_bytes());
        hasher.write(ty_name(db, param.ty).as_bytes());
    }

    ident(&format!("{}${:x}", def.name, hasher.finish()))
}

pub fn ty_name(db: &Db, t: Ty) -> String {
    match t.kind() {
        TyKind::Fn(f) => iter::once("fn".to_string())
            .chain(f.params.iter().map(|p| {
                let ty_name = ty_name(db, p.ty);
                if let Some(name) = p.name {
                    format!("{name}_{ty_name}")
                } else {
                    ty_name
                }
            }))
            .chain(iter::once(ty_name(db, f.ret)))
            .collect::<Vec<String>>()
            .join("_"),
        TyKind::Adt(adt_id, targs) => adt_name(db, &db[*adt_id], targs),
        TyKind::Slice(inner) => {
            format!("s{}", ty_name(db, *inner))
        }
        TyKind::Ref(inner, mutability) => {
            format!("r{}_{}", mutability, ty_name(db, *inner))
        }
        TyKind::RawPtr(pointee) => {
            format!("p{}", ty_name(db, *pointee))
        }
        TyKind::Unit => "unit".to_string(),
        TyKind::Param(p) => p.name.to_string(),
        TyKind::Int(_)
        | TyKind::Uint(_)
        | TyKind::Float(_)
        | TyKind::Str
        | TyKind::Char
        | TyKind::Bool
        | TyKind::Never => t.to_string(db),
        _ => unreachable!("unexpected ty {t:?}"),
    }
}

pub fn adt_name(db: &Db, adt: &Adt, targs: &[Ty]) -> String {
    let mut hasher = rustc_hash::FxHasher::default();
    let def = &db[adt.def_id];

    hasher.write(def.qpath.join().as_bytes());

    for &ty in targs {
        hasher.write(ty_name(db, ty).as_bytes());
    }

    ident(&format!("{}${:x}", def.name, hasher.finish()))
}

pub fn ident(s: &str) -> String {
    let mut new = String::with_capacity(s.len());

    for ch in s.chars() {
        match ch {
            '-' => new.push('_'),
            ch if ch.is_alphanumeric() => new.push(ch),
            ch => write!(new, "${}", ch as i32).unwrap(),
        }
    }

    new
}
