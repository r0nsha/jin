use compiler_core::db::Location;
use compiler_core::{
    db::DefKind,
    middle::{Mutability, TyParam, Vis},
    sym,
    ty::{ParamTy, Ty, TyKind},
    word::Word,
};
use ustr::ustr;

use crate::{TyAlias, Typeck};

pub(crate) fn define_all(cx: &mut Typeck) {
    let pairs = &[
        (sym::ty::I8, cx.db.types.i8),
        (sym::ty::I16, cx.db.types.i16),
        (sym::ty::I32, cx.db.types.i32),
        (sym::ty::I64, cx.db.types.i64),
        (sym::ty::INT, cx.db.types.int),
        (sym::ty::U8, cx.db.types.u8),
        (sym::ty::U16, cx.db.types.u16),
        (sym::ty::U32, cx.db.types.u32),
        (sym::ty::U64, cx.db.types.u64),
        (sym::ty::UINT, cx.db.types.uint),
        (sym::ty::F32, cx.db.types.f32),
        (sym::ty::F64, cx.db.types.f64),
        (sym::ty::STR, cx.db.types.str),
        (sym::ty::CHAR, cx.db.types.char),
        (sym::ty::BOOL, cx.db.types.bool),
        (sym::ty::UNIT, cx.db.types.unit),
        (sym::ty::NEVER, cx.db.types.never),
    ];

    let main_loc = cx.db.main_location();

    for &(name, ty) in pairs {
        let name = ustr(name);
        let id = cx.define().create_global(
            main_loc,
            Vis::Public,
            DefKind::BuiltinTy(ty),
            Word::new_unknown(name),
            Mutability::Imm,
        );
        cx.def_to_ty.insert(id, Ty::new(TyKind::Type(ty)));
        cx.global_env.builtin_tys.insert(name, id);
    }

    define_ptr(cx);
}

fn define_ptr(cx: &mut Typeck) {
    let main_loc = cx.db.main_location();

    let name = ustr(sym::ty::PTR);
    let word = Word::new_unknown(name);

    let id =
        cx.define().create_global(main_loc, Vis::Public, DefKind::TyAlias, word, Mutability::Imm);

    let tp = {
        let tp_word = Word::new_unknown(ustr("T"));
        let tp_ty = Ty::new(TyKind::Param(ParamTy { name: tp_word.name(), var: cx.fresh_var() }));
        let tp_id = cx.define().create_global(
            main_loc,
            Vis::Public,
            DefKind::BuiltinTy(tp_ty),
            tp_word,
            Mutability::Imm,
        );
        cx.def_to_ty.insert(id, Ty::new(TyKind::Type(tp_ty)));
        TyParam { id: tp_id, word: tp_word, ty: tp_ty }
    };

    cx.ty_aliases
        .insert(id, TyAlias { tyexpr: None, ty: Some(tp.ty.raw_ptr()), tparams: vec![tp] });
    cx.global_env.builtin_tys.insert(name, id);
}
