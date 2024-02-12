use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    db::{Db, Def, DefId, DefKind, ScopeInfo, ScopeLevel},
    middle::{Mutability, Vis},
    qpath::QPath,
    span::Span,
    sym,
    ty::{Ty, TyKind},
};

#[derive(Debug)]
pub struct BuiltinTys {
    inner: UstrMap<DefId>,
}

impl BuiltinTys {
    pub fn new(db: &mut Db, def_to_ty: &mut FxHashMap<DefId, Ty>) -> Self {
        let mut this = Self { inner: UstrMap::default() };

        this.define_all(
            db,
            def_to_ty,
            &[
                (sym::I8, db.types.i8),
                (sym::I16, db.types.i16),
                (sym::I32, db.types.i32),
                (sym::I64, db.types.i64),
                (sym::INT, db.types.int),
                (sym::U8, db.types.u8),
                (sym::U16, db.types.u16),
                (sym::U32, db.types.u32),
                (sym::U64, db.types.u64),
                (sym::UINT, db.types.uint),
                (sym::F32, db.types.f32),
                (sym::F64, db.types.f64),
                (sym::STR, db.types.str),
                (sym::BOOL, db.types.bool),
                (sym::NEVER, db.types.never),
            ],
        );

        this
    }

    fn define_all(
        &mut self,
        db: &mut Db,
        def_to_ty: &mut FxHashMap<DefId, Ty>,
        pairs: &[(&str, Ty)],
    ) {
        for (name, ty) in pairs {
            self.define(db, def_to_ty, name, *ty);
        }
    }

    fn define(
        &mut self,
        db: &mut Db,
        def_to_ty: &mut FxHashMap<DefId, Ty>,
        name: &str,
        ty: Ty,
    ) -> Option<DefId> {
        let name = ustr(name);
        let scope_info = ScopeInfo {
            module_id: db.main_module.unwrap(),
            level: ScopeLevel::Global,
            vis: Vis::Public,
        };

        let id = Def::alloc(
            db,
            QPath::from(name),
            scope_info,
            DefKind::BuiltinTy(ty),
            Mutability::Imm,
            Span::unknown(),
        );
        def_to_ty.insert(id, Ty::new(TyKind::Type(ty)));
        self.inner.insert(name, id)
    }

    pub fn get(&self, name: Ustr) -> Option<DefId> {
        self.inner.get(&name).copied()
    }
}
