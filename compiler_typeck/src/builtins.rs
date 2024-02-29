use compiler_core::{
    db::{Db, Def, DefId, DefKind, ScopeInfo, ScopeLevel},
    middle::{Mutability, Vis},
    qpath::QPath,
    span::Span,
    sym,
    ty::{Ty, TyKind},
};
use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

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
                (sym::ty::I8, db.types.i8),
                (sym::ty::I16, db.types.i16),
                (sym::ty::I32, db.types.i32),
                (sym::ty::I64, db.types.i64),
                (sym::ty::INT, db.types.int),
                (sym::ty::U8, db.types.u8),
                (sym::ty::U16, db.types.u16),
                (sym::ty::U32, db.types.u32),
                (sym::ty::U64, db.types.u64),
                (sym::ty::UINT, db.types.uint),
                (sym::ty::F32, db.types.f32),
                (sym::ty::F64, db.types.f64),
                (sym::ty::STR, db.types.str),
                (sym::ty::CHAR, db.types.char),
                (sym::ty::BOOL, db.types.bool),
                (sym::ty::NEVER, db.types.never),
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
