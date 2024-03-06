pub mod coerce;
pub mod fold;
pub mod printer;
pub mod size;

use std::ops::{self, Deref};

use bitflags::bitflags;
use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::Ustr;

use crate::middle::TyParam;
use crate::{
    db::{AdtId, AdtKind, Db, ModuleId, StructKind, UnionDef, UnionKind},
    middle::{CallConv, Mutability, Pat, Vis},
    span::{Span, Spanned as _},
    ty::{
        fold::TyFolder,
        printer::{FnTyPrinter, TyPrinter},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(Intern<TyKind>);

impl Ty {
    #[inline]
    pub fn new(kind: TyKind) -> Self {
        Self(Intern::new(kind))
    }

    #[inline]
    pub fn from_ref(kind: &TyKind) -> Self {
        Self(Intern::from_ref(kind))
    }

    #[inline]
    pub fn kind(&self) -> &TyKind {
        self.as_ref()
    }

    #[inline]
    pub fn as_tyvar(self) -> Option<TyVar> {
        match self.kind() {
            TyKind::Infer(InferTy::Ty(tv)) => Some(*tv),
            _ => None,
        }
    }

    pub fn occurs_check(self, var: TyVar) -> Result<(), Self> {
        match self.kind() {
            TyKind::Fn(fun) => {
                for p in &fun.params {
                    p.ty.occurs_check(var).map_err(|_| self)?;
                }

                fun.ret.occurs_check(var).map_err(|_| self)
            }
            TyKind::Slice(inner) | TyKind::Ref(inner, _) | TyKind::RawPtr(inner) => {
                inner.occurs_check(var).map_err(|_| self)
            }
            TyKind::Param(ParamTy { var: v, .. }) | TyKind::Infer(InferTy::Ty(v)) => {
                if *v == var {
                    Err(self)
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }

    pub fn collect_params(self) -> Vec<ParamTy> {
        let mut params = FxHashSet::default();
        self.collect_params_into(&mut params);
        params.into_iter().collect()
    }

    fn collect_params_into(self, collect_into: &mut FxHashSet<ParamTy>) {
        self.walk(|ty| {
            if let TyKind::Param(p) = ty.kind() {
                collect_into.insert(p.clone());
            }
        });
    }

    pub fn is_polymorphic(self) -> bool {
        self.walk_short(|ty| matches!(ty.kind(), TyKind::Param(_)))
    }

    pub fn is_diverging(self) -> bool {
        self.walk_short(|ty| matches!(ty.kind(), TyKind::Never))
    }

    pub fn walk_short(self, mut f: impl FnMut(Ty) -> bool) -> bool {
        self.walk_short_(&mut f)
    }

    fn walk_short_(self, f: &mut impl FnMut(Ty) -> bool) -> bool {
        f(self)
            || match self.kind() {
                TyKind::Fn(fun) => {
                    fun.params.iter().any(|p| p.ty.walk_short_(f)) || fun.ret.walk_short_(f)
                }
                TyKind::Slice(inner)
                | TyKind::Ref(inner, _)
                | TyKind::RawPtr(inner)
                | TyKind::Type(inner) => inner.walk_short_(f),
                TyKind::Adt(_, targs) => targs.iter().any(|ty| ty.walk_short_(f)),
                TyKind::Int(_)
                | TyKind::Uint(_)
                | TyKind::Float(_)
                | TyKind::Str
                | TyKind::Char
                | TyKind::Bool
                | TyKind::Unit
                | TyKind::Never
                | TyKind::Param(_)
                | TyKind::Infer(_)
                | TyKind::Module(_)
                | TyKind::Unknown => false,
            }
    }

    pub fn walk(self, mut f: impl FnMut(Ty)) {
        self.walk_(&mut f);
    }

    pub fn walk_(self, f: &mut impl FnMut(Ty)) {
        f(self);

        match self.kind() {
            TyKind::Fn(fun) => {
                fun.params.iter().for_each(|p| p.ty.walk_(f));
                fun.ret.walk_(f);
            }
            TyKind::Slice(inner)
            | TyKind::Ref(inner, _)
            | TyKind::RawPtr(inner)
            | TyKind::Type(inner) => inner.walk_(f),
            TyKind::Adt(_, targs) => targs.iter().for_each(|ty| ty.walk_(f)),
            TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Char
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Module(_)
            | TyKind::Unknown => (),
        }
    }

    pub fn has_more_private_ty(self, db: &Db, vis: Vis) -> Option<Ty> {
        let mut result = None;

        self.walk_short(|ty| match ty.kind() {
            TyKind::Adt(adt_id, _) if db[db[*adt_id].def_id].scope.vis < vis => {
                result = Some(ty);
                true
            }
            _ => false,
        });

        result
    }

    pub fn infinitely_contains_adt(self, db: &Db, adt_id: AdtId) -> bool {
        match self.kind() {
            TyKind::Fn(fun) => {
                fun.params.iter().any(|p| p.ty.infinitely_contains_adt(db, adt_id))
                    || fun.ret.infinitely_contains_adt(db, adt_id)
            }
            TyKind::Slice(inner)
            | TyKind::Ref(inner, _)
            | TyKind::RawPtr(inner)
            | TyKind::Type(inner) => inner.infinitely_contains_adt(db, adt_id),
            TyKind::Adt(id2, _) if *id2 == adt_id => true,
            TyKind::Adt(id2, targs) => {
                if db[*id2].is_value_type() {
                    db[*id2].fields(db).any(|f| f.ty.infinitely_contains_adt(db, adt_id))
                        || targs.iter().any(|ty| ty.infinitely_contains_adt(db, adt_id))
                } else {
                    false
                }
            }
            TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Char
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Module(_)
            | TyKind::Unknown => false,
        }
    }

    pub fn raw_ptr(self) -> Ty {
        Ty::new(TyKind::RawPtr(self))
    }

    pub fn slice_elem(self) -> Option<Ty> {
        match self.kind() {
            TyKind::Slice(elem) => Some(*elem),
            TyKind::Str => Some(Ty::new(TyKind::Uint(UintTy::U8))),
            _ => None,
        }
    }

    pub fn create_ref(self, mutability: Mutability) -> Ty {
        match self.kind() {
            TyKind::Ref(ty, _) => Ty::new(TyKind::Ref(*ty, mutability)),
            _ => Ty::new(TyKind::Ref(self, mutability)),
        }
    }

    pub fn auto_deref(self) -> Ty {
        match self.kind() {
            TyKind::Ref(inner, _) => inner.auto_deref(),
            _ => self,
        }
    }
}

impl Deref for Ty {
    type Target = TyKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<TyKind> for Ty {
    fn as_ref(&self) -> &TyKind {
        &self.0
    }
}

impl From<TyKind> for Ty {
    fn from(value: TyKind) -> Self {
        Self::new(value)
    }
}

impl From<&TyKind> for Ty {
    fn from(value: &TyKind) -> Self {
        Self::from_ref(value)
    }
}

impl From<&TyKind> for TyKind {
    fn from(value: &TyKind) -> Self {
        value.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    // Composite types
    Fn(FnTy),
    Adt(AdtId, PolyTyArgs),
    Slice(Ty),
    Ref(Ty, Mutability),
    RawPtr(Ty),

    // Primitive types
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Str,
    Char,
    Bool,
    Unit, // TODO: when we implement tuples, Unit should become Tuple([])
    Never,

    // Types related to phases inside or before the typeck pass
    Param(ParamTy),
    Infer(InferTy),
    Type(Ty),
    Module(ModuleId),
    Unknown,
}

pub type PolyTyArgs = Vec<Ty>;

impl TyKind {
    pub const DEFAULT_INT: Self = Self::Int(IntTy::Int);
    pub const DEFAULT_FLOAT: Self = Self::Float(FloatTy::F64);

    pub fn is_any_int(&self) -> bool {
        matches!(self, TyKind::Int(_) | TyKind::Uint(_) | TyKind::Infer(InferTy::Int(_)))
    }

    pub fn is_any_float(&self) -> bool {
        matches!(self, TyKind::Float(_) | TyKind::Infer(InferTy::Float(_)))
    }

    pub fn min(&self) -> i128 {
        match self {
            TyKind::Int(i) => i128::from(i.min()),
            TyKind::Uint(u) => i128::from(u.min()),
            _ => panic!("ty {self:?} doesn't have a min"),
        }
    }

    pub fn max(&self) -> i128 {
        match self {
            TyKind::Int(i) => i128::from(i.max()),
            TyKind::Uint(u) => i128::from(u.max()),
            _ => panic!("ty {self:?} doesn't have a min"),
        }
    }

    pub fn display<'db>(&'db self, db: &'db Db) -> TyPrinter {
        TyPrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Db) -> String {
        self.display(db).to_string()
    }

    pub fn print(&self, db: &Db) {
        println!("{}", self.display(db))
    }

    /// Returns `true` if the ty kind is [`Int`].
    ///
    /// [`Int`]: TyKind::Int
    #[must_use]
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    /// Returns `true` if the ty kind is [`Uint`].
    ///
    /// [`Uint`]: TyKind::Uint
    #[must_use]
    pub fn is_uint(&self) -> bool {
        matches!(self, Self::Uint(..))
    }

    /// Returns `true` if the ty kind is [`Bool`].
    ///
    /// [`Bool`]: TyKind::Bool
    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    /// Returns `true` if the ty kind is [`Unit`].
    ///
    /// [`Unit`]: TyKind::Unit
    #[must_use]
    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    #[must_use]
    pub fn as_fn(&self) -> Option<&FnTy> {
        if let Self::Fn(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_adt(&self) -> Option<(AdtId, &PolyTyArgs)> {
        if let Self::Adt(id, targs) = self {
            Some((*id, targs))
        } else {
            None
        }
    }

    /// Returns `true` if the ty kind is [`Never`].
    ///
    /// [`Never`]: TyKind::Never
    #[must_use]
    pub fn is_never(&self) -> bool {
        matches!(self, Self::Never)
    }

    /// Returns `true` if the ty kind is [`Module`].
    ///
    /// [`Module`]: TyKind::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module(..))
    }

    /// Returns `true` if the ty kind is [`Type`].
    ///
    /// [`Type`]: TyKind::Type
    #[must_use]
    pub fn is_type(&self) -> bool {
        matches!(self, Self::Type(..))
    }

    /// Returns `true` if the ty kind is [`Ref`].
    ///
    /// [`Ref`]: TyKind::Ref
    #[must_use]
    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Ref(..))
    }

    /// Returns `true` if the ty kind is an immutable [`Ref`].
    ///
    /// [`Ref`]: TyKind::Ref
    #[must_use]
    pub fn is_imm_ref(&self) -> bool {
        matches!(self, Self::Ref(_, Mutability::Imm))
    }

    /// Returns `true` if the ty kind is a mutable [`Ref`].
    ///
    /// [`Ref`]: TyKind::Ref
    #[must_use]
    pub fn is_mut_ref(&self) -> bool {
        matches!(self, Self::Ref(_, Mutability::Mut))
    }

    /// Returns `true` if a reference can be taken to this type
    #[must_use]
    pub fn can_create_ref(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, _) if db[*adt_id].is_ref() => true,
            Self::Slice(..) | Self::Param(_) | Self::Ref(..) => true,
            _ => false,
        }
    }

    /// Returns `true` if this type has move semantics
    #[must_use]
    pub fn needs_free(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, targs) => {
                let adt = &db[*adt_id];
                match &adt.kind {
                    AdtKind::Struct(s) => match s.kind {
                        StructKind::Ref => true,
                        StructKind::Value => {
                            let instantiation = adt.instantiation(targs);
                            let mut folder = instantiation.folder();
                            s.fields.iter().any(|f| {
                                let ty = folder.fold(f.ty);
                                ty.is_ref() || ty.needs_free(db)
                            })
                        }
                    },
                    AdtKind::Union(u) => match u.kind {
                        UnionKind::Ref => true,
                        UnionKind::Value => {
                            let instantiation = adt.instantiation(targs);
                            let mut folder = instantiation.folder();
                            u.variants(db).any(|v| {
                                v.fields.iter().any(|f| {
                                    let ty = folder.fold(f.ty);
                                    ty.is_ref() || ty.needs_free(db)
                                })
                            })
                        }
                    },
                }
            }
            Self::Slice(_) | Self::Str => true,
            _ => false,
        }
    }

    /// Returns `true` if this type has move semantics
    #[must_use]
    pub fn is_move(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, targs) => {
                let adt = &db[*adt_id];
                match &adt.kind {
                    AdtKind::Struct(s) => match s.kind {
                        StructKind::Ref => true,
                        StructKind::Value => {
                            let instantiation = adt.instantiation(targs);
                            let mut folder = instantiation.folder();
                            s.fields.iter().any(|f| folder.fold(f.ty).is_move(db))
                        }
                    },
                    AdtKind::Union(u) => match u.kind {
                        UnionKind::Ref => true,
                        UnionKind::Value => {
                            let instantiation = adt.instantiation(targs);
                            let mut folder = instantiation.folder();
                            u.variants(db)
                                .any(|v| v.fields.iter().any(|f| folder.fold(f.ty).is_move(db)))
                        }
                    },
                }
            }
            Self::Slice(_) | Self::Str | Self::Param(_) => true,
            _ => false,
        }
    }

    /// Returns `true` if this type is reference counted
    #[must_use]
    pub fn is_rc(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, _) => db[*adt_id].is_ref(),
            Self::Slice(_) | Self::Str => true,
            _ => false,
        }
    }

    /// Returns `true` if this type is a value struct
    #[must_use]
    pub fn is_value_struct(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, _) => db[*adt_id].is_value_struct(),
            _ => false,
        }
    }

    #[must_use]
    pub fn as_param(&self) -> Option<&ParamTy> {
        if let Self::Param(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the ty kind is [`RawPtr`].
    ///
    /// [`RawPtr`]: TyKind::RawPtr
    #[must_use]
    pub fn is_raw_ptr(&self) -> bool {
        matches!(self, Self::RawPtr(..))
    }

    pub fn as_union<'db>(&'db self, db: &'db Db) -> Option<&UnionDef> {
        let TyKind::Adt(adt_id, _) = self else {
            return None;
        };
        db[*adt_id].kind.as_union()
    }

    pub fn is_slice_like(&self) -> bool {
        matches!(self, TyKind::Slice(..) | TyKind::Str)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(u32);

impl From<u32> for TyVar {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<TyVar> for u32 {
    fn from(value: TyVar) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntVar(u32);

impl From<u32> for IntVar {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<IntVar> for u32 {
    fn from(value: IntVar) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatVar(u32);

impl From<u32> for FloatVar {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<FloatVar> for u32 {
    fn from(value: FloatVar) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntVarValue {
    Int(IntTy),
    Uint(UintTy),
}

impl From<IntVarValue> for TyKind {
    fn from(value: IntVarValue) -> Self {
        match value {
            IntVarValue::Int(ty) => Self::Int(ty),
            IntVarValue::Uint(ty) => Self::Uint(ty),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    Int,
}

impl IntTy {
    pub fn contains(self, value: i128) -> bool {
        // TODO: use target_metrics for Int
        match self {
            Self::I8 => i8::try_from(value).is_ok(),
            Self::I16 => i16::try_from(value).is_ok(),
            Self::I32 => i32::try_from(value).is_ok(),
            Self::I64 => i64::try_from(value).is_ok(),
            Self::Int => isize::try_from(value).is_ok(),
        }
    }

    pub fn min(self) -> i64 {
        // TODO: use target_metrics for Int
        match self {
            Self::I8 => i64::from(i8::MIN),
            Self::I16 => i64::from(i16::MIN),
            Self::I32 => i64::from(i32::MIN),
            Self::I64 => i64::MIN,
            Self::Int => isize::MIN as _,
        }
    }

    pub fn max(self) -> i64 {
        // TODO: use target_metrics for Int
        match self {
            Self::I8 => i64::from(i8::MAX),
            Self::I16 => i64::from(i16::MAX),
            Self::I32 => i64::from(i32::MAX),
            Self::I64 => i64::MAX,
            Self::Int => isize::MAX as _,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    Uint,
}

impl UintTy {
    pub fn contains(self, value: i128) -> bool {
        // TODO: use target_metrics
        match self {
            Self::U8 => u8::try_from(value).is_ok(),
            Self::U16 => u16::try_from(value).is_ok(),
            Self::U32 => u32::try_from(value).is_ok(),
            Self::U64 => u64::try_from(value).is_ok(),
            Self::Uint => usize::try_from(value).is_ok(),
        }
    }

    pub fn min(self) -> u64 {
        // TODO: use target_metrics for Uint
        match self {
            Self::U8 => u64::from(u8::MIN),
            Self::U16 => u64::from(u16::MIN),
            Self::U32 => u64::from(u32::MIN),
            Self::U64 => u64::MIN,
            Self::Uint => usize::MIN as _,
        }
    }

    pub fn max(self) -> u64 {
        // TODO: use target_metrics for Uint
        match self {
            Self::U8 => u64::from(u8::MAX),
            Self::U16 => u64::from(u16::MAX),
            Self::U32 => u64::from(u32::MAX),
            Self::U64 => u64::MAX,
            Self::Uint => usize::MAX as _,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

impl From<FloatTy> for TyKind {
    fn from(value: FloatTy) -> Self {
        Self::Float(value)
    }
}

impl FloatTy {
    pub fn contains(self, value: f64) -> bool {
        match self {
            Self::F32 => value >= f64::from(f32::MIN) && value <= f64::from(f32::MAX),
            Self::F64 => true,
        }
    }

    pub fn min(self) -> f64 {
        match self {
            Self::F32 => f64::from(f32::MIN),
            Self::F64 => f64::MIN,
        }
    }

    pub fn max(self) -> f64 {
        match self {
            Self::F32 => f64::from(f32::MAX),
            Self::F64 => f64::MAX,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnTy {
    pub params: Vec<FnTyParam>,
    pub ret: Ty,
    pub callconv: CallConv,
    pub flags: FnTyFlags,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct FnTyFlags: u32 {
        const EXTERN = 0b0000_0001;
        const C_VARIADIC = 0b0000_0010;
    }
}

impl FnTy {
    #[inline]
    pub fn is_extern(&self) -> bool {
        self.flags.contains(FnTyFlags::EXTERN)
    }

    #[inline]
    pub fn is_c_variadic(&self) -> bool {
        self.flags.contains(FnTyFlags::C_VARIADIC)
    }

    pub fn collect_params(&self) -> Vec<ParamTy> {
        let mut params = FxHashSet::default();
        self.collect_params_into(&mut params);
        params.into_iter().collect()
    }

    fn collect_params_into(&self, params: &mut FxHashSet<ParamTy>) {
        for p in &self.params {
            p.ty.collect_params_into(params);
        }

        self.ret.collect_params_into(params);
    }

    pub fn display<'db>(&'db self, db: &'db Db, name: Option<Ustr>) -> FnTyPrinter {
        FnTyPrinter {
            db,
            name,
            params: &self.params,
            ret: Some(self.ret),
            callconv: self.callconv,
            flags: self.flags,
        }
    }

    pub fn to_string(&self, db: &Db, name: Option<Ustr>) -> String {
        self.display(db, name).to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnTyParam {
    pub name: Option<Ustr>,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamTy {
    pub name: Ustr,
    pub var: TyVar,
}

impl ParamTy {
    pub fn new(name: Ustr, var: TyVar) -> Self {
        Self { name, var }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    Ty(TyVar),
    Int(IntVar),
    Float(FloatVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Instantiation(FxHashMap<TyVar, Ty>);

impl Instantiation {
    pub fn get(&self, var: TyVar) -> Option<Ty> {
        self.0.get(&var).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn tys(&self) -> impl Iterator<Item = Ty> + '_ {
        self.0.values().copied()
    }

    pub fn folder(&self) -> ParamFolder {
        ParamFolder::new(self)
    }

    pub fn fold(&self, ty: Ty) -> Ty {
        self.folder().fold(ty)
    }

    pub fn subst(&mut self, s: &mut impl SubstTy, span: Span) {
        for ty in self.0.values_mut() {
            *ty = s.subst_ty(*ty, span);
        }
    }
}

impl ops::Index<TyVar> for Instantiation {
    type Output = Ty;

    #[inline]
    fn index(&self, index: TyVar) -> &Self::Output {
        &self.0[&index]
    }
}

impl ops::IndexMut<TyVar> for Instantiation {
    #[track_caller]
    #[inline]
    fn index_mut(&mut self, index: TyVar) -> &mut Self::Output {
        self.0.get_mut(&index).unwrap()
    }
}

impl FromIterator<(TyVar, Ty)> for Instantiation {
    fn from_iter<T: IntoIterator<Item = (TyVar, Ty)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a, 'b> From<(&'a [TyParam], &'b [Ty])> for Instantiation {
    fn from((tparams, targs): (&'a [TyParam], &'b [Ty])) -> Self {
        debug_assert!(targs.len() == tparams.len());
        tparams.iter().zip(targs).map(|(tp, ty)| (tp.ty.as_param().unwrap().var, *ty)).collect()
    }
}

impl<'a, 'b> From<(&'a [ParamTy], &'b [Ty])> for Instantiation {
    fn from((tparams, targs): (&'a [ParamTy], &'b [Ty])) -> Self {
        debug_assert!(targs.len() == tparams.len());
        tparams.iter().zip(targs).map(|(tp, ty)| (tp.var, *ty)).collect()
    }
}

pub struct ParamFolder<'a> {
    pub instantiation: &'a Instantiation,
}

impl<'a> ParamFolder<'a> {
    pub fn new(instantiation: &'a Instantiation) -> Self {
        Self { instantiation }
    }
}

impl SubstTy for ParamFolder<'_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }
}

impl TyFolder for ParamFolder<'_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => match self.instantiation.get(p.var) {
                Some(ty) => ty,
                None => {
                    // NOTE: It currently makes sense to not instantiate params that are part of
                    // the currently typechecked function.
                    panic!(
                        "type param `{:?}` not part of instantation: {:?}",
                        p, self.instantiation
                    )
                }
            },
            _ => self.super_fold(ty),
        }
    }
}

pub trait SubstTy {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty;
}

pub trait Subst<S: SubstTy> {
    fn subst(&mut self, s: &mut S);
}

impl<S: SubstTy> Subst<S> for Pat {
    fn subst(&mut self, s: &mut S) {
        match self {
            Pat::Name(name) => {
                name.ty = s.subst_ty(name.ty, name.span());
            }
            Pat::Discard(_) => (),
        }
    }
}

impl UnifyKey for TyVar {
    type Value = Option<Ty>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

impl EqUnifyValue for Ty {}

impl UnifyKey for IntVar {
    type Value = Option<IntVarValue>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "IntVar"
    }
}

impl EqUnifyValue for IntVarValue {}

impl UnifyKey for FloatVar {
    type Value = Option<FloatTy>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "FloatVar"
    }
}

impl EqUnifyValue for FloatTy {}
