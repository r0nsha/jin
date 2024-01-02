pub mod coerce;
pub mod fold;
pub mod printer;

use std::ops::{self, Deref};

use derive_more::{From, Into};
use internment::Intern;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::Ustr;

use crate::{
    db::{AdtId, Db, ModuleId},
    middle::Mutability,
    subst::ParamFolder,
    target::TargetMetrics,
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
            TyKind::RawPtr(pointee) => {
                pointee.occurs_check(var).map_err(|_| self)
            }
            TyKind::Param(ParamTy { var: v, .. })
            | TyKind::Infer(InferTy::Ty(v)) => {
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
        self.collect_params_inner(&mut params);
        params.into_iter().collect()
    }

    fn collect_params_inner(self, params: &mut FxHashSet<ParamTy>) {
        match self.kind() {
            TyKind::Fn(fun) => {
                for p in &fun.params {
                    p.ty.collect_params_inner(params);
                }

                fun.ret.collect_params_inner(params);
            }
            TyKind::Ref(inner, _) | TyKind::RawPtr(inner) => {
                inner.collect_params_inner(params);
            }
            TyKind::Param(p) => {
                params.insert(p.clone());
            }
            TyKind::Adt(..)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Infer(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => (),
        }
    }

    pub fn is_polymorphic(self) -> bool {
        self.walk_short(|ty| matches!(ty.kind(), TyKind::Param(_)))
    }

    pub fn is_diverging(self) -> bool {
        self.walk_short(|ty| matches!(ty.kind(), TyKind::Never))
    }

    pub fn walk_short(self, mut f: impl Fn(Ty) -> bool) -> bool {
        self.walk_short_(&mut f)
    }

    fn walk_short_(self, f: &mut impl Fn(Ty) -> bool) -> bool {
        match self.kind() {
            TyKind::Fn(fun) => {
                fun.params.iter().any(|p| p.ty.walk_short_(f))
                    || fun.ret.walk_short_(f)
            }
            TyKind::RawPtr(pointee) => pointee.walk_short_(f),
            _ => f(self),
        }
    }

    pub fn walk(self, mut f: impl Fn(Ty) -> bool) -> bool {
        self.walk_(&mut f)
    }

    pub fn walk_(self, f: &mut impl Fn(Ty) -> bool) -> bool {
        match self.kind() {
            TyKind::Fn(fun) => {
                fun.params.iter().all(|p| p.ty.walk_(f)) && fun.ret.walk_(f)
            }
            _ => f(self),
        }
    }

    pub fn raw_ptr(self) -> Ty {
        Ty::new(TyKind::RawPtr(self))
    }

    pub fn create_ref(self, mutability: Mutability) -> Ty {
        match self.kind() {
            TyKind::Ref(ty, _) => Ty::new(TyKind::Ref(*ty, mutability)),
            _ => Ty::new(TyKind::Ref(self, mutability)),
        }
    }

    pub(crate) fn auto_deref(self) -> Ty {
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
    Ref(Ty, Mutability),
    RawPtr(Ty),

    // Primitive types
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Str,
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
    pub const DEFAULT_FLOAT: Self = Self::Float(FloatTy::F32);

    pub fn is_any_int(&self) -> bool {
        matches!(
            self,
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Infer(InferTy::Int(_))
        )
    }

    pub fn is_any_float(&self) -> bool {
        matches!(self, TyKind::Float(_) | TyKind::Infer(InferTy::Float(_)))
    }

    pub fn size(&self, target_metrics: &TargetMetrics) -> usize {
        match self {
            TyKind::Int(x) => x.size(target_metrics),
            TyKind::Uint(x) => x.size(target_metrics),
            TyKind::Float(x) => x.size(),
            _ => panic!("cant find size of {self:?}"),
        }
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

    /// Returns `true` if the ty kind is [`Int`].
    ///
    /// [`Int`]: TyKind::Int
    #[must_use]
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(..))
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

    /// Returns `true` if a reference can be taken to this type
    #[must_use]
    pub fn can_create_ref(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, _) if db[*adt_id].is_ref() => true,
            Self::Param(_) | Self::Ref(..) => true,
            _ => false,
        }
    }

    /// Returns `true` if this type has move semantics
    #[must_use]
    pub fn is_move(&self, db: &Db) -> bool {
        match self {
            Self::Adt(adt_id, _) => db[*adt_id].is_ref(),
            Self::Param(_) => true,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct TyVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct IntVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct FloatVar(u32);

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
    pub fn size(self, target_metrics: &TargetMetrics) -> usize {
        match self {
            Self::I8 => 8,
            Self::I16 => 16,
            Self::I32 => 32,
            Self::I64 => 64,
            Self::Int => target_metrics.word_bit_size(),
        }
    }

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
    pub fn size(self, target_metrics: &TargetMetrics) -> usize {
        match self {
            Self::U8 => 8,
            Self::U16 => 16,
            Self::U32 => 32,
            Self::U64 => 64,
            Self::Uint => target_metrics.word_bit_size(),
        }
    }

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
    pub fn size(self) -> usize {
        match self {
            Self::F32 => 32,
            Self::F64 => 64,
        }
    }

    pub fn contains(self, value: f64) -> bool {
        match self {
            Self::F32 => {
                value >= f64::from(f32::MIN) && value <= f64::from(f32::MAX)
            }
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
    pub is_c_variadic: bool,
}

impl FnTy {
    pub fn collect_params(&self) -> Vec<ParamTy> {
        let mut params = FxHashSet::default();
        self.collect_params_inner(&mut params);
        params.into_iter().collect()
    }

    fn collect_params_inner(&self, params: &mut FxHashSet<ParamTy>) {
        for p in &self.params {
            p.ty.collect_params_inner(params);
        }

        self.ret.collect_params_inner(params);
    }

    pub fn display<'db>(
        &'db self,
        db: &'db Db,
        name: Option<Ustr>,
    ) -> FnTyPrinter {
        FnTyPrinter { db, name, params: &self.params, ret: Some(self.ret) }
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

    pub fn tys_mut(&mut self) -> impl Iterator<Item = &mut Ty> + '_ {
        self.0.values_mut()
    }

    pub fn fold(&self, ty: Ty) -> Ty {
        ParamFolder { instantiation: self }.fold(ty)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    Ty(TyVar),
    Int(IntVar),
    Float(FloatVar),
}

pub trait Typed {
    fn ty(&self) -> Ty;
    fn ty_mut(&mut self) -> &mut Ty;

    fn set_ty(&mut self, ty: Ty) {
        *self.ty_mut() = ty;
    }
}
