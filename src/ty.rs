pub mod coerce;
pub mod fold;
mod printer;

use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use derive_more::{From, Into};
use enum_as_inner::EnumAsInner;
use internment::Intern;
use ustr::Ustr;

use crate::{common::target::TargetMetrics, db::Db, ty::printer::TyPrinter};

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
            TyKind::Infer(InferTy::TyVar(tv)) => Some(*tv),
            _ => None,
        }
    }

    pub fn occurs_check(self, var: TyVar) -> Result<(), Self> {
        match self.kind() {
            TyKind::Fn(fun) => fun.ret.occurs_check(var).map_err(|_| self),
            TyKind::Infer(InferTy::TyVar(v)) => {
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
        let mut params = HashSet::new();
        self.collect_params_inner(&mut params);
        params.into_iter().collect()
    }

    fn collect_params_inner(self, params: &mut HashSet<ParamTy>) {
        match self.kind() {
            TyKind::Fn(fun) => {
                for p in &fun.params {
                    p.ty.collect_params_inner(params);
                }

                fun.ret.collect_params_inner(params);
            }
            TyKind::Param(p) => {
                params.insert(p.clone());
            }
            _ => (),
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
                fun.params.iter().any(|p| p.ty.walk_short_(f)) || fun.ret.walk_short_(f)
            }
            _ => f(self),
        }
    }

    pub fn walk(self, mut f: impl Fn(Ty) -> bool) -> bool {
        self.walk_(&mut f)
    }

    pub fn walk_(self, f: &mut impl Fn(Ty) -> bool) -> bool {
        match self.kind() {
            TyKind::Fn(fun) => fun.params.iter().all(|p| p.ty.walk_(f)) && fun.ret.walk_(f),
            _ => f(self),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum TyKind {
    // Composite types
    Fn(FnTy),
    RawPtr(Ty),

    // Primitive types
    Int(IntTy),
    Uint(UintTy),
    Str,
    Bool,
    Unit, // TODO: when we implement tuples, Unit should become Tuple([])
    Never,

    // Types related to phases inside or before the typeck pass
    Param(ParamTy),
    Infer(InferTy),
    Type,
    Unknown,
}

impl TyKind {
    pub const DEFAULT_INT: Self = Self::Int(IntTy::Int);

    pub fn display<'db>(&'db self, db: &'db Db) -> TyPrinter<'db> {
        TyPrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Db) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct TyVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct IntVar(u32);

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
            Self::Int => target_metrics.word_size,
        }
    }

    pub fn contains(self, value: i128) -> bool {
        // TODO: use target_metrics
        match self {
            IntTy::I8 => i8::try_from(value).is_ok(),
            IntTy::I16 => i16::try_from(value).is_ok(),
            IntTy::I32 => i32::try_from(value).is_ok(),
            IntTy::I64 => i64::try_from(value).is_ok(),
            IntTy::Int => isize::try_from(value).is_ok(),
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
            Self::Uint => target_metrics.word_size,
        }
    }

    pub fn contains(self, value: i128) -> bool {
        // TODO: use target_metrics
        match self {
            UintTy::U8 => u8::try_from(value).is_ok(),
            UintTy::U16 => u16::try_from(value).is_ok(),
            UintTy::U32 => u32::try_from(value).is_ok(),
            UintTy::U64 => u64::try_from(value).is_ok(),
            UintTy::Uint => usize::try_from(value).is_ok(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnTy {
    pub params: Vec<FnTyParam>,
    pub ret: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnTyParam {
    // TODO: This shouldn't be optional...
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

pub type Instantiation = HashMap<TyVar, Ty>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    TyVar(TyVar),
    IntVar(IntVar),
}

pub trait Typed {
    fn ty(&self) -> Ty;
    fn ty_mut(&mut self) -> &mut Ty;

    fn set_ty(&mut self, ty: Ty) {
        *self.ty_mut() = ty;
    }
}
