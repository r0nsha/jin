mod printer;

use std::ops::Deref;

use derive_more::{From, Into};
use enum_as_inner::EnumAsInner;
use internment::Intern;
use lazy_static::lazy_static;
use ustr::Ustr;

use crate::{db::Db, span::Span, ty::printer::TypePrinter};

lazy_static! {
    static ref TYPE_UNKNOWN: Type = Type::new(TypeKind::Unknown);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type(Intern<TypeKind>);

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self(Intern::new(kind))
    }

    pub fn from_ref(kind: &TypeKind) -> Self {
        Self(Intern::from_ref(kind))
    }

    pub fn unknown() -> Self {
        *TYPE_UNKNOWN
    }

    pub fn occurs_check(self, var: TypeVar) -> Result<(), Self> {
        match self.as_ref() {
            TypeKind::Function(fun) => fun.ret.occurs_check(var).map_err(|_| self),
            TypeKind::Infer(InferType::TypeVar(v), _) => {
                if *v == var {
                    Err(self)
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}

impl Deref for Type {
    type Target = TypeKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<TypeKind> for Type {
    fn as_ref(&self) -> &TypeKind {
        &self.0
    }
}

impl From<TypeKind> for Type {
    fn from(value: TypeKind) -> Self {
        Self::new(value)
    }
}

impl From<&TypeKind> for Type {
    fn from(value: &TypeKind) -> Self {
        Self::from_ref(value)
    }
}

impl From<&TypeKind> for TypeKind {
    fn from(value: &TypeKind) -> Self {
        value.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum TypeKind {
    Function(FunctionType),
    Int(IntType, Span),
    Bool(Span),
    // TODO: when we implement tuples, Unit should become Tuple([])
    Unit(Span),
    Never(Span),
    Infer(InferType, Span),
    Unknown,
}

impl TypeKind {
    pub fn default_int(span: Span) -> Self {
        Self::Int(IntType::Int, span)
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Function(FunctionType { span, .. })
            | Self::Int(_, span)
            | Self::Bool(span)
            | Self::Unit(span)
            | Self::Never(span)
            | Self::Infer(_, span) => *span,
            Self::Unknown => panic!(),
        }
    }

    pub fn display<'db>(&'db self, db: &'db Db) -> TypePrinter<'db> {
        TypePrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Db) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct TypeVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct IntVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntVarValue {
    Int(IntType, Span),
}

impl From<IntVarValue> for TypeKind {
    fn from(value: IntVarValue) -> Self {
        match value {
            IntVarValue::Int(ty, span) => Self::Int(ty, span),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntType {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub ret: Type,
    pub params: Vec<FunctionTypeParam>,
    pub span: Span,
}

impl FunctionType {
    pub fn param(&self, name: Ustr) -> Option<&FunctionTypeParam> {
        self.params.iter().find(|p| p.name == Some(name))
    }

    pub fn param_at(&self, index: usize) -> Option<&FunctionTypeParam> {
        self.params.get(index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTypeParam {
    pub name: Option<Ustr>,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferType {
    TypeVar(TypeVar),
    IntVar(IntVar),
}

pub trait Typed {
    fn ty(&self) -> Type;
    fn ty_mut(&mut self) -> &mut Type;

    fn set_ty(&mut self, ty: Type) {
        *self.ty_mut() = ty;
    }
}
