mod printer;

use derive_more::{From, Into};
use enum_as_inner::EnumAsInner;
use ustr::Ustr;

use crate::{
    db::{Db, TypeId},
    span::Span,
    ty::printer::TypePrinter,
};

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Type {
    Function(FunctionType),
    Int(IntType, Span),
    Bool(Span),
    // TODO: when we implement tuples, Unit should become Tuple([])
    Unit(Span),
    Never(Span),
    Infer(InferType, Span),
}

impl Type {
    pub fn default_int(span: Span) -> Self {
        Self::Int(IntType::Int, span)
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Self> {
        match self {
            Self::Function(fun) => fun.ret.occurs_check(var).map_err(|_| self.clone()),
            Self::Infer(InferType::TypeVar(v), _) => {
                if *v == var {
                    Err(self.clone())
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Function(FunctionType { span, .. })
            | Self::Int(_, span)
            | Self::Bool(span)
            | Self::Unit(span)
            | Self::Never(span)
            | Self::Infer(_, span) => *span,
        }
    }

    pub fn display<'db>(&'db self, db: &'db Db) -> TypePrinter<'db> {
        TypePrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Db) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct TypeVar(u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct IntVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntVarValue {
    Int(IntType, Span),
}

impl From<IntVarValue> for Type {
    fn from(value: IntVarValue) -> Self {
        match value {
            IntVarValue::Int(ty, span) => Self::Int(ty, span),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntType {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub ret: Box<Type>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTypeParam {
    pub name: Option<Ustr>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferType {
    TypeVar(TypeVar),
    IntVar(IntVar),
}

pub trait Typed {
    fn ty(&self) -> TypeId;
    fn ty_mut(&mut self) -> &mut TypeId;

    fn set_ty(&mut self, ty: TypeId) {
        *self.ty_mut() = ty;
    }
}
