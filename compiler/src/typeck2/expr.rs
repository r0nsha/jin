use crate::{
    ast::{self, Ast},
    counter::Counter,
    db::{
        Adt, AdtField, AdtId, AdtKind, Db, DefId, DefKind, FnInfo, Intrinsic, ModuleId, StructDef,
        UnionDef, Variant, VariantId,
    },
    diagnostics::{Diagnostic, Label},
    hir,
    hir::{ExprId, FnParam, Hir},
    macros::create_bool_enum,
    middle::{BinOp, CallConv, CmpOp, IsUfcs, Mutability, Pat, TyExpr, TyParam, UnOp, Vis},
    span::{Span, Spanned},
    sym,
    ty::{
        FloatVar, FnTy, FnTyFlags, FnTyParam, InferTy, Instantiation, IntVar, ParamTy, Ty, TyKind,
        TyVar,
    },
    word::{Word, WordMap},
};
