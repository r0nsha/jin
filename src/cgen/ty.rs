use std::fmt;

use pretty::RcDoc as D;

use crate::{
    cgen::generate::Generator,
    db::{AdtId, AdtKind, StructKind},
    sym,
    ty::{FloatTy, FnTy, IntTy, TyKind, UintTy},
};

pub trait CTy<'db>
where
    Self: fmt::Debug,
{
    fn cty(&self, cx: &Generator<'db>) -> D<'db>;

    fn cdecl(&self, cx: &Generator<'db>, name: D<'db>) -> D<'db>
    where
        Self: Sized,
    {
        ty_and_name(self, cx, name)
    }

    fn is_ptr(&self, _cx: &Generator<'db>) -> bool {
        false
    }
}

impl<'db> CTy<'db> for TyKind {
    fn cty(&self, cx: &Generator<'db>) -> D<'db> {
        match self {
            Self::Fn(fty) => fty.cty(cx),
            Self::Adt(adt_id) => adt_id.cty(cx),
            Self::RawPtr(ty) => ty.cty(cx).append(D::text("*")),
            Self::Int(ity) => ity.cty(cx),
            Self::Uint(uty) => uty.cty(cx),
            Self::Float(fty) => fty.cty(cx),
            Self::Str | Self::Bool | Self::Never | Self::Unit => {
                D::text(self.to_string(cx.db))
            }
            _ => panic!("unexpected type {self:?}"),
        }
    }

    fn is_ptr(&self, cx: &Generator<'db>) -> bool {
        match self {
            Self::Adt(adt_id) => cx.db[*adt_id].is_ref(),
            Self::RawPtr(_) => true,
            Self::Fn(_)
            | Self::Int(_)
            | Self::Uint(_)
            | Self::Float(_)
            | Self::Str
            | Self::Bool
            | Self::Never
            | Self::Unit => false,
            _ => panic!("unexpected type {self:?}"),
        }
    }

    fn cdecl(&self, cx: &Generator<'db>, name: D<'db>) -> D<'db> {
        match self {
            Self::Fn(fty) => fty.cdecl(cx, name),
            _ => ty_and_name(self, cx, name),
        }
    }
}

impl<'db> CTy<'db> for IntTy {
    fn cty(&self, _: &Generator<'db>) -> D<'db> {
        D::text(match self {
            Self::I8 => sym::I8,
            Self::I16 => sym::I16,
            Self::I32 => sym::I32,
            Self::I64 => sym::I64,
            Self::Int => "isize",
        })
    }
}

impl<'db> CTy<'db> for UintTy {
    fn cty(&self, _: &Generator<'db>) -> D<'db> {
        D::text(match self {
            Self::U8 => sym::U8,
            Self::U16 => sym::U16,
            Self::U32 => sym::U32,
            Self::U64 => sym::U64,
            Self::Uint => "usize",
        })
    }
}

impl<'db> CTy<'db> for FloatTy {
    fn cty(&self, _: &Generator<'db>) -> D<'db> {
        D::text(match self {
            Self::F32 => sym::F32,
            Self::F64 => sym::F64,
        })
    }
}

impl<'db> CTy<'db> for AdtId {
    fn cty(&self, cx: &Generator<'db>) -> D<'db> {
        let name = D::text(cx.adt_names[self].as_str());

        let name = if cx.defining_types {
            D::text("struct").append(D::space()).append(name)
        } else {
            name
        };

        match &cx.db[*self].kind {
            AdtKind::Struct(s) => match s.kind {
                StructKind::Ref => name.append(D::text("*")),
                StructKind::Extern => name,
            },
        }
    }
}

impl<'db> CTy<'db> for FnTy {
    fn cty(&self, cx: &Generator<'db>) -> D<'db> {
        fn_ty(self, cx, None)
    }

    fn cdecl(&self, cx: &Generator<'db>, name: D<'db>) -> D<'db> {
        fn_ty(self, cx, Some(name))
    }
}

fn fn_ty<'a>(fn_ty: &FnTy, cx: &Generator<'a>, name: Option<D<'a>>) -> D<'a> {
    fn_ty
        .ret
        .cty(cx)
        .append(D::space())
        .append(D::text("("))
        .append(D::text("*"))
        .append(name.unwrap_or(D::nil()))
        .append(D::text(")"))
        .append(
            D::text("(")
                .append(
                    D::intersperse(
                        fn_ty.params.iter().map(|p| p.ty.cty(cx)),
                        D::text(",").append(D::space()),
                    )
                    .nest(1)
                    .group(),
                )
                .append(if fn_ty.is_c_variadic {
                    D::text(", ...")
                } else {
                    D::nil()
                })
                .append(D::text(")")),
        )
}

fn ty_and_name<'a>(
    ty: &impl CTy<'a>,
    cx: &Generator<'a>,
    name: D<'a>,
) -> D<'a> {
    ty.cty(cx).append(D::space()).append(name)
}
