use std::fmt;

use pretty::RcDoc as D;

use crate::{
    cgen::generate::Generator,
    db::StructId,
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
}

impl<'db> CTy<'db> for TyKind {
    fn cty(&self, cx: &Generator<'db>) -> D<'db> {
        match self {
            Self::Fn(fty) => fty.cty(cx),
            Self::Struct(sid) => sid.cty(cx),
            Self::RawPtr(ty) => ty.cty(cx).append(D::text("*")),
            Self::Int(ity) => ity.cty(cx),
            Self::Uint(uty) => uty.cty(cx),
            Self::Float(fty) => fty.cty(cx),
            Self::Str => D::text(sym::STR),
            Self::Bool => D::text(sym::BOOL),
            Self::Unit => D::text("unit"),
            Self::Never => D::text(sym::NEVER),
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

impl<'db> CTy<'db> for StructId {
    fn cty(&self, cx: &Generator<'db>) -> D<'db> {
        let name = D::text(cx.struct_names[self].clone());

        match cx.curr_generated_struct {
            Some(sid) if sid == *self => {
                D::text("struct").append(D::space()).append(name)
            }
            _ => name,
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
