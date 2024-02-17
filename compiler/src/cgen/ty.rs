use core::fmt;

use pretty::RcDoc as D;

use crate::{
    cgen::{generate::Generator, util},
    middle::CallConv,
    ty::{FloatTy, FnTy, IntTy, Ty, TyKind, UintTy},
};

pub trait CTy<'db>
where
    Self: fmt::Debug,
{
    fn cty(&self, cx: &mut Generator<'db>) -> D<'db>;

    fn cdecl(&self, cx: &mut Generator<'db>, name: D<'db>) -> D<'db>
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
    fn cty(&self, cx: &mut Generator<'db>) -> D<'db> {
        match self {
            Self::Fn(fty) => fty.cty(cx),
            Self::Adt(adt_id, targs) => {
                let adt_name = cx.get_or_create_adt(Ty::from_ref(self), *adt_id, targs);
                let name = D::text(format!("struct {adt_name}"));

                if cx.db[*adt_id].is_ref() {
                    name.append("*")
                } else {
                    name
                }
            }
            Self::Slice(..) => D::text("slice"),
            Self::Ref(ty, _) => ty.cty(cx),
            Self::RawPtr(ty) => ty.cty(cx).append(D::text("*")),
            Self::Int(ity) => ity.cty(cx),
            Self::Uint(uty) => uty.cty(cx),
            Self::Float(fty) => fty.cty(cx),
            Self::Str => D::text("str"),
            Self::Bool => D::text("bool"),
            Self::Never => D::text("never"),
            Self::Unit => D::text("unit"),
            _ => panic!("unexpected type {self:?}"),
        }
    }

    fn is_ptr(&self, cx: &Generator<'db>) -> bool {
        match self {
            Self::Adt(adt_id, _) => cx.db[*adt_id].is_ref(),
            Self::Ref(..) | Self::RawPtr(_) => true,
            Self::Fn(_)
            | Self::Int(_)
            | Self::Uint(_)
            | Self::Float(_)
            | Self::Slice(_)
            | Self::Str
            | Self::Bool
            | Self::Never
            | Self::Unit => false,
            _ => panic!("unexpected type {self:?}"),
        }
    }

    fn cdecl(&self, cx: &mut Generator<'db>, name: D<'db>) -> D<'db> {
        match self {
            Self::Fn(fty) => fty.cdecl(cx, name),
            _ => ty_and_name(self, cx, name),
        }
    }
}

impl<'db> CTy<'db> for IntTy {
    fn cty(&self, _: &mut Generator<'db>) -> D<'db> {
        D::text(match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::Int => "isize",
        })
    }
}

impl<'db> CTy<'db> for UintTy {
    fn cty(&self, _: &mut Generator<'db>) -> D<'db> {
        D::text(match self {
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::Uint => "usize",
        })
    }
}

impl<'db> CTy<'db> for FloatTy {
    fn cty(&self, _: &mut Generator<'db>) -> D<'db> {
        D::text(match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
        })
    }
}

impl<'db> CTy<'db> for FnTy {
    fn cty(&self, cx: &mut Generator<'db>) -> D<'db> {
        fn_ty(self, cx, None)
    }

    fn cdecl(&self, cx: &mut Generator<'db>, name: D<'db>) -> D<'db> {
        fn_ty(self, cx, Some(name))
    }
}

fn fn_ty<'a>(fn_ty: &FnTy, cx: &mut Generator<'a>, name: Option<D<'a>>) -> D<'a> {
    let ret = fn_ty.ret.cty(cx);
    let name_doc = util::group(D::text("*").append(name.unwrap_or(D::nil())));

    let mut param_docs = vec![];

    if fn_ty.callconv == CallConv::Jin {
        param_docs.push(D::text("jinrt_backtrace *backtrace"));
    }

    param_docs.extend(fn_ty.params.iter().map(|p| p.ty.cty(cx)));

    let params = D::intersperse(param_docs, D::text(",").append(D::space())).nest(1).group();

    ret.append(D::space()).append(name_doc).append(util::group(
        params.append(if fn_ty.is_c_variadic() { D::text(", ...") } else { D::nil() }),
    ))
}

fn ty_and_name<'a>(ty: &impl CTy<'a>, cx: &mut Generator<'a>, name: D<'a>) -> D<'a> {
    ty.cty(cx).append(D::space()).append(name)
}
