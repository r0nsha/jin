use core::fmt;
use std::iter;

use codespan_reporting::files::Files;
use compiler_core::{
    db::VariantId,
    span::Span,
    sym,
    ty::{Ty, TyKind},
};
use compiler_helpers::escape;
use compiler_mir::{Block, ValueId, ValueKind};
use pretty::RcDoc as D;

use crate::{
    generate::{variant_name, GenState, Generator, DATA_FIELD, START_FIELD},
    ty::CTy,
    util,
};

impl<'db> Generator<'db> {
    pub fn panic_if(&self, state: &GenState<'db>, cond: D<'db>, msg: &str, span: Span) -> D<'db> {
        if_stmt(cond, stmt(|| self.call_panic(state, msg, span)), None)
    }

    pub fn call_panic(&self, state: &GenState<'db>, msg: &str, span: Span) -> D<'db> {
        call(
            D::text("jinrt_panic_raw"),
            [D::text("backtrace"), str_lit(msg), self.create_stackframe_value(state, span)],
        )
    }

    pub fn field(&mut self, state: &GenState<'db>, value: ValueId, field: &str) -> D<'db> {
        let value = state.body.value(value);
        let value_doc = self.value(state, value.id);
        let ty = value.ty.auto_deref();

        match &value.kind {
            ValueKind::Variant(_, _) => util::field(value_doc, field, false),
            _ => match (ty.kind(), field) {
                (TyKind::Adt(adt, _), _) if self.db[*adt].is_ref() => {
                    self.rc_field(state, value.id, field)
                }
                (TyKind::Ref(_, _), _) => self.rc_field(state, value.id, field),
                (TyKind::Slice(_) | TyKind::Str, sym::field::PTR) => {
                    self.slice_addr_field(state, value.id)
                }
                (TyKind::Slice(_) | TyKind::Str, sym::field::CAP) => {
                    self.slice_cap_field(state, value.id)
                }
                _ => util::field(value_doc, field, ty.is_ptr(self)),
            },
        }
    }

    pub fn rc_field(&mut self, state: &GenState<'db>, value: ValueId, field: &str) -> D<'db> {
        let value = self.value(state, value);
        let data_field = util::field(value, DATA_FIELD, true);
        util::field(data_field, field, false)
    }

    pub fn variant_field(
        &mut self,
        state: &GenState<'db>,
        value: ValueId,
        id: VariantId,
    ) -> D<'db> {
        let variant = &self.db[id];
        let name = variant_name(variant.name, variant.id);
        let ty = state.ty_of(value);

        if ty.auto_deref().is_rc(self.db) {
            self.rc_field(state, value, &name)
        } else {
            util::field(self.value(state, value), &name, false)
        }
    }

    pub fn slice_addr_field(&mut self, state: &GenState<'db>, slice: ValueId) -> D<'db> {
        // ((elem_ty*)(slice.start))
        let elem_ty = Self::slice_value_elem_ty(state, slice);
        let start_field = util::field(self.value(state, slice), START_FIELD, false);
        group(util::cast(elem_ty.cty(self).append(D::text("*")), start_field))
    }

    pub fn slice_cap_field(&mut self, state: &GenState<'db>, slice: ValueId) -> D<'db> {
        call(D::text("jinrt_slice_cap"), [addr(self.value(state, slice))])
    }

    pub fn slice_index(&mut self, state: &GenState<'db>, slice: ValueId, index: ValueId) -> D<'db> {
        let data_field = self.slice_addr_field(state, slice);
        data_field.append(D::text("[")).append(self.value(state, index)).append(D::text("]"))
    }

    pub fn alloc_value(&mut self, state: &GenState<'db>, value: ValueId) -> D<'db> {
        let ty = state.ty_of(value);

        let ty_doc = match ty.kind() {
            TyKind::Adt(..) => D::text(self.adt_names[&ty].as_str()),
            kind => panic!("unexpected type {kind:?} in Inst::Alloc"),
        };

        self.value_assign(state, value, |_| {
            cast(
                ty_doc.clone().append(D::text("*")),
                call(D::text("jinrt_alloc"), [sizeof(ty_doc)]),
            )
        })
    }

    pub fn alloc_slice(&mut self, state: &GenState<'db>, value: ValueId, cap: ValueId) -> D<'db> {
        self.value_assign(state, value, |this| {
            call(
                D::text("jinrt_slice_alloc"),
                [this.sizeof_slice_elem(state, value), this.value(state, cap)],
            )
        })
    }

    pub fn sizeof_slice_elem(&mut self, state: &GenState<'db>, slice: ValueId) -> D<'db> {
        let elem_ty = Self::slice_value_elem_ty(state, slice);
        sizeof(elem_ty.cty(self))
    }

    pub fn free(
        &mut self,
        state: &GenState<'db>,
        value: ValueId,
        traced: bool,
        span: Span,
    ) -> D<'db> {
        let free_call = stmt(|| self.call_free(state, value, span));

        if traced {
            self.with_stack_frame(state, free_call, span)
        } else {
            free_call
        }
    }

    pub fn call_free(&mut self, state: &GenState<'db>, value: ValueId, span: Span) -> D<'db> {
        let free_fn = if state.value_is_slice(value) { "jinrt_slice_free" } else { "jinrt_free" };

        let tyname = str_lit(state.ty_of(value).display(self.db));
        let frame = self.create_stackframe_value(state, span);
        let value = self.value(state, value);
        call(D::text(free_fn), [D::text("backtrace"), value, tyname, frame])
    }

    fn slice_value_elem_ty(state: &GenState<'db>, slice: ValueId) -> Ty {
        state.ty_of(slice).auto_deref().slice_elem().unwrap()
    }

    pub fn with_stack_frame(&self, state: &GenState<'db>, stmt: D<'db>, span: Span) -> D<'db> {
        let push_frame = self.push_stack_frame(state, span);
        let pop_frame = Self::pop_stack_frame();
        D::intersperse([push_frame, stmt, pop_frame], D::hardline())
    }

    pub fn push_stack_frame(&self, state: &GenState<'db>, span: Span) -> D<'db> {
        stmt(|| {
            call(
                D::text("jinrt_backtrace_push"),
                [D::text("backtrace"), self.create_stackframe_value(state, span)],
            )
        })
    }

    pub fn pop_stack_frame() -> D<'db> {
        stmt(|| call(D::text("jinrt_backtrace_pop"), [D::text("backtrace")]))
    }

    pub fn create_stackframe_value(&self, state: &GenState<'db>, span: Span) -> D<'db> {
        let source = self.db.sources.get(span.source_id()).unwrap();
        let root_path = &self.db.find_package_by_source_id(source.id()).unwrap().root_path;
        let root_parent = root_path.parent().unwrap_or(root_path);
        let path = source.path();
        let file = path.strip_prefix(root_parent).unwrap_or(path);

        let loc = source.location(span.source_id(), span.start() as usize).unwrap();

        D::text("(struct jinrt_stackframe)").append(D::space()).append(util::struct_lit(vec![
            ("file", str_lit(file)),
            ("line", D::text(loc.line_number.to_string())),
            ("in", str_lit(state.name)),
        ]))
    }
}

pub const NEST: isize = 2;

pub fn sizeof(ty: D<'_>) -> D<'_> {
    call(D::text("sizeof"), iter::once(ty))
}

pub fn call<'a>(callee: D<'a>, args: impl IntoIterator<Item = D<'a>>) -> D<'a> {
    callee
        .append(util::group(D::intersperse(args, D::text(",").append(D::space())).nest(1).group()))
}

pub fn cmp_strs<'a>(a: D<'a>, b: D<'a>) -> D<'a> {
    util::call(D::text("jinrt_str_cmp"), [a, b])
}

pub fn escaped_str_value(value: &str) -> D {
    D::text("(str)").append(struct_lit(vec![
        ("array", null_value()),
        (START_FIELD, str_lit(escape::escape(value))),
        (sym::field::LEN, D::text(value.len().to_string())),
    ]))
}

pub fn str_lit<'a>(value: impl fmt::Display) -> D<'a> {
    D::text(format!("\"{value}\""))
}

pub fn bool_value<'a>(value: bool) -> D<'a> {
    D::text(if value { "true" } else { "false" })
}

pub fn unit_value<'a>() -> D<'a> {
    D::text("(unit){}")
}

pub fn null_value<'a>() -> D<'a> {
    D::text("NULL")
}

pub fn struct_lit<'a>(fields: Vec<(&'a str, D<'a>)>) -> D<'a> {
    soft_block(|| {
        D::intersperse(
            fields.into_iter().map(|(name, value)| D::text(format!(".{name} = ")).append(value)),
            D::text(",").append(D::softline()),
        )
    })
}

pub fn stmts<'a>(docs: impl IntoIterator<Item = D<'a>>) -> D<'a> {
    D::intersperse(docs.into_iter().map(|d| stmt(|| d)), D::hardline())
}

pub fn stmt<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    f().append(D::text(";"))
}

pub fn assign<'a>(l: D<'a>, r: D<'a>) -> D<'a> {
    l.append(D::space()).append(D::text("=")).append(D::space()).append(r)
}

pub fn field<'a>(value: D<'a>, field: &str, is_ptr: bool) -> D<'a> {
    value.append(D::text(format!("{}{}", if is_ptr { "->" } else { "." }, field)))
}

pub fn goto_stmt(block: &Block) -> D<'_> {
    stmt(|| D::text("goto").append(D::space()).append(D::text(block.display_name())))
}

pub fn cast<'a>(ty: D<'a>, value: D<'a>) -> D<'a> {
    util::group(ty).append(util::group(value))
}

pub fn addr(value: D<'_>) -> D<'_> {
    D::text("&").append(value)
}

pub fn deref(value: D<'_>) -> D<'_> {
    D::text("*").append(value)
}

pub fn if_stmt<'a>(cond: D<'a>, then: D<'a>, otherwise: Option<D<'a>>) -> D<'a> {
    D::text("if")
        .append(D::space())
        .append(util::group(cond))
        .append(D::space())
        .append(block(|| then))
        .append(otherwise.map_or(D::nil(), |o| {
            D::space().append(D::text("else")).append(D::space()).append(block(|| o))
        }))
}

#[allow(unused)]
pub fn ternary<'a>(cond: D<'a>, then: D<'a>, otherwise: D<'a>) -> D<'a> {
    cond.append(D::space())
        .append(D::text("?"))
        .append(D::space())
        .append(then)
        .append(D::space())
        .append(D::text(":"))
        .append(D::space())
        .append(otherwise)
}

pub fn switch_stmt<'a>(cond: D<'a>, cases: impl Iterator<Item = (D<'a>, D<'a>)>) -> D<'a> {
    D::text("switch").append(D::space()).append(util::group(cond)).append(D::space()).append(block(
        || {
            D::intersperse(
                cases.into_iter().map(|(case, body)| {
                    D::text("case")
                        .append(D::space())
                        .append(case)
                        .append(D::text(":"))
                        .append(D::hardline())
                        .append(D::intersperse([body, stmt(|| D::text("break"))], D::hardline()))
                        .nest(NEST)
                }),
                D::hardline(),
            )
        },
    ))
}

pub fn block<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    block_(f, NEST)
}

pub fn block_<'a>(f: impl FnOnce() -> D<'a>, nest: isize) -> D<'a> {
    D::text("{")
        .append(D::hardline())
        .append(f())
        .nest(nest)
        .group()
        .append(D::hardline())
        .append(D::text("}"))
}

pub fn soft_block<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    soft_block_(f, NEST)
}

pub fn soft_block_<'a>(f: impl FnOnce() -> D<'a>, nest: isize) -> D<'a> {
    D::text("{")
        .append(D::softline())
        .append(f())
        .nest(nest)
        .group()
        .append(D::softline())
        .append(D::text("}"))
}

pub fn group(d: D<'_>) -> D<'_> {
    D::text("(").append(d).append(D::text(")"))
}

pub fn attr<'a>(name: &str) -> D<'a> {
    D::text(format!("__attribute__(({name}))"))
}
