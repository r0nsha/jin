use core::fmt;
use std::iter;

use codespan_reporting::files::{Files, Location};
use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{FnState, Generator, DATA_FIELD, REFCNT_FIELD},
        ty::CTy,
        util,
    },
    mir::{Block, ValueId},
    span::Span,
    ty::TyKind,
};

impl<'db> Generator<'db> {
    pub fn panic_if(&self, cond: D<'db>, msg: &str, span: Span) -> D<'db> {
        let then = stmt(|| self.call_panic(msg, span));
        if_stmt(cond, then, None)
    }

    fn create_location_value(&self, span: Span) -> D<'db> {
        let sources = self.db.sources.borrow();
        let source = sources.get(span.source_id()).unwrap();

        let root_path =
            &self.db.find_package_by_source_id(source.id()).unwrap().root_path;
        let root_parent = root_path.parent().unwrap_or(root_path);
        let path = source.path();
        let path = path.strip_prefix(root_parent).unwrap_or(path);

        let Location { line_number, column_number } =
            source.location(span.source_id(), span.start() as usize).unwrap();

        D::text("(struct jin_rt_location)").append(D::space()).append(
            util::struct_lit(vec![
                ("path", str_lit(path)),
                ("line", D::text(line_number.to_string())),
                ("column", D::text(column_number.to_string())),
            ]),
        )
    }

    pub fn call_panic(&self, msg: &str, span: Span) -> D<'db> {
        call(
            D::text("jin_rt_panic_at"),
            [str_lit(msg), self.create_location_value(span)],
        )
    }

    pub fn field(
        &self,
        state: &FnState<'db>,
        value: ValueId,
        field: &str,
    ) -> D<'db> {
        let ty = state.body.value(value).ty;

        match ty.kind() {
            TyKind::Adt(adt, _) if self.db[*adt].is_ref() => {
                self.adt_field(state, value, field)
            }
            TyKind::Ref(_, _) => self.adt_field(state, value, field),
            _ => util::field(self.value(state, value), field, ty.is_ptr(self)),
        }
    }

    pub fn adt_field(
        &self,
        state: &FnState<'db>,
        value: ValueId,
        field: &str,
    ) -> D<'db> {
        let data_field =
            util::field(self.value(state, value), DATA_FIELD, true);
        util::field(data_field, field, false)
    }

    pub fn refcnt_field(&self, state: &FnState<'db>, value: ValueId) -> D<'db> {
        util::field(self.value(state, value), REFCNT_FIELD, true)
    }

    pub fn refcheck_and_free(
        &self,
        state: &FnState<'db>,
        value: ValueId,
        span: Span,
    ) -> D<'db> {
        let refcheck = stmt(|| self.refcheck(state, value, span));
        let free_call = stmt(|| util::call_free(self.value(state, value)));
        D::intersperse([refcheck, free_call], D::hardline())
    }

    pub fn refcheck(
        &self,
        state: &FnState<'db>,
        value: ValueId,
        span: Span,
    ) -> D<'db> {
        let refcnt = self.refcnt_field(state, value);
        let fmt = str_lit(format!(
            "cannot destroy a value of type `{}` as it still has %u \
             reference(s)",
            state.body.value(value).ty.display(self.db)
        ));
        let loc = self.create_location_value(span);
        call(D::text("jin_rt_refcheck"), [refcnt, fmt, loc])
    }
}

pub const NEST: isize = 2;

pub fn call_alloc(ty: D<'_>) -> D<'_> {
    cast(
        ty.clone().append(D::text("*")),
        call(D::text("jin_rt_alloc"), iter::once(sizeof(ty))),
    )
}

pub fn sizeof(ty: D<'_>) -> D<'_> {
    call(D::text("sizeof"), iter::once(ty))
}

pub fn call_free(value: D<'_>) -> D<'_> {
    call(D::text("jin_rt_free"), iter::once(value))
}

pub fn call<'a>(callee: D<'a>, args: impl IntoIterator<Item = D<'a>>) -> D<'a> {
    callee
        .append(D::text("("))
        .append(
            D::intersperse(args, D::text(",").append(D::space()))
                .nest(1)
                .group(),
        )
        .append(D::text(")"))
}

pub fn str_value(value: &str) -> D {
    struct_lit(vec![
        ("ptr", str_lit(value)),
        ("len", D::text(value.len().to_string())),
    ])
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

pub fn struct_lit<'a>(fields: Vec<(&'a str, D<'a>)>) -> D<'a> {
    soft_block(|| {
        D::intersperse(
            fields.into_iter().map(|(name, value)| {
                D::text(format!(".{name} = ")).append(value)
            }),
            D::text(",").append(D::softline()),
        )
    })
}

pub fn stmt<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    f().append(D::text(";"))
}

pub fn assign<'a>(l: D<'a>, r: D<'a>) -> D<'a> {
    l.append(D::space()).append(D::text("=")).append(D::space()).append(r)
}

pub fn field<'a>(value: D<'a>, field: &str, is_ptr: bool) -> D<'a> {
    value.append(D::text(format!(
        "{}{}",
        if is_ptr { "->" } else { "." },
        field
    )))
}

pub fn goto_stmt(blk: &Block) -> D<'_> {
    stmt(|| {
        D::text("goto").append(D::space()).append(D::text(blk.display_name()))
    })
}

pub fn cast<'a>(ty: D<'a>, value: D<'a>) -> D<'a> {
    D::text("(").append(ty).append(D::text(")")).append(value)
}

pub fn if_stmt<'a>(
    cond: D<'a>,
    then: D<'a>,
    otherwise: Option<D<'a>>,
) -> D<'a> {
    D::text("if")
        .append(D::space())
        .append(D::text("("))
        .append(cond)
        .append(D::text(")"))
        .append(D::space())
        .append(block(|| then))
        .append(otherwise.map_or(D::nil(), |o| {
            D::space()
                .append(D::text("else"))
                .append(D::space())
                .append(block(|| o))
        }))
}

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

pub fn attr<'a>(name: &str) -> D<'a> {
    D::text(format!("__attribute__(({name}))"))
}
