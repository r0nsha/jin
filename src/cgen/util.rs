use codespan_reporting::files::{Files, Location};
use pretty::RcDoc as D;

use crate::{
    cgen::{generate::Generator, util},
    mir::Block,
    span::Span,
};

impl<'db> Generator<'db> {
    pub fn panic_if(&self, cond: D<'db>, msg: &str, span: Span) -> D<'db> {
        let then = stmt(|| self.call_panic(msg, span));
        if_stmt(cond, then, None)
    }

    fn create_location_value(&self, span: Span) -> D<'db> {
        let sources = self.db.sources.borrow();
        let source = sources.get(span.source_id()).unwrap();
        let path = source.path();
        let Location { line_number, column_number } =
            source.location(span.source_id(), span.start() as usize).unwrap();

        D::text("(struct jin_rt_location)").append(D::space()).append(
            util::struct_lit(vec![
                ("path", D::text(format!("\"{path}\""))),
                ("line", D::text(line_number.to_string())),
                ("column", D::text(column_number.to_string())),
            ]),
        )
    }

    pub fn call_panic(&self, msg: &str, span: Span) -> D<'db> {
        call(
            "jin_rt_panic_at",
            self.create_location_value(span)
                .append(", ")
                .append(D::text(format!("\"{msg}\""))),
        )
    }
}

pub const NEST: isize = 2;

pub fn call_alloc(ty: D<'_>) -> D<'_> {
    cast(
        ty.clone().append(D::text("*")),
        call("jin_rt_alloc", call("sizeof", ty)),
    )
}

pub fn call_free(value: D<'_>) -> D<'_> {
    call("jin_rt_free", value)
}

pub fn call<'a>(name: &'a str, params: D<'a>) -> D<'a> {
    D::text(name).append("(").append(params).append(")")
}

pub fn str_value(value: &str) -> D {
    struct_lit(vec![
        ("ptr", str_lit(value)),
        ("len", D::text(value.len().to_string())),
    ])
}

pub fn str_lit(value: &str) -> D {
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
