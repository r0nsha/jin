use pretty::RcDoc as D;

use crate::mir::{Block, ValueId};

pub const NEST: isize = 2;

pub fn str_value(value: &str) -> D {
    D::text("{")
        .append(D::text(".ptr = ").append(str_lit(value)))
        .append(D::text(", "))
        .append(D::text(format!(".len = {}", value.len())))
        .append(D::text("}"))
        .group()
}

pub fn str_lit(value: &str) -> D {
    D::text(format!("\"{value}\""))
}

pub fn bool_value<'a>(value: bool) -> D<'a> {
    D::text(if value { "true" } else { "false" })
}

pub fn unit_value<'a>() -> D<'a> {
    D::text("{}")
}

pub fn stmt<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    f().append(D::text(";"))
}

pub fn value_name<'a>(id: ValueId) -> D<'a> {
    D::text(value_name_str(id))
}

pub fn value_name_str(id: ValueId) -> String {
    format!("v{id}")
}

pub fn block_name(blk: &Block) -> D<'_> {
    D::text(format!("{}_{}", blk.name(), blk.id()))
}

pub fn goto_stmt(blk: &Block) -> D<'_> {
    stmt(|| D::text("goto").append(D::space()).append(block_name(blk)))
}

pub fn if_stmt<'a>(cond: D<'a>, then: D<'a>, otherwise: Option<D<'a>>) -> D<'a> {
    D::text("if")
        .append(D::space())
        .append(D::text("("))
        .append(cond)
        .append(D::text(")"))
        .append(D::space())
        .append(block(|| then))
        .append(otherwise.map_or(D::nil(), |o| {
            D::space().append(D::text("else")).append(D::space()).append(block(|| o))
        }))
}

pub fn block<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    D::text("{")
        .append(D::hardline())
        .append(f())
        .nest(NEST)
        .group()
        .append(D::hardline())
        .append(D::text("}"))
}

pub fn attr<'a>(name: &str) -> D<'a> {
    D::text(format!("__attribute__(({name}))"))
}
