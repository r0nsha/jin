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

pub fn stmt<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    f().append(D::text(";"))
}

pub fn value_name<'a>(id: ValueId) -> D<'a> {
    D::text("v").append(id.to_string())
}

pub fn block_name(blk: &Block) -> D<'_> {
    D::text(format!("{}_{}", blk.name(), blk.id()))
}

pub fn goto_stmt(blk: &Block) -> D<'_> {
    stmt(|| D::text("goto").append(D::space()).append(block_name(blk)))
}

pub fn if_stmt<'a>(cond: D<'a>, then: D<'a>, otherwise: D<'a>) -> D<'a> {
    stmt(|| {
        D::text("if")
            .append(D::space())
            .append(D::text("("))
            .append(cond)
            .append(D::text(")"))
            .append(D::space())
            .append(block(|| then))
            .append(D::space())
            .append(D::text("else"))
            .append(D::space())
            .append(block(|| otherwise))
    })
}

pub fn block<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    D::text("{")
        .append(D::softline())
        .append(f())
        .nest(NEST)
        .group()
        .append(D::softline())
        .append(D::text("}"))
}
