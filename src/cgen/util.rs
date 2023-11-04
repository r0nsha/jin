use pretty::RcDoc;

pub fn str_value(value: &str) -> RcDoc {
    RcDoc::text("{")
        .append(RcDoc::text(".ptr = ").append(str_lit(value)))
        .append(RcDoc::text(", "))
        .append(RcDoc::text(format!(".len = {}", value.len())))
        .append(RcDoc::text("}"))
        .group()
}

pub fn str_lit(value: &str) -> RcDoc {
    RcDoc::text(format!("\"{value}\""))
}

pub fn bool_value<'a>(value: bool) -> RcDoc<'a> {
    RcDoc::text(if value { "true" } else { "false" })
}
