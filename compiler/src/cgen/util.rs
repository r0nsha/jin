use core::fmt;
use std::iter;

use codespan_reporting::files::Files;
use pretty::RcDoc as D;

use crate::{
    cgen::{
        generate::{GenState, Generator, DATA_FIELD, REFCNT_FIELD},
        ty::CTy,
        util,
    },
    mir::{Block, ValueId, ValueKind},
    span::Span,
    sym,
    ty::{Ty, TyKind},
};

impl<'db> Generator<'db> {
    pub fn panic_if(
        &self,
        state: &GenState<'db>,
        cond: D<'db>,
        msg: &str,
        span: Span,
    ) -> D<'db> {
        if_stmt(cond, stmt(|| self.call_panic(state, msg, span)), None)
    }

    pub fn call_panic(
        &self,
        state: &GenState<'db>,
        msg: &str,
        span: Span,
    ) -> D<'db> {
        call(
            D::text("jinrt_panic_at"),
            [
                D::text("backtrace"),
                str_lit(msg),
                self.create_stackframe_value(state, span),
            ],
        )
    }

    pub fn field(
        &self,
        state: &GenState<'db>,
        value: ValueId,
        field: &str,
    ) -> D<'db> {
        let value = state.body.value(value);

        match &value.kind {
            ValueKind::Variant(_, _) => {
                util::field(self.value(state, value.id), field, false)
            }
            _ => match value.ty.kind() {
                TyKind::Adt(adt, _) if self.db[*adt].is_ref() => {
                    self.adt_field(state, value.id, field)
                }
                TyKind::Ref(_, _) => self.adt_field(state, value.id, field),
                _ => util::field(
                    self.value(state, value.id),
                    field,
                    value.ty.is_ptr(self),
                ),
            },
        }
    }

    pub fn variant(
        &self,
        state: &GenState<'db>,
        value: ValueId,
        variant: &str,
    ) -> D<'db> {
        self.adt_field(state, value, variant)
    }

    pub fn adt_field(
        &self,
        state: &GenState<'db>,
        value: ValueId,
        field: &str,
    ) -> D<'db> {
        let data_field =
            util::field(self.value(state, value), DATA_FIELD, true);
        util::field(data_field, field, false)
    }

    pub fn refcnt_field(
        &self,
        state: &GenState<'db>,
        value: ValueId,
    ) -> D<'db> {
        util::field(self.value(state, value), REFCNT_FIELD, true)
    }

    pub fn alloc_value(
        &mut self,
        state: &GenState<'db>,
        value: ValueId,
    ) -> D<'db> {
        let ty = state.body.value(value).ty;

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

    pub fn alloc_slice(
        &mut self,
        state: &GenState<'db>,
        value: ValueId,
        cap: ValueId,
    ) -> D<'db> {
        todo!()
    }

    pub fn free(
        &self,
        state: &GenState<'db>,
        value: ValueId,
        traced: bool,
        span: Span,
    ) -> D<'db> {
        let tyname = str_lit(state.body.value(value).ty.display(self.db));
        let frame = self.create_stackframe_value(state, span);
        let free_call = stmt(|| {
            call(
                D::text("jinrt_free"),
                [D::text("backtrace"), self.value(state, value), tyname, frame],
            )
        });

        if traced {
            self.with_stack_frame(state, free_call, span)
        } else {
            free_call
        }
    }

    pub fn with_stack_frame(
        &self,
        state: &GenState<'db>,
        stmt: D<'db>,
        span: Span,
    ) -> D<'db> {
        let push_frame = self.push_stack_frame(state, span);
        let pop_frame = Self::pop_stack_frame();
        D::intersperse([push_frame, stmt, pop_frame], D::hardline())
    }

    pub fn push_stack_frame(
        &self,
        state: &GenState<'db>,
        span: Span,
    ) -> D<'db> {
        stmt(|| {
            call(
                D::text("jinrt_backtrace_push"),
                [
                    D::text("backtrace"),
                    self.create_stackframe_value(state, span),
                ],
            )
        })
    }

    pub fn pop_stack_frame() -> D<'db> {
        stmt(|| call(D::text("jinrt_backtrace_pop"), [D::text("backtrace")]))
    }

    fn create_stackframe_value(
        &self,
        state: &GenState<'db>,
        span: Span,
    ) -> D<'db> {
        let sources = self.db.sources.borrow();
        let source = sources.get(span.source_id()).unwrap();

        let root_path =
            &self.db.find_package_by_source_id(source.id()).unwrap().root_path;
        let root_parent = root_path.parent().unwrap_or(root_path);
        let path = source.path();
        let file = path.strip_prefix(root_parent).unwrap_or(path);

        let loc =
            source.location(span.source_id(), span.start() as usize).unwrap();

        D::text("(struct jinrt_stackframe)").append(D::space()).append(
            util::struct_lit(vec![
                ("file", str_lit(file)),
                ("line", D::text(loc.line_number.to_string())),
                ("in", str_lit(state.name)),
            ]),
        )
    }
}

pub const NEST: isize = 2;

pub fn sizeof(ty: D<'_>) -> D<'_> {
    call(D::text("sizeof"), iter::once(ty))
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

pub fn cmp_strs<'a>(a: D<'a>, b: D<'a>) -> D<'a> {
    util::call(D::text("jinrt_strcmp"), [a, b])
}

pub fn str_value(value: &str) -> D {
    D::text("(str)").append(struct_lit(vec![
        (sym::PTR, str_lit(value)),
        (sym::LEN, D::text(value.len().to_string())),
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
    value.append(D::text(format!(
        "{}{}",
        if is_ptr { "->" } else { "." },
        field
    )))
}

pub fn goto_stmt(block: &Block) -> D<'_> {
    stmt(|| {
        D::text("goto").append(D::space()).append(D::text(block.display_name()))
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

pub fn switch_stmt<'a>(
    cond: D<'a>,
    cases: impl Iterator<Item = (D<'a>, D<'a>)>,
) -> D<'a> {
    D::text("switch")
        .append(D::space())
        .append(D::text("("))
        .append(cond)
        .append(D::text(")"))
        .append(D::space())
        .append(block(|| {
            D::intersperse(
                cases.into_iter().map(|(case, body)| {
                    D::text("case")
                        .append(D::space())
                        .append(case)
                        .append(D::text(":"))
                        .append(D::hardline())
                        .append(D::intersperse(
                            [body, stmt(|| D::text("break"))],
                            D::hardline(),
                        ))
                        .nest(NEST)
                }),
                D::hardline(),
            )
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
