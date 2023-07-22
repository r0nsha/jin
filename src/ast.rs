use ustr::{ustr, Ustr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Fun(Fun),
    Lit(Lit),
}

impl Ast {
    pub fn fun(name: &str, body: Self) -> Self {
        Self::Fun(Fun {
            name: ustr(name),
            body: Box::new(body),
        })
    }

    pub fn int(value: usize) -> Self {
        Self::Lit(Lit {
            kind: LitKind::Int(value),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fun {
    name: Ustr,
    body: Box<Ast>,
    // TODO: span
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lit {
    kind: LitKind,
    // TODO: span
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
}
