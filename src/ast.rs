#[derive(Clone, PartialEq, Eq)]
pub enum Ast {
    Fn(Fn),
    Lit(Lit),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Fn {
    body: Box<Ast>,
    // TODO: span
}

#[derive(Clone, PartialEq, Eq)]
pub struct Lit {
    kind: LitKind,
    // TODO: span
}

#[derive(Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
}
