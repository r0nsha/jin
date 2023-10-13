mod pretty_print;
pub mod token;

use std::{fmt, io};

use enum_as_inner::EnumAsInner;
use ustr::Ustr;

use crate::{
    db::ModuleId,
    index_vec::{new_key_type, IndexVec},
    middle::{BinOp, TyExpr, UnOp},
    qpath::QPath,
    span::{SourceId, Span, Spanned},
    word::Word,
};

#[derive(Debug, Clone)]
pub struct Ast {
    pub modules: IndexVec<ModuleId, Module>,
}

impl Ast {
    pub fn new() -> Self {
        Self { modules: IndexVec::new() }
    }

    pub fn pretty_print(&self, w: &mut impl io::Write) -> io::Result<()> {
        for module in &self.modules {
            pretty_print::print_module(module, w)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub id: Option<ModuleId>,
    pub source: SourceId,
    pub name: QPath,
    pub items: IndexVec<ItemId, Item>,
    is_main: bool,
}

impl Module {
    pub fn new(source_id: SourceId, name: QPath, is_main: bool) -> Self {
        Self { id: None, source: source_id, name, is_main, items: IndexVec::new() }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

new_key_type!(ItemId);

#[derive(Debug, Clone, EnumAsInner)]
pub enum Item {
    Fn(Fn),
    Let(Let),
    ExternLet(ExternLet),
}

impl Item {
    pub fn walk_names(&self, mut f: impl FnMut(Word)) {
        self._walk_names(&mut f);
    }

    fn _walk_names(&self, f: &mut impl FnMut(Word)) {
        match self {
            Self::Fn(fun) => f(fun.sig.word),
            Self::Let(let_) => let_.pat.walk(|p| f(p.word)),
            Self::ExternLet(let_) => f(let_.word),
        }
    }
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Self::Fn(Fn { span, .. })
            | Self::Let(Let { span, .. })
            | Self::ExternLet(ExternLet { span, .. }) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Item(Item),
    Return { expr: Option<Box<Self>>, span: Span },
    If { cond: Box<Self>, then: Box<Self>, otherwise: Option<Box<Self>>, span: Span },
    Block { exprs: Vec<Self>, span: Span },
    Call { callee: Box<Self>, args: Vec<CallArg>, span: Span },
    Unary { expr: Box<Self>, op: UnOp, span: Span },
    Binary { lhs: Box<Self>, rhs: Box<Self>, op: BinOp, span: Span },
    Cast { expr: Box<Self>, ty_expr: TyExpr, span: Span },
    Member { expr: Box<Self>, member: Word, span: Span },
    Name { word: Word, args: Option<Vec<TyExpr>>, span: Span },
    Lit { kind: LitKind, span: Span },
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Item(x) => x.span(),
            Self::Name { span, .. }
            | Self::Member { span, .. }
            | Self::Return { span, .. }
            | Self::If { span, .. }
            | Self::Block { span, .. }
            | Self::Call { span, .. }
            | Self::Unary { span, .. }
            | Self::Binary { span, .. }
            | Self::Cast { span, .. }
            | Self::Lit { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub attrs: Attrs,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FnKind {
    Bare { body: Box<Expr> },
    Extern,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub word: Word,
    pub ty_params: Vec<TyParam>,
    pub params: Vec<FnParam>,
    pub ret: Option<TyExpr>,
}

#[derive(Debug, Clone)]
pub struct TyParam {
    pub name: Word,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: Word,
    pub ty_expr: TyExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub attrs: Attrs,
    pub pat: Pat,
    pub ty_expr: Option<TyExpr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternLet {
    pub attrs: Attrs,
    pub word: Word,
    pub ty_expr: TyExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Name(NamePat),
    Discard(Span),
}

impl Pat {
    pub fn walk(&self, mut f: impl FnMut(&NamePat)) {
        self.walk_(&mut f);
    }

    fn walk_(&self, f: &mut impl FnMut(&NamePat)) {
        match self {
            Self::Name(n) => f(n),
            Self::Discard(_) => (),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamePat {
    pub word: Word,
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Name(n) => n.word.fmt(f),
            Pat::Discard(_) => f.write_str("_"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CallArg {
    Positional(Expr),
    Named(Word, Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Str(Ustr),
    Int(u128),
    Bool(bool),
}

pub type Attrs = Vec<Attr>;

#[derive(Debug, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttrKind {
    Link,
}

impl TryFrom<&str> for AttrKind {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "link" => Ok(Self::Link),
            _ => Err(()),
        }
    }
}

impl fmt::Display for AttrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            AttrKind::Link => "lib",
        })
    }
}
