mod pretty_print;
pub mod token;

use std::{fmt, io};

use camino::Utf8PathBuf;
use data_structures::{
    index_vec::{IndexVec, Key as _},
    new_key_type,
};
use ustr::Ustr;

use crate::{
    db::{ExternLib, ModuleId, StructKind},
    middle::{BinOp, CallConv, IsUfcs, Mutability, Pat, TyExpr, UnOp, Vis},
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

    pub fn find_item(&self, id: GlobalItemId) -> Option<&Item> {
        self.modules.get(id.module_id).and_then(|m| m.items.get(id.item_id))
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
    pub id: ModuleId,
    pub source: SourceId,
    pub name: QPath,
    pub items: IndexVec<ItemId, Item>,
    is_main: bool,
}

impl Module {
    pub fn new(source_id: SourceId, name: QPath, is_main: bool) -> Self {
        Self {
            id: ModuleId::null(),
            source: source_id,
            name,
            is_main,
            items: IndexVec::new(),
        }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

new_key_type! {
    pub struct ItemId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalItemId {
    pub module_id: ModuleId,
    pub item_id: ItemId,
}

impl GlobalItemId {
    pub fn new(module_id: ModuleId, item_id: ItemId) -> Self {
        Self { module_id, item_id }
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Fn(Fn),
    Let(Let),
    Type(TyDef),
    Import(Import),
    ExternLet(ExternLet),
    ExternImport(ExternImport),
    Assoc(Word, Box<Self>),
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Self::Fn(Fn { span, .. })
            | Self::Let(Let { span, .. })
            | Self::Type(TyDef { span, .. })
            | Self::Import(Import { span, .. })
            | Self::ExternLet(ExternLet { span, .. })
            | Self::ExternImport(ExternImport { span, .. }) => *span,
            Self::Assoc(name, item) => name.span().merge(item.span()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Let(Let),
    Fn {
        params: Vec<FnParam>,
        ret: Option<TyExpr>,
        body: Box<Self>,
        span: Span,
    },
    Assign {
        lhs: Box<Self>,
        rhs: Box<Self>,
        op: Option<BinOp>,
        span: Span,
    },
    Swap {
        lhs: Box<Self>,
        rhs: Box<Self>,
        span: Span,
    },
    Return {
        expr: Option<Box<Self>>,
        span: Span,
    },
    If {
        cond: Box<Self>,
        then: Box<Self>,
        otherwise: Option<Box<Self>>,
        span: Span,
    },
    Match {
        expr: Box<Self>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Loop {
        cond: Option<Box<Self>>,
        expr: Box<Self>,
        span: Span,
    },
    Break {
        span: Span,
    },
    Block {
        exprs: Vec<Self>,
        span: Span,
    },
    Call {
        callee: Box<Self>,
        args: Vec<CallArg>,
        span: Span,
    },
    MethodCall {
        expr: Box<Self>,
        method: Word,
        targs: Option<Vec<TyExpr>>,
        args: Vec<CallArg>,
        span: Span,
    },
    Unary {
        expr: Box<Self>,
        op: UnOp,
        span: Span,
    },
    Binary {
        lhs: Box<Self>,
        rhs: Box<Self>,
        op: BinOp,
        span: Span,
    },
    Cast {
        expr: Box<Self>,
        ty_expr: TyExpr,
        span: Span,
    },
    Field {
        expr: Box<Self>,
        field: Word,
        span: Span,
    },
    Index {
        expr: Box<Self>,
        index: Box<Self>,
        span: Span,
    },
    Name {
        word: Word,
        targs: Option<Vec<TyExpr>>,
        span: Span,
    },
    SliceLit {
        exprs: Vec<Self>,
        span: Span,
    },
    SliceLitCap {
        cap: Box<Self>,
        span: Span,
    },
    BoolLit {
        value: bool,
        span: Span,
    },
    IntLit {
        value: u128,
        span: Span,
    },
    FloatLit {
        value: f64,
        span: Span,
    },
    StrLit {
        value: Ustr,
        span: Span,
    },
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Let(Let { span, .. })
            | Self::Fn { span, .. }
            | Self::Assign { span, .. }
            | Self::Swap { span, .. }
            | Self::Name { span, .. }
            | Self::Field { span, .. }
            | Self::Index { span, .. }
            | Self::Return { span, .. }
            | Self::If { span, .. }
            | Self::Match { span, .. }
            | Self::Loop { span, .. }
            | Self::Break { span, .. }
            | Self::Block { span, .. }
            | Self::Call { span, .. }
            | Self::MethodCall { span, .. }
            | Self::Unary { span, .. }
            | Self::Binary { span, .. }
            | Self::Cast { span, .. }
            | Self::SliceLit { span, .. }
            | Self::SliceLitCap { span, .. }
            | Self::BoolLit { span, .. }
            | Self::IntLit { span, .. }
            | Self::FloatLit { span, .. }
            | Self::StrLit { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub attrs: Attrs,
    pub vis: Vis,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FnKind {
    Bare { body: Box<Expr> },
    Extern { callconv: CallConv, is_c_variadic: bool },
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
    pub word: Word,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub pat: Pat,
    pub ty_expr: Option<TyExpr>,
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
pub struct TyDef {
    pub attrs: Attrs,
    pub word: Word,
    pub vis: Vis,
    pub ty_params: Vec<TyParam>,
    pub kind: TyDefKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyDefKind {
    Struct(StructTyDef),
    Union(UnionTyDef),
}

#[derive(Debug, Clone)]
pub struct StructTyDef {
    pub kind: StructKind,
    pub fields: Vec<StructTyField>,
}

#[derive(Debug, Clone)]
pub struct StructTyField {
    pub name: Word,
    pub vis: Vis,
    pub ty_expr: TyExpr,
}

#[derive(Debug, Clone)]
pub struct UnionTyDef {
    pub variants: Vec<UnionVariant>,
}

#[derive(Debug, Clone)]
pub struct UnionVariant {
    pub name: Word,
    pub fields: Vec<UnionVariantField>,
}

#[derive(Debug, Clone)]
pub struct UnionVariantField {
    pub name: Word,
    pub ty_expr: TyExpr,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub attrs: Attrs,
    pub module_path: Utf8PathBuf,
    pub path: Vec<Word>,
    pub kind: ImportKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    Qualified(Option<Word>, Vis),
    Unqualified(Vec<UnqualifiedImport>),
}

#[derive(Debug, Clone)]
pub enum UnqualifiedImport {
    Name(Word, Option<Word>, Vis),
    Glob(IsUfcs, Span),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pat: MatchPat,
    pub guard: Option<Box<Expr>>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum MatchPat {
    Name(Word, Mutability),
    Wildcard(Span),
    Unit(Span),
    Bool(bool, Span),
    Int(i128, Span),
    Str(Ustr, Span),
    Adt(MatchPatAdt),
    Or(Vec<Self>, Span),
}

impl Spanned for MatchPat {
    fn span(&self) -> Span {
        match self {
            Self::Name(w, _) => w.span(),
            Self::Wildcard(span)
            | Self::Unit(span)
            | Self::Bool(_, span)
            | Self::Int(_, span)
            | Self::Str(_, span)
            | Self::Adt(MatchPatAdt { span, .. })
            | Self::Or(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchPatAdt {
    pub path: Vec<Word>,
    pub subpats: Vec<Subpat>,
    pub is_exhaustive: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Subpat {
    Positional(MatchPat),
    Named(Word, MatchPat),
}

impl Spanned for Subpat {
    fn span(&self) -> Span {
        match self {
            Subpat::Positional(p) => p.span(),
            Subpat::Named(n, p) => n.span().merge(p.span()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternLet {
    pub attrs: Attrs,
    pub word: Word,
    pub vis: Vis,
    pub mutability: Mutability,
    pub ty_expr: TyExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternImport {
    pub attrs: Attrs,
    pub lib: ExternLib,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum CallArg {
    Positional(Expr),
    Named(Word, Expr),
}

#[derive(Debug, Clone)]
pub struct Attrs(Vec<Attr>);

impl Attrs {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, attr: Attr) {
        self.0.push(attr);
    }

    pub fn find(&self, kind: AttrKind) -> Option<&Attr> {
        self.iter().find(|a| a.kind == kind)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Attr> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttrKind {
    Intrinsic,
}

impl TryFrom<&str> for AttrKind {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "intrinsic" => Ok(Self::Intrinsic),
            _ => Err(()),
        }
    }
}

impl fmt::Display for AttrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // f.write_str(match self {})
        f.write_str("")
    }
}
