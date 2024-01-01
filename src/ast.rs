mod pretty_print;
pub mod token;

use std::{fmt, io};

use camino::Utf8PathBuf;
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::middle::IsUfcs;
use crate::{
    data_structures::index_vec::{new_key_type, IndexVec},
    db::{ExternLib, ModuleId, StructKind},
    middle::{BinOp, Mutability, Pat, TyExpr, UnOp, Vis},
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
            id: ModuleId::INVALID,
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

new_key_type!(ItemId);

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

pub type ItemMap<T> = FxHashMap<GlobalItemId, T>;

#[derive(Debug, Clone)]
pub enum Item {
    Fn(Fn),
    Let(Let),
    Type(TyDef),
    Import(Import),
    ExternLet(ExternLet),
    ExternImport(ExternImport),
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Let(Let),
    Assign {
        lhs: Box<Self>,
        rhs: Box<Self>,
        op: Option<BinOp>,
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
        ty_args: Option<Vec<TyExpr>>,
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
    Name {
        word: Word,
        ty_args: Option<Vec<TyExpr>>,
        span: Span,
    },
    Lit {
        kind: LitKind,
        span: Span,
    },
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Let(Let { span, .. })
            | Self::Assign { span, .. }
            | Self::Name { span, .. }
            | Self::Field { span, .. }
            | Self::Return { span, .. }
            | Self::If { span, .. }
            | Self::Loop { span, .. }
            | Self::Break { span, .. }
            | Self::Block { span, .. }
            | Self::Call { span, .. }
            | Self::MethodCall { span, .. }
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
    pub vis: Vis,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FnKind {
    Bare { body: Box<Expr> },
    Extern { is_c_variadic: bool },
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
pub struct TyDef {
    pub attrs: Attrs,
    pub word: Word,
    pub vis: Vis,
    pub kind: TyDefKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyDefKind {
    Struct(StructTyDef),
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
pub struct Import {
    pub attrs: Attrs,
    pub path: Utf8PathBuf,
    pub root: ImportName,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ImportName {
    pub word: Word,
    pub vis: Vis,
    pub alias: Option<Word>,
    pub node: Option<ImportNode>,
}

impl ImportName {
    pub fn name(&self) -> Word {
        self.alias.unwrap_or(self.word)
    }
}

impl Spanned for ImportName {
    fn span(&self) -> Span {
        self.name().span()
    }
}

#[derive(Debug, Clone)]
pub enum ImportNode {
    Name(Box<ImportName>),
    Group(Vec<ImportNode>),
    Glob(IsUfcs, Span),
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

#[derive(Debug, Clone, PartialEq)]
pub enum LitKind {
    Bool(bool),
    Int(u128),
    Float(f64),
    Str(Ustr),
}

pub type Attrs = Vec<Attr>;

#[derive(Debug, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttrKind {}

impl TryFrom<&str> for AttrKind {
    type Error = ();

    fn try_from(_value: &str) -> Result<Self, Self::Error> {
        Err(())
    }
}

impl fmt::Display for AttrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // f.write_str(match self {})
        f.write_str("")
    }
}
