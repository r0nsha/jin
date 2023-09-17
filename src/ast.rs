mod pretty_print;
pub mod token;

use std::{fmt, io};

use enum_as_inner::EnumAsInner;
use ustr::Ustr;

use crate::{
    ast::token::TokenKind,
    common::{QPath, Word},
    db::{DefId, ModuleId},
    span::{SourceId, Span, Spanned},
};

#[derive(Debug, Clone)]
pub struct Ast {
    pub modules: Vec<Module>,
}

impl Ast {
    pub fn new() -> Self {
        Self { modules: vec![] }
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
    pub items: Vec<Item>,
    is_main: bool,
}

impl Module {
    pub fn new(source_id: SourceId, name: QPath, is_main: bool) -> Self {
        Self { id: None, source: source_id, name, is_main, items: vec![] }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Item {
    Fn(Fn),
    Let(Let),
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Self::Fn(Fn { span, .. }) | Self::Let(Let { span, .. }) => *span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Fn(Fn { span, .. }) | Self::Let(Let { span, .. }) => span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Item(Item),
    Return(Return),
    If(If),
    Block(Block),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Cast(Cast),
    MemberAccess(MemberAccess),
    Name(Name),
    Lit(Lit),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Item(x) => x.span(),
            Self::Name(Name { span, .. })
            | Self::MemberAccess(MemberAccess { span, .. })
            | Self::Return(Return { span, .. })
            | Self::If(If { span, .. })
            | Self::Block(Block { span, .. })
            | Self::Call(Call { span, .. })
            | Self::Unary(Unary { span, .. })
            | Self::Binary(Binary { span, .. })
            | Self::Cast(Cast { span, .. })
            | Self::Lit(Lit { span, .. }) => *span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Item(x) => x.span_mut(),
            Self::Return(x) => &mut x.span,
            Self::If(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::Unary(x) => &mut x.span,
            Self::Binary(x) => &mut x.span,
            Self::Cast(x) => &mut x.span,
            Self::MemberAccess(x) => &mut x.span,
            Self::Name(x) => &mut x.span,
            Self::Lit(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: Option<DefId>,
    pub attrs: Attrs,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FnKind {
    Bare { body: Block },
    Extern,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub name: Word,
    pub ty_params: Vec<TyParam>,
    pub params: Vec<FnParam>,
    pub ret: Option<Ty>,
}

#[derive(Debug, Clone)]
pub struct TyParam {
    pub id: Option<DefId>,
    pub name: Word,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub id: Option<DefId>,
    pub name: Word,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub pat: Pat,
    pub ty_annot: Option<Ty>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Name(NamePat),
    Ignore(Span),
}

impl Pat {
    pub fn walk(&self, mut f: impl FnMut(&NamePat)) {
        self.walk_(&mut f);
    }

    fn walk_(&self, f: &mut impl FnMut(&NamePat)) {
        match self {
            Pat::Name(n) => f(n),
            Pat::Ignore(_) => (),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamePat {
    pub id: Option<DefId>,
    pub word: Word,
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Name(n) => n.word.fmt(f),
            Pat::Ignore(_) => f.write_str("_"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<CallArg>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum CallArg {
    Positional(Expr),
    Named(Word, Expr),
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOp,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Cmp(CmpOp),
}

impl BinOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::And => "&&",
            Self::Or => "||",
            Self::Cmp(CmpOp::Eq) => "==",
            Self::Cmp(CmpOp::Ne) => "!=",
            Self::Cmp(CmpOp::Lt) => "<",
            Self::Cmp(CmpOp::Le) => "<=",
            Self::Cmp(CmpOp::Gt) => ">",
            Self::Cmp(CmpOp::Ge) => ">=",
        }
    }

    #[allow(unused)]
    pub fn name(self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Rem => "mod",
            Self::Shl => "shl",
            Self::Shr => "shr",
            Self::BitAnd => "bitand",
            Self::BitOr => "bitor",
            Self::BitXor => "bitxor",
            Self::And => "and",
            Self::Or => "or",
            Self::Cmp(CmpOp::Eq) => "eq",
            Self::Cmp(CmpOp::Ne) => "ne",
            Self::Cmp(CmpOp::Lt) => "lt",
            Self::Cmp(CmpOp::Le) => "le",
            Self::Cmp(CmpOp::Gt) => "gt",
            Self::Cmp(CmpOp::Ge) => "ge",
        }
    }

    pub fn precedence(self) -> usize {
        match self {
            Self::Mul | Self::Div | Self::Rem => 11,
            Self::Add | Self::Sub => 10,
            Self::Shl | Self::Shr => 9,
            Self::BitAnd => 8,
            Self::BitXor => 7,
            Self::BitOr => 6,
            Self::Cmp(CmpOp::Eq | CmpOp::Ne) => 5,
            Self::Cmp(CmpOp::Lt | CmpOp::Le | CmpOp::Gt | CmpOp::Ge) => 4,
            Self::And => 3,
            Self::Or => 2,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub expr: Box<Expr>,
    pub op: UnOp,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl UnOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl TryFrom<TokenKind> for BinOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        let op = match value {
            TokenKind::EqEq => Self::Cmp(CmpOp::Eq),
            TokenKind::BangEq => Self::Cmp(CmpOp::Ne),
            TokenKind::Star => Self::Mul,
            TokenKind::FwSlash => Self::Div,
            TokenKind::Percent => Self::Rem,
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Lt => Self::Cmp(CmpOp::Lt),
            TokenKind::LtLt => Self::Shl,
            TokenKind::LtEq => Self::Cmp(CmpOp::Le),
            TokenKind::Gt => Self::Cmp(CmpOp::Gt),
            TokenKind::GtGt => Self::Shr,
            TokenKind::GtEq => Self::Cmp(CmpOp::Ge),
            TokenKind::Amp => Self::BitAnd,
            TokenKind::AmpAmp => Self::And,
            TokenKind::Caret => Self::BitXor,
            TokenKind::Pipe => Self::BitOr,
            TokenKind::PipePipe => Self::Or,
            _ => return Err(()),
        };

        Ok(op)
    }
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub expr: Box<Expr>,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MemberAccess {
    pub expr: Box<Expr>,
    pub member: Word,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: Option<DefId>,
    pub word: Word,
    pub args: Option<Vec<Ty>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Str(Ustr),
    Int(u128),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Ty {
    RawPtr(Box<Ty>, Span),
    Name(TyName),
    Unit(Span),
    Infer(Span),
}

impl Spanned for Ty {
    fn span(&self) -> Span {
        match self {
            Self::RawPtr(_, span)
            | Self::Name(TyName { span, .. })
            | Self::Unit(span)
            | Self::Infer(span) => *span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Ty::Name(x) => &mut x.span,
            Self::RawPtr(_, span) | Ty::Unit(span) | Ty::Infer(span) => span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyName {
    pub id: Option<DefId>,
    pub word: Word,
    pub args: Vec<Ty>,
    pub span: Span,
}

pub type Attrs = Vec<Attr>;

#[derive(Debug, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttrKind {
    Lib,
}

impl TryFrom<&str> for AttrKind {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "lib" => Ok(Self::Lib),
            value => Err(()),
        }
    }
}

impl fmt::Display for AttrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            AttrKind::Lib => "lib",
        })
    }
}
