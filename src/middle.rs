use std::fmt;

use ustr::Ustr;

use crate::{
    ast::token::TokenKind,
    db::DefId,
    span::{Span, Spanned},
    word::Word,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Imm,
    Mut,
}

impl Mutability {
    pub fn is_imm(self) -> bool {
        matches!(self, Mutability::Imm)
    }

    pub fn is_mut(self) -> bool {
        matches!(self, Mutability::Mut)
    }
}

impl Default for Mutability {
    fn default() -> Self {
        Self::Imm
    }
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
            Self::Mul | Self::Div | Self::Rem => 7,
            Self::Add | Self::Sub => 6,
            Self::Shl | Self::Shr => 5,
            Self::BitAnd | Self::BitXor | Self::BitOr => 4,
            Self::Cmp(..) => 3,
            Self::And => 2,
            Self::Or => 1,
        }
    }

    pub fn from_assign_op(tk: TokenKind) -> Option<Self> {
        let op = match tk {
            TokenKind::StarEq => Self::Mul,
            TokenKind::FwSlashEq => Self::Div,
            TokenKind::PercentEq => Self::Rem,
            TokenKind::PlusEq => Self::Add,
            TokenKind::MinusEq => Self::Sub,
            TokenKind::LtLtEq => Self::Shl,
            TokenKind::GtGtEq => Self::Shr,
            TokenKind::AmpEq => Self::BitAnd,
            TokenKind::CaretEq => Self::BitXor,
            TokenKind::PipeEq => Self::BitOr,
            _ => return None,
        };

        Some(op)
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

#[derive(Debug, Clone)]
pub enum TyExpr {
    Fn(TyExprFn),
    RawPtr(Box<TyExpr>, Span),
    Name(TyExprName),
    Hole(Span),
}

impl Spanned for TyExpr {
    fn span(&self) -> Span {
        match self {
            Self::Fn(TyExprFn { span, .. })
            | Self::RawPtr(_, span)
            | Self::Name(TyExprName { span, .. })
            | Self::Hole(span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyExprFn {
    pub params: Vec<TyExpr>,
    pub ret: Box<TyExpr>,
    pub is_c_variadic: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TyExprName {
    pub word: Word,
    pub ty_args: Vec<TyExpr>,
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

    pub fn any(&self, mut f: impl FnMut(&NamePat) -> bool) -> bool {
        self.any_(&mut f)
    }

    fn any_(&self, f: &mut impl FnMut(&NamePat) -> bool) -> bool {
        match self {
            Self::Name(n) => f(n),
            Self::Discard(_) => false,
        }
    }

    pub fn word(&self) -> Option<Word> {
        match self {
            Pat::Name(n) => Some(n.word),
            Pat::Discard(_) => None,
        }
    }

    pub fn name(&self) -> Option<Ustr> {
        self.word().map(|w| w.name())
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Name(n) => n.word.fmt(f),
            Pat::Discard(_) => f.write_str("_"),
        }
    }
}

impl Spanned for Pat {
    fn span(&self) -> Span {
        match self {
            Pat::Name(n) => n.span(),
            Pat::Discard(span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamePat {
    pub id: DefId,
    pub word: Word,
    pub vis: Vis,
    pub mutability: Mutability,
}

impl Spanned for NamePat {
    fn span(&self) -> Span {
        self.word.span()
    }
}
