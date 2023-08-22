mod pretty_print;
pub mod token;

use std::{fmt, io};

use crate::{
    ast::token::TokenKind,
    common::{QName, Word},
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

    pub fn pretty_print(&self) -> io::Result<()> {
        println!();
        println!("AST:");
        println!();

        for module in &self.modules {
            pretty_print::print_module(module)?;
        }

        println!();

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub source: SourceId,
    pub name: QName,
    pub items: Vec<Item>,
    is_main: bool,
}

impl Module {
    pub fn new(source_id: SourceId, name: QName, is_main: bool) -> Self {
        Self { source: source_id, name, is_main, items: vec![] }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self {
            Self::Function(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Function(x) => &mut x.span,
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
    Binary(Binary),
    Name(Name),
    Lit(Lit),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Item(x) => x.span(),
            Self::Return(x) => x.span,
            Self::If(x) => x.span,
            Self::Block(x) => x.span,
            Self::Call(x) => x.span,
            Self::Binary(x) => x.span,
            Self::Name(x) => x.name.span(),
            Self::Lit(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Item(x) => x.span_mut(),
            Self::Return(x) => &mut x.span,
            Self::If(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::Binary(x) => &mut x.span,
            Self::Name(x) => x.name.span_mut(),
            Self::Lit(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub sig: FunctionSig,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub name: Word,
    pub params: Vec<FunctionParam>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Word,
    pub span: Span,
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
    pub op: BinaryOp,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Cmp(CmpOp),
}

impl BinaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
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

    pub fn name(self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Mod => "mod",
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
            Self::Mul | Self::Div | Self::Mod => 11,
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        let op = match value {
            TokenKind::EqEq => Self::Cmp(CmpOp::Eq),
            TokenKind::BangEq => Self::Cmp(CmpOp::Ne),
            TokenKind::Star => Self::Mul,
            TokenKind::FwSlash => Self::Div,
            TokenKind::Percent => Self::Mod,
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
pub struct Name {
    pub name: Word,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
    Bool(bool),
    Unit,
}
