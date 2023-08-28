mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
pub use lower::lower;

use crate::{
    ast::BinOp,
    common::Word,
    db::{Db, SymbolId},
    span::{Span, Spanned},
    ty::{Ty, Typed},
};

#[derive(Debug, Clone)]
pub struct TypedAst {
    pub items: Vec<Item>,
}

impl TypedAst {
    pub fn pretty_print(&self, db: &Db) -> io::Result<()> {
        println!();
        pretty_print::print(db, self)?;
        println!();
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub ty: Ty,
}

impl Spanned for Item {
    fn span(&self) -> Span {
        self.kind.span()
    }

    fn span_mut(&mut self) -> &mut Span {
        self.kind.span_mut()
    }
}

impl Typed for Item {
    fn ty(&self) -> Ty {
        self.kind.ty()
    }

    fn ty_mut(&mut self) -> &mut Ty {
        self.kind.ty_mut()
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ItemKind {
    Function(Fn),
}

impl Spanned for ItemKind {
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

impl Typed for ItemKind {
    fn ty(&self) -> Ty {
        match self {
            Self::Function(x) => x.ty,
        }
    }

    fn ty_mut(&mut self) -> &mut Ty {
        match self {
            Self::Function(x) => &mut x.ty,
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Expr {
    Item(Item),
    If(If),
    Block(Block),
    Return(Return),
    Call(Call),
    Bin(Bin),
    Name(Name),
    Lit(Lit),
}

impl Typed for Expr {
    fn ty(&self) -> Ty {
        match self {
            Self::Item(x) => x.ty(),
            Self::If(x) => x.ty,
            Self::Block(x) => x.ty,
            Self::Return(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Bin(x) => x.ty,
            Self::Name(x) => x.ty,
            Self::Lit(x) => x.ty,
        }
    }

    fn ty_mut(&mut self) -> &mut Ty {
        match self {
            Self::Item(x) => x.ty_mut(),
            Self::If(x) => &mut x.ty,
            Self::Block(x) => &mut x.ty,
            Self::Return(x) => &mut x.ty,
            Self::Call(x) => &mut x.ty,
            Self::Bin(x) => &mut x.ty,
            Self::Name(x) => &mut x.ty,
            Self::Lit(x) => &mut x.ty,
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Item(x) => x.span(),
            Self::If(x) => x.span,
            Self::Block(x) => x.span,
            Self::Return(x) => x.span,
            Self::Call(x) => x.span,
            Self::Bin(x) => x.span,
            Self::Name(x) => x.span,
            Self::Lit(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Item(x) => x.span_mut(),
            Self::If(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Return(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::Bin(x) => &mut x.span,
            Self::Name(x) => &mut x.span,
            Self::Lit(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: SymbolId,
    pub sig: FnSig,
    pub body: Block,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub params: Vec<FnParam>,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub id: SymbolId,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Box<Expr>,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<CallArg>,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct CallArg {
    pub name: Option<Word>,
    pub expr: Expr,
    pub index: Option<usize>,
}

impl Typed for CallArg {
    fn ty(&self) -> Ty {
        self.expr.ty()
    }

    fn ty_mut(&mut self) -> &mut Ty {
        self.expr.ty_mut()
    }
}

// #[derive(Debug, Clone)]
// pub struct CallArg {
//     Positional(Expr),
//     Named(Word, Expr),
// }
//
// impl Typed for CallArg {
//     fn ty(&self) -> Ty {
//         match self {
//             Self::Positional(e) | Self::Named(_, e) => e.ty(),
//         }
//     }
//
//     fn ty_mut(&mut self) -> &mut Ty {
//         match self {
//             Self::Positional(e) | Self::Named(_, e) => e.ty_mut(),
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct Bin {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOp,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: SymbolId,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum LitKind {
    Int(usize),
    Bool(bool),
    Unit,
}
