use std::{
    fmt::{self, Write},
    mem,
};

use ustr::{ustr, Ustr};

use crate::span::Span;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn as_ident(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(ident) => ident,
            kind => panic!("expected Ident, got {kind:?}"),
        }
    }

    pub fn kind_is(&self, other: TokenKind) -> bool {
        mem::discriminant(&self.kind) == mem::discriminant(&other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Delimiters
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Comma,

    // Symbols
    Eq,

    // Ident & Keywords
    Ident(Ustr),
    Fn,
    Return,

    // Values
    Int(usize),
}

impl TokenKind {
    #[inline]
    pub fn empty_ident() -> Self {
        Self::Ident(ustr(""))
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OpenParen => f.write_char('('),
            Self::CloseParen => f.write_char(')'),
            Self::OpenCurly => f.write_char('{'),
            Self::CloseCurly => f.write_char('}'),
            Self::Comma => f.write_char(','),
            Self::Eq => f.write_char('='),
            Self::Ident(_) => f.write_str("identifier"),
            Self::Fn => f.write_str("`fn`"),
            Self::Return => f.write_str("`return`"),
            Self::Int(_) => f.write_str("int literal"),
        }
    }
}
