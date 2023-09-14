use std::{
    fmt::{self, Write},
    mem,
};

use ustr::{ustr, Ustr};

use crate::{common::Word, span::Span};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn ident(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(ident) => ident,
            kind => panic!("expected Ident, found {kind:?}"),
        }
    }

    pub fn spanned_word(&self) -> Word {
        Word::new(self.ident(), self.span)
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
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Comma,
    Dot,
    Colon,

    // Operators
    Eq,
    EqEq,
    Bang,
    BangEq,
    Star,
    FwSlash,
    Percent,
    Plus,
    Minus,
    Lt,
    LtLt,
    LtEq,
    Gt,
    GtGt,
    GtEq,
    Amp,
    AmpAmp,
    Caret,
    Pipe,
    PipePipe,

    // Ident & Keywords
    Ident(Ustr),
    Underscore,
    Return,
    Fn,
    Let,
    If,
    Else,
    True,
    False,
    As,

    // Literals
    Int(u128),
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
            Self::OpenBracket => f.write_char('['),
            Self::CloseBracket => f.write_char(']'),
            Self::OpenCurly => f.write_char('{'),
            Self::CloseCurly => f.write_char('}'),
            Self::Comma => f.write_char(','),
            Self::Dot => f.write_char('.'),
            Self::Colon => f.write_char(':'),
            Self::Eq => f.write_char('='),
            Self::EqEq => f.write_str("=="),
            Self::Bang => f.write_str("!"),
            Self::BangEq => f.write_str("!="),
            Self::Star => f.write_str("*"),
            Self::FwSlash => f.write_str("/"),
            Self::Percent => f.write_str("%"),
            Self::Plus => f.write_str("+"),
            Self::Minus => f.write_str("-"),
            Self::Lt => f.write_str("<"),
            Self::LtLt => f.write_str("<<"),
            Self::LtEq => f.write_str("<="),
            Self::Gt => f.write_str(">"),
            Self::GtGt => f.write_str(">>"),
            Self::GtEq => f.write_str(">="),
            Self::Amp => f.write_str("&"),
            Self::AmpAmp => f.write_str("&&"),
            Self::Caret => f.write_str("^"),
            Self::Pipe => f.write_str("|"),
            Self::PipePipe => f.write_str("||"),
            Self::Ident(..) => f.write_str("identifier"),
            Self::Underscore => f.write_str("_"),
            Self::Return => f.write_str("`return`"),
            Self::If => f.write_str("`if`"),
            Self::Else => f.write_str("`else`"),
            Self::Fn => f.write_str("`fn`"),
            Self::Let => f.write_str("`let`"),
            Self::True => f.write_str("`true`"),
            Self::False => f.write_str("`false`"),
            Self::As => f.write_str("`as`"),
            Self::Int(..) => f.write_str("int literal"),
        }
    }
}
