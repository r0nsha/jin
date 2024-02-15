use std::{
    fmt::{self, Write},
    mem,
};

use ustr::{ustr, Ustr};

use crate::{span::Span, word::Word};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[track_caller]
    pub fn str_value(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(v) | TokenKind::Int(v) | TokenKind::Float(v) | TokenKind::Str(v) => v,
            kind => panic!("expected Ident or Str, found {kind:?}"),
        }
    }

    pub fn word(&self) -> Word {
        Word::new(self.str_value(), self.span)
    }

    #[inline]
    pub fn kind_is(&self, other: TokenKind) -> bool {
        mem::discriminant(&self.kind) == mem::discriminant(&other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Punctuation
    Comma,
    Dot,
    DotDot,
    Colon,
    Semi(bool),
    At,
    Arrow,
    QuestionMark,

    // Delimiters
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,

    // Operators
    Eq,
    EqEq,
    Bang,
    BangEq,
    Star,
    StarEq,
    FwSlash,
    FwSlashEq,
    Percent,
    PercentEq,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Lt,
    LtEq,
    LtLt,
    LtLtEq,
    Gt,
    GtEq,
    GtGt,
    GtGtEq,
    Amp,
    AmpEq,
    AmpAmp,
    Caret,
    CaretEq,
    Pipe,
    PipeEq,
    PipePipe,
    Walrus,

    // Ident & Keywords
    Ident(Ustr),
    Underscore,
    Return,
    Fn,
    Let,
    Type,
    Extern,
    If,
    Else,
    Match,
    True,
    False,
    As,
    Import,
    For,
    Break,
    Mut,
    Imm,
    Transmute,
    Ref,
    Move,
    Unsafe,
    Pub,

    // Literals
    Int(Ustr),
    Float(Ustr),
    Str(Ustr),
}

impl TokenKind {
    #[inline]
    pub fn empty_ident() -> Self {
        Self::Ident(ustr(""))
    }

    #[inline]
    pub fn empty_str() -> Self {
        Self::Str(ustr(""))
    }

    // Whether this token can come before a semicolon
    #[inline]
    pub fn is_before_semi(self) -> bool {
        match self {
            TokenKind::Return
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Break
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Str(_)
            | TokenKind::Ident(_)
            | TokenKind::Underscore
            | TokenKind::QuestionMark
            | TokenKind::CloseParen
            | TokenKind::CloseBracket
            | TokenKind::CloseCurly
            | TokenKind::Star
            | TokenKind::Semi(_) => true,

            TokenKind::Fn
            | TokenKind::Let
            | TokenKind::Type
            | TokenKind::Extern
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Match
            | TokenKind::As
            | TokenKind::Import
            | TokenKind::For
            | TokenKind::Mut
            | TokenKind::Imm
            | TokenKind::Transmute
            | TokenKind::Ref
            | TokenKind::Move
            | TokenKind::Unsafe
            | TokenKind::Pub
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Colon
            | TokenKind::Arrow
            | TokenKind::Comma
            | TokenKind::OpenParen
            | TokenKind::OpenBracket
            | TokenKind::OpenCurly
            | TokenKind::Eq
            | TokenKind::EqEq
            | TokenKind::Bang
            | TokenKind::BangEq
            | TokenKind::StarEq
            | TokenKind::FwSlash
            | TokenKind::FwSlashEq
            | TokenKind::Percent
            | TokenKind::PercentEq
            | TokenKind::Plus
            | TokenKind::PlusEq
            | TokenKind::Minus
            | TokenKind::MinusEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::LtLt
            | TokenKind::LtLtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::GtGt
            | TokenKind::GtGtEq
            | TokenKind::Amp
            | TokenKind::AmpEq
            | TokenKind::AmpAmp
            | TokenKind::Caret
            | TokenKind::CaretEq
            | TokenKind::Pipe
            | TokenKind::PipeEq
            | TokenKind::PipePipe
            | TokenKind::Walrus
            | TokenKind::At => false,
        }
    }

    // Whether this token can come after a semicolon
    #[inline]
    pub fn is_after_semi(self) -> bool {
        match self {
            TokenKind::Return
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Break
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Str(_)
            | TokenKind::Ident(_)
            | TokenKind::Underscore
            | TokenKind::Semi(_)
            | TokenKind::Fn
            | TokenKind::Let
            | TokenKind::Type
            | TokenKind::Extern
            | TokenKind::If
            | TokenKind::Match
            | TokenKind::Import
            | TokenKind::For
            | TokenKind::Transmute
            | TokenKind::Unsafe
            | TokenKind::Pub
            | TokenKind::OpenParen
            | TokenKind::OpenBracket
            | TokenKind::OpenCurly
            | TokenKind::CloseCurly
            | TokenKind::Bang
            | TokenKind::Minus
            | TokenKind::Amp
            | TokenKind::At => true,

            TokenKind::QuestionMark
            | TokenKind::CloseParen
            | TokenKind::CloseBracket
            | TokenKind::Star
            | TokenKind::Else
            | TokenKind::As
            | TokenKind::Mut
            | TokenKind::Imm
            | TokenKind::Ref
            | TokenKind::Move
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Colon
            | TokenKind::Arrow
            | TokenKind::Comma
            | TokenKind::Eq
            | TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::StarEq
            | TokenKind::FwSlash
            | TokenKind::FwSlashEq
            | TokenKind::Percent
            | TokenKind::PercentEq
            | TokenKind::Plus
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::LtLt
            | TokenKind::LtLtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::GtGt
            | TokenKind::GtGtEq
            | TokenKind::AmpEq
            | TokenKind::AmpAmp
            | TokenKind::Caret
            | TokenKind::CaretEq
            | TokenKind::Pipe
            | TokenKind::PipeEq
            | TokenKind::PipePipe
            | TokenKind::Walrus => false,
        }
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
            Self::DotDot => f.write_str(".."),
            Self::Colon => f.write_char(':'),
            Self::Semi(auto) => {
                if *auto {
                    f.write_str("automatically inserted ;")
                } else {
                    f.write_char(';')
                }
            }
            Self::At => f.write_char('@'),
            Self::Arrow => f.write_str("->"),
            Self::QuestionMark => f.write_char('?'),
            Self::Eq => f.write_char('='),
            Self::EqEq => f.write_str("=="),
            Self::Bang => f.write_str("!"),
            Self::BangEq => f.write_str("!="),
            Self::Star => f.write_str("*"),
            Self::StarEq => f.write_str("*="),
            Self::FwSlash => f.write_str("/"),
            Self::FwSlashEq => f.write_str("/="),
            Self::Percent => f.write_str("%"),
            Self::PercentEq => f.write_str("%="),
            Self::Plus => f.write_str("+"),
            Self::PlusEq => f.write_str("+="),
            Self::Minus => f.write_str("-"),
            Self::MinusEq => f.write_str("-="),
            Self::Lt => f.write_str("<"),
            Self::LtEq => f.write_str("<="),
            Self::LtLt => f.write_str("<<"),
            Self::LtLtEq => f.write_str("<<="),
            Self::Gt => f.write_str(">"),
            Self::GtEq => f.write_str(">="),
            Self::GtGt => f.write_str(">>"),
            Self::GtGtEq => f.write_str(">>="),
            Self::Amp => f.write_str("&"),
            Self::AmpEq => f.write_str("&="),
            Self::AmpAmp => f.write_str("&&"),
            Self::Caret => f.write_str("^"),
            Self::CaretEq => f.write_str("^="),
            Self::Pipe => f.write_str("|"),
            Self::PipeEq => f.write_str("|="),
            Self::PipePipe => f.write_str("||"),
            Self::Walrus => f.write_str(":="),
            Self::Ident(..) => f.write_str("identifier"),
            Self::Underscore => f.write_str("_"),
            Self::Return => f.write_str("`return`"),
            Self::If => f.write_str("`if`"),
            Self::Else => f.write_str("`else`"),
            Self::Match => f.write_str("`match`"),
            Self::Fn => f.write_str("`fn`"),
            Self::Let => f.write_str("`let`"),
            Self::Type => f.write_str("`type`"),
            Self::Extern => f.write_str("`extern`"),
            Self::True => f.write_str("`true`"),
            Self::False => f.write_str("`false`"),
            Self::As => f.write_str("`as`"),
            Self::Import => f.write_str("`import`"),
            Self::For => f.write_str("`for`"),
            Self::Break => f.write_str("`break`"),
            Self::Mut => f.write_str("`mut`"),
            Self::Imm => f.write_str("`imm`"),
            Self::Transmute => f.write_str("`transmute`"),
            Self::Ref => f.write_str("`ref`"),
            Self::Move => f.write_str("`move`"),
            Self::Unsafe => f.write_str("`unsafe`"),
            Self::Pub => f.write_str("`pub`"),
            Self::Int(lit) => write!(f, "integer literal `{lit}`"),
            Self::Float(lit) => write!(f, "float literal `{lit}`"),
            Self::Str(lit) => write!(f, "\"{lit}\""),
        }
    }
}
