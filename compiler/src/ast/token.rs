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
    pub fn str_value(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(v) | TokenKind::Str(v) => v,
            kind => panic!("expected Ident or Str, found {kind:?}"),
        }
    }

    pub fn word(&self) -> Word {
        Word::new(self.str_value(), self.span)
    }

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
    At,
    Hash,
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
    True,
    False,
    As,
    Import,
    Loop,
    Break,
    Mut,
    Imm,

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
            Self::At => f.write_char('@'),
            Self::Hash => f.write_char('#'),
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
            Self::Ident(..) => f.write_str("identifier"),
            Self::Underscore => f.write_str("_"),
            Self::Return => f.write_str("`return`"),
            Self::If => f.write_str("`if`"),
            Self::Else => f.write_str("`else`"),
            Self::Fn => f.write_str("`fn`"),
            Self::Let => f.write_str("`let`"),
            Self::Type => f.write_str("`type`"),
            Self::Extern => f.write_str("`extern`"),
            Self::True => f.write_str("`true`"),
            Self::False => f.write_str("`false`"),
            Self::As => f.write_str("`as`"),
            Self::Import => f.write_str("`import`"),
            Self::Loop => f.write_str("`loop`"),
            Self::Break => f.write_str("`break`"),
            Self::Mut => f.write_str("`mut`"),
            Self::Imm => f.write_str("`imm`"),
            Self::Int(lit) => write!(f, "integer literal `{lit}`"),
            Self::Float(lit) => write!(f, "float literal `{lit}`"),
            Self::Str(lit) => write!(f, "\"{lit}\""),
        }
    }
}
