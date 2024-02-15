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
    Kw(Kw),

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
            TokenKind::Kw(Kw::Return | Kw::True | Kw::False | Kw::Break)
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Str(_)
            | TokenKind::Ident(_)
            | TokenKind::Underscore
            | TokenKind::QuestionMark
            | TokenKind::CloseParen
            | TokenKind::CloseBracket
            | TokenKind::CloseCurly
            | TokenKind::Semi(_) => true,

            TokenKind::Kw(
                Kw::Fn
                | Kw::Let
                | Kw::Type
                | Kw::Extern
                | Kw::If
                | Kw::Else
                | Kw::Match
                | Kw::As
                | Kw::Use
                | Kw::For
                | Kw::Mut
                | Kw::Imm
                | Kw::Transmute
                | Kw::Ref
                | Kw::Move
                | Kw::Unsafe
                | Kw::Pub,
            )
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
            | TokenKind::Star
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
            TokenKind::Kw(
                Kw::Return
                | Kw::True
                | Kw::False
                | Kw::Break
                | Kw::Fn
                | Kw::Let
                | Kw::Type
                | Kw::Extern
                | Kw::If
                | Kw::Match
                | Kw::Use
                | Kw::For
                | Kw::Transmute
                | Kw::Unsafe
                | Kw::Pub,
            )
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Str(_)
            | TokenKind::Ident(_)
            | TokenKind::Underscore
            | TokenKind::Semi(_)
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
            | TokenKind::Kw(Kw::Else | Kw::As | Kw::Mut | Kw::Imm | Kw::Ref | Kw::Move)
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Colon
            | TokenKind::Arrow
            | TokenKind::Comma
            | TokenKind::Eq
            | TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Star
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
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Int(lit) => write!(f, "integer literal `{lit}`"),
            Self::Float(lit) => write!(f, "float literal `{lit}`"),
            Self::Str(lit) => write!(f, "\"{lit}\""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kw {
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
    Use,
    For,
    Break,
    Mut,
    Imm,
    Transmute,
    Ref,
    Move,
    Unsafe,
    Pub,
}

impl<'a> TryFrom<&'a str> for Kw {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Ok(match value {
            "return" => Self::Return,
            "fn" => Self::Fn,
            "let" => Self::Let,
            "type" => Self::Type,
            "extern" => Self::Extern,
            "if" => Self::If,
            "else" => Self::Else,
            "match" => Self::Match,
            "true" => Self::True,
            "false" => Self::False,
            "as" => Self::As,
            "use" => Self::Use,
            "for" => Self::For,
            "break" => Self::Break,
            "mut" => Self::Mut,
            "imm" => Self::Imm,
            "transmute" => Self::Transmute,
            "ref" => Self::Ref,
            "move" => Self::Move,
            "unsafe" => Self::Unsafe,
            "pub" => Self::Pub,
            value => return Err(value),
        })
    }
}

impl fmt::Display for Kw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "`{}`",
            match self {
                Self::Return => "return",
                Self::If => "if",
                Self::Else => "else",
                Self::Match => "match",
                Self::Fn => "fn",
                Self::Let => "let",
                Self::Type => "type",
                Self::Extern => "extern",
                Self::True => "true",
                Self::False => "false",
                Self::As => "as",
                Self::Use => "use",
                Self::For => "for",
                Self::Break => "break",
                Self::Mut => "mut",
                Self::Imm => "imm",
                Self::Transmute => "transmute",
                Self::Ref => "ref",
                Self::Move => "move",
                Self::Unsafe => "unsafe",
                Self::Pub => "pub",
            }
        )
    }
}
