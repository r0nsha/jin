use std::{
    fmt::{self, Write},
    mem,
};

use compiler_core::{span::Span, word::Word};
use phf::phf_map;
use ustr::{ustr, Ustr};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[track_caller]
    pub fn str_value(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(v) | TokenKind::StrText(v) => v,
            TokenKind::Int(v) => ustr(&v.to_string()),
            kind => panic!("unexpected token {kind:?}"),
        }
    }

    #[track_caller]
    pub fn int_value(&self) -> i128 {
        match self.kind {
            TokenKind::Int(v) => v,
            kind => panic!("unexpected token {kind:?}"),
        }
    }

    #[track_caller]
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
    OpenBrack,
    CloseBrack,
    OpenCurly(bool),
    CloseCurly(bool),

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

    // Strings
    StrOpen,
    StrClose,
    StrText(Ustr),
    StrExprOpen,
    StrExprClose,

    // Literals
    Int(i128),
    Float(f64),
    Char(char),
    ByteChar(char),
}

impl TokenKind {
    #[inline]
    pub fn empty_ident() -> Self {
        Self::Ident(ustr(""))
    }

    #[inline]
    pub fn is_start_cont(self) -> bool {
        match self {
            TokenKind::Kw(Kw::Else)
            | TokenKind::CloseParen
            | TokenKind::CloseBrack
            | TokenKind::Comma
            | TokenKind::CloseCurly(_)
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
            | TokenKind::Walrus
            | TokenKind::Dot
            | TokenKind::Colon
            | TokenKind::Arrow => true,

            TokenKind::Kw(
                Kw::Return
                | Kw::Fn
                | Kw::Let
                | Kw::Const
                | Kw::Type
                | Kw::Extern
                | Kw::If
                | Kw::Match
                | Kw::True
                | Kw::False
                | Kw::As
                | Kw::Mod
                | Kw::Import
                | Kw::For
                | Kw::Break
                | Kw::Mut
                | Kw::Imm
                | Kw::Ref
                | Kw::Move
                | Kw::Unsafe,
            )
            | TokenKind::DotDot
            | TokenKind::Semi(_)
            | TokenKind::At
            | TokenKind::QuestionMark
            | TokenKind::OpenParen
            | TokenKind::OpenBrack
            | TokenKind::OpenCurly(_)
            | TokenKind::Ident(_)
            | TokenKind::Underscore
            | TokenKind::Bang
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Amp
            | TokenKind::StrOpen
            | TokenKind::StrClose
            | TokenKind::StrText(_)
            | TokenKind::StrExprOpen
            | TokenKind::StrExprClose
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Char(_)
            | TokenKind::ByteChar(_) => false,
        }
    }

    #[inline]
    pub fn is_end_cont(self) -> bool {
        match self {
            TokenKind::OpenParen
            | TokenKind::OpenBrack
            | TokenKind::Comma
            | TokenKind::OpenCurly(_)
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
            | TokenKind::AmpEq
            | TokenKind::AmpAmp
            | TokenKind::Caret
            | TokenKind::CaretEq
            | TokenKind::Pipe
            | TokenKind::PipeEq
            | TokenKind::PipePipe
            | TokenKind::Walrus
            | TokenKind::Dot => true,

            TokenKind::Kw(
                Kw::Return
                | Kw::Fn
                | Kw::Let
                | Kw::Const
                | Kw::Type
                | Kw::Extern
                | Kw::If
                | Kw::Else
                | Kw::Match
                | Kw::True
                | Kw::False
                | Kw::As
                | Kw::Mod
                | Kw::Import
                | Kw::For
                | Kw::Break
                | Kw::Mut
                | Kw::Imm
                | Kw::Ref
                | Kw::Move
                | Kw::Unsafe,
            )
            | TokenKind::DotDot
            | TokenKind::Semi(_)
            | TokenKind::At
            | TokenKind::QuestionMark
            | TokenKind::CloseParen
            | TokenKind::CloseBrack
            | TokenKind::CloseCurly(_)
            | TokenKind::Colon
            | TokenKind::Arrow
            | TokenKind::Ident(_)
            | TokenKind::Underscore
            | TokenKind::Star
            | TokenKind::Amp
            | TokenKind::StrOpen
            | TokenKind::StrClose
            | TokenKind::StrText(_)
            | TokenKind::StrExprOpen
            | TokenKind::StrExprClose
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Char(_)
            | TokenKind::ByteChar(_) => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OpenParen => f.write_char('('),
            Self::CloseParen => f.write_char(')'),
            Self::OpenBrack => f.write_char('['),
            Self::CloseBrack => f.write_char(']'),
            Self::OpenCurly(true) => f.write_str("implicit {"),
            Self::OpenCurly(false) => f.write_char('{'),
            Self::CloseCurly(true) => f.write_str("implicit }"),
            Self::CloseCurly(false) => f.write_char('}'),
            Self::Comma => f.write_char(','),
            Self::Dot => f.write_char('.'),
            Self::DotDot => f.write_str(".."),
            Self::Colon => f.write_char(':'),
            Self::Semi(true) => f.write_str("implicit ;"),
            Self::Semi(false) => f.write_char(';'),
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
            Self::StrOpen => f.write_char('"'),
            Self::StrClose => f.write_str("a terminating `\"`"),
            Self::StrText(_) => f.write_str("a string literal"),
            Self::StrExprOpen => f.write_str("the start of a string interpolation"),
            Self::StrExprClose => f.write_str("the end of a string interpolation"),
            Self::Kw(kw) => write!(f, "the `{kw}` keyword"),
            Self::Int(lit) => write!(f, "the integer literal `{lit}`"),
            Self::Float(lit) => write!(f, "the float literal `{lit}`"),
            Self::Char(ch) | Self::ByteChar(ch) => write!(f, "character literal \'{ch}\'"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kw {
    Return,
    Fn,
    Let,
    Const,
    Type,
    Extern,
    If,
    Else,
    Match,
    True,
    False,
    As,
    Mod,
    Import,
    For,
    Break,
    Mut,
    Imm,
    Ref,
    Move,
    Unsafe,
}

static KEYWORDS: phf::Map<&'static str, Kw> = phf_map! {
    "return" => Kw::Return,
    "fn" => Kw::Fn,
    "let" => Kw::Let,
    "const" => Kw::Const,
    "type" => Kw::Type,
    "extern" => Kw::Extern,
    "if" => Kw::If,
    "else" => Kw::Else,
    "match" => Kw::Match,
    "true" => Kw::True,
    "false" => Kw::False,
    "as" => Kw::As,
    "mod" => Kw::Mod,
    "import" => Kw::Import,
    "for" => Kw::For,
    "break" => Kw::Break,
    "mut" => Kw::Mut,
    "imm" => Kw::Imm,
    "ref" => Kw::Ref,
    "move" => Kw::Move,
    "unsafe" => Kw::Unsafe,
};

impl<'a> TryFrom<&'a str> for Kw {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        KEYWORDS.get(value).copied().ok_or(value)
    }
}

impl fmt::Display for Kw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Return => "return",
            Self::If => "if",
            Self::Else => "else",
            Self::Match => "match",
            Self::Fn => "fn",
            Self::Let => "let",
            Self::Const => "const",
            Self::Type => "type",
            Self::Extern => "extern",
            Self::True => "true",
            Self::False => "false",
            Self::As => "as",
            Self::Mod => "mod",
            Self::Import => "import",
            Self::For => "for",
            Self::Break => "break",
            Self::Mut => "mut",
            Self::Imm => "imm",
            Self::Ref => "ref",
            Self::Move => "move",
            Self::Unsafe => "unsafe",
        })
    }
}
