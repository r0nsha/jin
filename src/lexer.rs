use crate::span::{Source, Span};

pub fn tokenize(source: &Source) -> Tokens {
    Tokens { tokens: vec![] }
}

#[derive(Debug)]
pub struct Tokens {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {}
