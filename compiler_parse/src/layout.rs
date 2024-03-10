use std::cmp::Ordering;

use codespan_reporting::files::Location;
use compiler_core::diagnostics::{Diagnostic, DiagnosticResult, Label};
use compiler_core::span::{Source, Span};

use crate::token::{Token, TokenKind};

pub(crate) fn apply(source: &Source, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
    Layout::new(source, input.len()).apply(input)
}

struct Layout<'a> {
    source: &'a Source,
    new_tokens: Vec<Token>,
    indents: Vec<usize>,
    line: usize,
}

impl<'a> Layout<'a> {
    fn new(source: &'a Source, len: usize) -> Self {
        Self { source, new_tokens: Vec::with_capacity(len * 2), indents: vec![0], line: 0 }
    }

    fn apply(mut self, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
        if input.is_empty() {
            return Ok(input);
        }

        for (i, tok) in input.iter().enumerate() {
            let Location { line_number, column_number } = self.source.span_location(tok.span);
            let is_first_tok = i == 0;
            let mut is_line_first_tok = is_first_tok || line_number > self.line;
            let is_expr_cont = is_line_first_tok && self.is_expr_cont(tok);
            let layout_indent = self.layout_indent();

            // Brace insertion when encountering the first token in a given line
            if is_line_first_tok && !is_first_tok {
                // Open curly insertion
                if column_number > layout_indent && !is_expr_cont {
                    is_line_first_tok = false;
                    self.push_open_curly(tok.span);
                }

                // Close curly insertion
                if column_number < layout_indent && tok.kind != TokenKind::CloseCurly {
                    is_line_first_tok = false;
                    self.push_close_curly(tok.span);
                }
            }

            // Layout indent
            if is_first_tok || self.last_token().map_or(false, |t| t.kind == TokenKind::OpenCurly) {
                if tok.kind != TokenKind::CloseCurly && column_number < layout_indent {
                    return Err(Diagnostic::error(format!(
                        "line must be indented more then its \
                         enclosing layout (column {layout_indent})"
                    ))
                    .with_label(Label::primary(tok.span, "insufficient indentation")));
                }

                self.indents.push(column_number);
            }

            // Layout dedent
            if tok.kind == TokenKind::CloseCurly {
                self.indents.pop();
            }

            // Semicolon insertion when encountering the first token in a given line
            if is_line_first_tok {
                match column_number.cmp(&layout_indent) {
                    Ordering::Less => {
                        todo!("error: must be equal or larger than the layout indentation")
                    }
                    Ordering::Equal if !is_first_tok && !is_expr_cont => {
                        let span = self.last_token().map_or(tok.span.head(), |t| t.span.tail());
                        self.push_semi(span);
                    }
                    _ => (),
                }
            }

            self.line = line_number;
            self.new_tokens.push(*tok);
        }

        let last_span = self.last_token().unwrap().span;

        if self.layout_indent() > 1 {
            self.push_close_curly(last_span);
        }

        self.new_tokens.push(Token { kind: TokenKind::Semi(true), span: last_span.tail() });

        Ok(self.new_tokens)
    }

    fn push_open_curly(&mut self, span: Span) {
        self.new_tokens.push(Token { kind: TokenKind::OpenCurly, span: span.tail() });
    }

    fn push_close_curly(&mut self, span: Span) {
        self.new_tokens.push(Token { kind: TokenKind::CloseCurly, span: span.head() });
    }

    fn push_semi(&mut self, span: Span) {
        self.new_tokens.push(Token { kind: TokenKind::Semi(true), span });
    }

    fn is_expr_cont(&self, tok: &Token) -> bool {
        tok.kind.is_start_cont() || self.last_token().map_or(false, |t| t.kind.is_end_cont())
    }

    fn last_token(&self) -> Option<&Token> {
        self.new_tokens.last()
    }

    fn layout_indent(&self) -> usize {
        *self.indents.last().unwrap()
    }
}
