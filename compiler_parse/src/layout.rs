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
        Self { source, new_tokens: Vec::with_capacity(len * 2), indents: vec![1], line: 0 }
    }

    fn apply(mut self, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
        if input.is_empty() {
            return Ok(input);
        }

        let mut is_curly_balanced = true;

        for (i, tok) in input.iter().enumerate() {
            let Location { line_number, column_number } = self.source.span_location(tok.span);
            let is_first_tok = i == 0;
            let is_first_line_tok = is_first_tok || line_number > self.line;

            let layout_indent = self.indents.last().copied().unwrap();

            // Brace insertion
            if is_first_line_tok {
                // Open curly insertion
                if column_number > layout_indent {
                    let is_expr_cont = tok.kind.is_start_cont()
                        || self.last_token().map_or(false, |t| t.kind.is_end_cont());

                    if !is_expr_cont {
                        is_curly_balanced = false;
                        self.push_open_curly(tok.span);
                    }
                }

                // Close curly insertion
                if column_number < layout_indent && tok.kind != TokenKind::CloseCurly {
                    is_curly_balanced = true;
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

            // TODO: semicolon insertion
            // TODO: insert semicolon before any curly
            // TODO: insert semicolon after all tokens

            self.line = line_number;
            self.new_tokens.push(*tok);
        }

        if !is_curly_balanced {
            let span = self.last_token().unwrap().span;
            self.push_close_curly(span);
        }

        Ok(self.new_tokens)
    }

    fn push_open_curly(&mut self, span: Span) {
        self.new_tokens.push(Token { kind: TokenKind::OpenCurly, span: span.tail() });
    }

    fn push_close_curly(&mut self, span: Span) {
        self.new_tokens.push(Token { kind: TokenKind::CloseCurly, span: span.head() });
    }

    fn last_token(&self) -> Option<&Token> {
        self.new_tokens.last()
    }
}
