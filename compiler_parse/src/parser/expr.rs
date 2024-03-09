use std::ops::ControlFlow;

use compiler_ast::{Attrs, CallArg, CharKind, Expr, LetKind};
use compiler_core::{
    db::DefId,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{BinOp, NamePat, Pat, UnOp, Vis},
    span::{Span, Spanned},
    ty::TyKind,
};
use compiler_data_structures::index_vec::Key as _;
use ustr::ustr;

use crate::parser::item::AllowVis;
use crate::parser::SEMI;
use crate::{
    bin_op_from_assign_op, errors,
    parser::{item::RequireTy, AllowOmitParens, Parser, RequireSigTy},
    token::{Kw, TokenKind},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
        let mut expr_stack: Vec<Expr> = vec![];
        let mut op_stack: Vec<BinOp> = vec![];
        let mut last_precedence = usize::MAX;

        expr_stack.push(self.parse_operand()?);

        while let Some(tok) = self.token() {
            let op = match BinOp::try_from(tok.kind).ok() {
                Some(op) => {
                    self.next();
                    op
                }
                None => break,
            };

            let rhs = self.parse_operand()?;

            let precedence = op.precedence();

            while precedence <= last_precedence && expr_stack.len() > 1 {
                let rhs = expr_stack.pop().unwrap();
                let op = op_stack.pop().unwrap();

                last_precedence = op.precedence();

                if last_precedence < precedence {
                    expr_stack.push(rhs);
                    op_stack.push(op);
                    break;
                }

                let lhs = expr_stack.pop().unwrap();
                let span = lhs.span().merge(rhs.span());

                expr_stack.push(Expr::Binary { lhs: Box::new(lhs), rhs: Box::new(rhs), op, span });
            }

            op_stack.push(op);
            expr_stack.push(rhs);

            last_precedence = precedence;
        }

        while expr_stack.len() > 1 {
            let rhs = expr_stack.pop().unwrap();
            let op = op_stack.pop().unwrap();
            let lhs = expr_stack.pop().unwrap();

            let span = lhs.span().merge(rhs.span());

            expr_stack.push(Expr::Binary { lhs: Box::new(lhs), op, rhs: Box::new(rhs), span });
        }

        Ok(expr_stack.into_iter().next().unwrap())
    }

    fn parse_operand(&mut self) -> DiagnosticResult<Expr> {
        let atom = self.parse_atom()?;
        self.parse_postfix(atom)
    }

    fn parse_atom(&mut self) -> DiagnosticResult<Expr> {
        let tok = self.eat_any()?;

        let expr = match tok.kind {
            TokenKind::Kw(Kw::Fn) => self.parse_fn_expr()?,
            TokenKind::Kw(Kw::Return) => self.parse_return()?,
            TokenKind::Kw(Kw::If) => self.parse_if()?,
            TokenKind::Kw(Kw::Match) => self.parse_match()?,
            TokenKind::Kw(Kw::For) => self.parse_loop()?,
            TokenKind::Kw(Kw::Break) => Expr::Break { span: tok.span },
            TokenKind::Kw(Kw::As) => self.parse_cast()?,
            TokenKind::Minus => {
                let expr = self.parse_operand()?;

                Expr::Unary {
                    span: tok.span.merge(expr.span()),
                    expr: Box::new(expr),
                    op: UnOp::Neg,
                }
            }
            TokenKind::Amp => {
                let mutability = self.parse_mutability();
                let expr = self.parse_operand()?;

                Expr::Unary {
                    span: tok.span.merge(expr.span()),
                    expr: Box::new(expr),
                    op: UnOp::Ref(mutability),
                }
            }
            TokenKind::Bang => {
                let expr = self.parse_operand()?;

                Expr::Unary {
                    span: tok.span.merge(expr.span()),
                    expr: Box::new(expr),
                    op: UnOp::Not,
                }
            }
            TokenKind::OpenParen => {
                if self.is(TokenKind::CloseParen) {
                    Expr::UnitLit { span: tok.span.merge(self.last_span()) }
                } else {
                    let expr = self.parse_expr()?;
                    self.eat(TokenKind::CloseParen)?;
                    expr
                }
            }
            TokenKind::OpenCurly => {
                self.back();
                self.parse_block()?
            }
            TokenKind::OpenBrack => self.parse_slice_lit()?,
            TokenKind::Kw(Kw::Unsafe) => {
                let expr = self.parse_expr()?;
                let span = tok.span.merge(expr.span());
                Expr::Unsafe { expr: Box::new(expr), span }
            }
            TokenKind::Kw(Kw::True) => Expr::BoolLit { value: true, span: tok.span },
            TokenKind::Kw(Kw::False) => Expr::BoolLit { value: false, span: tok.span },
            TokenKind::Ident(..) => {
                let targs = self.parse_optional_targs()?;
                Expr::Name { word: tok.word(), targs, span: tok.span }
            }
            TokenKind::Int(value) => Expr::IntLit { value: value as u128, span: tok.span },
            TokenKind::Float(value) => Expr::FloatLit { value, span: tok.span },
            TokenKind::StrOpen => self.parse_str()?,
            TokenKind::Char(value) => Expr::CharLit { value, kind: CharKind::Char, span: tok.span },
            TokenKind::ByteChar(value) => {
                Expr::CharLit { value, kind: CharKind::Byte, span: tok.span }
            }
            _ => return Err(errors::unexpected_token_err("an expression", tok.kind, tok.span)),
        };

        Ok(expr)
    }

    fn parse_if(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let cond = self.parse_expr()?;
        let then = self.parse_block()?;

        let otherwise = if self.is_kw(Kw::Else) {
            if self.peek_is(TokenKind::OpenCurly) {
                Some(Box::new(self.parse_block()?))
            } else if self.is_kw(Kw::If) {
                Some(Box::new(self.parse_if()?))
            } else {
                return Err(self.unexpected_token("{ or `if`"));
            }
        } else {
            None
        };

        let span = start.merge(otherwise.as_ref().map_or(then.span(), |o| o.span()));

        Ok(Expr::If { cond: Box::new(cond), then: Box::new(then), otherwise, span })
    }

    fn parse_loop(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let cond = if self.is_kw(Kw::If) { Some(Box::new(self.parse_expr()?)) } else { None };
        let expr = self.parse_block()?;
        let span = start.merge(expr.span());
        Ok(Expr::Loop { cond, expr: Box::new(expr), span })
    }

    fn parse_cast(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();

        self.eat(TokenKind::OpenBrack)?;
        let target = self.parse_ty()?;
        self.eat(TokenKind::CloseBrack)?;

        self.eat(TokenKind::OpenParen)?;
        let expr = self.parse_expr()?;
        self.eat(TokenKind::CloseParen)?;

        let span = start.merge(self.last_span());
        Ok(Expr::Cast { expr: Box::new(expr), target, span })
    }

    fn parse_postfix(&mut self, mut expr: Expr) -> DiagnosticResult<Expr> {
        while let Some(tok) = self.token() {
            expr = match tok.kind {
                TokenKind::OpenParen => self.parse_call(expr)?,
                TokenKind::Dot => {
                    self.next();

                    if self.is(TokenKind::OpenBrack) {
                        self.parse_index(expr)?
                    } else {
                        self.parse_field(expr)?
                    }
                }
                TokenKind::Eq => {
                    self.next();
                    let rhs = self.parse_expr()?;
                    let span = expr.span().merge(rhs.span());

                    Expr::Assign { lhs: Box::new(expr), rhs: Box::new(rhs), op: None, span }
                }
                TokenKind::Walrus => {
                    self.next();
                    let rhs = self.parse_expr()?;
                    let span = expr.span().merge(rhs.span());
                    Expr::Swap { lhs: Box::new(expr), rhs: Box::new(rhs), span }
                }
                TokenKind::Kw(Kw::Fn) => {
                    self.next();

                    let fn_expr = self.parse_fn_expr()?;
                    let span = expr.span().merge(fn_expr.span());
                    let fn_arg = CallArg::Positional(fn_expr);

                    match expr {
                        Expr::Call { ref mut args, .. } | Expr::MethodCall { ref mut args, .. } => {
                            args.push(fn_arg);
                            expr
                        }
                        Expr::Field { expr, field, .. } => Expr::MethodCall {
                            expr,
                            method: field,
                            targs: None,
                            args: vec![fn_arg],
                            span,
                        },
                        _ => Expr::Call { callee: Box::new(expr), args: vec![fn_arg], span },
                    }
                }
                _ => {
                    if let Some(op) = bin_op_from_assign_op(tok.kind) {
                        // OpAssign (x += 1)
                        self.next();

                        let rhs = self.parse_expr()?;
                        let span = expr.span().merge(rhs.span());

                        Expr::Assign { lhs: Box::new(expr), rhs: Box::new(rhs), op: Some(op), span }
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(expr)
    }

    fn parse_call(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let (args, args_span) = self.parse_call_args()?;
        let span = expr.span().merge(args_span);
        Ok(Expr::Call { callee: Box::new(expr), args, span })
    }

    fn parse_call_args(&mut self) -> DiagnosticResult<(Vec<CallArg>, Span)> {
        let mut passed_named_arg = false;

        self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            let arg = Parser::parse_call_arg(this)?;

            match &arg {
                CallArg::Positional(expr) if passed_named_arg => {
                    return Err(Diagnostic::error(
                        "positional arguments are not allowed after named arguments",
                    )
                    .with_label(Label::primary(expr.span(), "unexpected positional argument")));
                }
                CallArg::Positional(_) => (),
                CallArg::Named(..) => passed_named_arg = true,
            }

            Ok(ControlFlow::Continue(arg))
        })
    }

    fn parse_call_arg(&mut self) -> DiagnosticResult<CallArg> {
        if self.is_ident() {
            let ident_tok = self.last_token();

            if self.is(TokenKind::Colon) {
                let expr = self.parse_expr()?;
                return Ok(CallArg::Named(ident_tok.word(), expr));
            }

            self.back();
        }

        let expr = self.parse_expr()?;
        Ok(CallArg::Positional(expr))
    }

    fn parse_index(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        if self.is(TokenKind::DotDot) {
            return self.parse_slice_high_helper(expr, None);
        }

        let index = self.parse_expr()?;

        if self.is(TokenKind::DotDot) {
            return self.parse_slice_high_helper(expr, Some(Box::new(index)));
        }

        self.eat(TokenKind::CloseBrack)?;
        let span = expr.span().merge(self.last_span());
        Ok(Expr::Index { expr: Box::new(expr), index: Box::new(index), span })
    }

    fn parse_slice_high_helper(
        &mut self,
        expr: Expr,
        low: Option<Box<Expr>>,
    ) -> DiagnosticResult<Expr> {
        let high = if self.peek_is(TokenKind::CloseBrack) {
            None
        } else {
            Some(Box::new(self.parse_expr()?))
        };

        self.eat(TokenKind::CloseBrack)?;
        let span = expr.span().merge(self.last_span());
        Ok(Expr::Slice { expr: Box::new(expr), low, high, span })
    }

    fn parse_field(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        if self.is(TokenKind::Int(0)) {
            let field = self.last_token().word();
            let span = expr.span().merge(field.span());
            return Ok(Expr::Field { expr: Box::new(expr), field, span });
        }

        let name = self.eat_ident()?.word();

        let targs = self.parse_optional_targs()?;

        if targs.is_some() || self.peek_is(TokenKind::OpenParen) {
            let (args, args_span) = self.parse_call_args()?;
            let span = expr.span().merge(args_span);
            Ok(Expr::MethodCall { expr: Box::new(expr), method: name, targs, args, span })
        } else {
            let span = expr.span().merge(name.span());
            Ok(Expr::Field { expr: Box::new(expr), field: name, span })
        }
    }

    pub(super) fn parse_pat(&mut self, allow_vis: AllowVis) -> DiagnosticResult<Pat> {
        let mutability = self.parse_mutability();
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::Ident(_) => {
                let vis =
                    if allow_vis == AllowVis::Yes { self.parse_vis() } else { Vis::default() };
                Ok(Pat::Name(NamePat {
                    id: DefId::null(),
                    word: tok.word(),
                    mutability,
                    vis,
                    ty: TyKind::Unknown.into(),
                }))
            }
            TokenKind::Underscore => Ok(Pat::Discard(tok.span)),
            _ => Err(errors::unexpected_token_err("a pattern", tok.kind, tok.span)),
        }
    }

    pub(crate) fn parse_block(&mut self) -> DiagnosticResult<Expr> {
        self.parse_list_with_sep(TokenKind::OpenCurly, TokenKind::CloseCurly, SEMI, |this| {
            this.parse_stmt().map(ControlFlow::Continue)
        })
        .map(|(exprs, span)| Expr::Block { exprs, span })
    }

    fn parse_slice_lit(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();

        if self.is(TokenKind::Colon) {
            let cap = Box::new(self.parse_expr()?);
            self.eat(TokenKind::CloseBrack)?;
            return Ok(Expr::SliceLitCap { cap, span: start.merge(self.last_span()) });
        }

        self.back();

        let (exprs, span) =
            self.parse_list(TokenKind::OpenBrack, TokenKind::CloseBrack, |this| {
                this.parse_expr().map(ControlFlow::Continue)
            })?;

        Ok(Expr::SliceLit { exprs, span })
    }

    fn parse_stmt(&mut self) -> DiagnosticResult<Expr> {
        if self.is_kw(Kw::Let) {
            self.parse_let(Attrs::new(), LetKind::Let, AllowVis::No, RequireTy::No).map(Expr::Let)
        } else {
            self.parse_expr()
        }
    }

    fn parse_return(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let expr = if self.is(SEMI) { None } else { Some(Box::new(self.parse_expr()?)) };
        let span = expr.as_ref().map_or(start, |e| start.merge(e.span()));
        Ok(Expr::Return { expr, span })
    }

    fn parse_fn_expr(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let (params, ret, is_c_variadic) =
            self.parse_fn_sig_helper(AllowOmitParens::Yes, RequireSigTy::No)?;

        if is_c_variadic {
            return Err(errors::invalid_c_variadic(start));
        }

        let body = self.parse_block()?;
        let span = start.merge(body.span());

        Ok(Expr::Fn { params, ret, body: Box::new(body), span })
    }

    fn parse_str(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let mut exprs = vec![];

        while !self.is(TokenKind::StrClose) {
            if self.is(TokenKind::StrText(ustr(""))) {
                exprs.push(Expr::StrLit {
                    value: self.last_token().str_value(),
                    span: self.last_span(),
                });
            } else if self.is(TokenKind::StrExprOpen) {
                let expr = self.parse_expr()?;
                self.eat(TokenKind::StrExprClose)?;
                exprs.push(expr);
            } else {
                return Err(self.unexpected_token("a string"));
            }
        }

        match exprs.len() {
            0 => Ok(Expr::StrLit { value: ustr(""), span: start.merge(self.last_span()) }),
            1 if matches!(&exprs[0], Expr::StrLit { .. }) => {
                let Expr::StrLit { value, span } = exprs.swap_remove(0) else { unreachable!() };
                Ok(Expr::StrLit { value, span: span.merge(self.last_span()) })
            }
            _ => Ok(Expr::StrInterp { exprs, span: start.merge(self.last_span()) }),
        }
    }
}
