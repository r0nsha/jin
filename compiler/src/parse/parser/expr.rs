use std::ops::ControlFlow;

use data_structures::index_vec::Key as _;
use ustr::ustr;

use crate::{
    ast::{Attrs, CallArg, Expr, MatchArm, MatchPat, MatchPatAdt, Subpat},
    db::DefId,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{BinOp, Mutability, NamePat, Pat, UnOp},
    parse::{
        errors,
        parser::{item::RequireTy, AllowOmitParens, Parser, RequireSigTy},
        token::TokenKind,
    },
    span::{Span, Spanned},
    ty::TyKind,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
        let mut expr_stack: Vec<Expr> = vec![];
        let mut op_stack: Vec<BinOp> = vec![];
        let mut last_precedence = usize::MAX;

        expr_stack.push(self.parse_operand()?);

        while let Some(tok) = self.token() {
            let op = match BinOp::try_from(tok.kind).ok() {
                Some(BinOp::BitAnd | BinOp::Sub)
                    if !self.spans_are_on_same_line(
                        expr_stack.last().expect("to have an expr").span(),
                        tok.span,
                    ) =>
                {
                    // For these specific operators, we check if they are on the same line as the
                    // last expr, to avoid ambiguity with unary operators
                    break;
                }
                None => break,
                Some(op) => {
                    self.next();
                    op
                }
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
            TokenKind::Fn => self.parse_fn_expr()?,
            TokenKind::Return => self.parse_return()?,
            TokenKind::If => self.parse_if()?,
            TokenKind::Match => self.parse_match()?,
            TokenKind::For => self.parse_loop()?,
            TokenKind::Break => Expr::Break { span: tok.span },
            TokenKind::Transmute => self.parse_transmute()?,
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
            TokenKind::OpenCurly => {
                self.back();
                self.parse_block()?
            }
            TokenKind::OpenBracket => self.parse_slice_lit()?,
            TokenKind::True => Expr::BoolLit { value: true, span: tok.span },
            TokenKind::False => Expr::BoolLit { value: false, span: tok.span },
            TokenKind::Ident(..) => {
                let targs = self.parse_optional_ty_args()?;
                Expr::Name { word: tok.word(), targs, span: tok.span }
            }
            TokenKind::Int(value) => {
                Expr::IntLit { value: Self::int_lit(value.as_str()), span: tok.span }
            }
            TokenKind::Float(value) => Expr::FloatLit {
                value: value.replace('_', "").parse().expect("to be a valid float"),
                span: tok.span,
            },
            TokenKind::Str(value) => Expr::StrLit { value, span: tok.span },
            _ => return Err(errors::unexpected_token_err("an expression", tok.kind, tok.span)),
        };

        Ok(expr)
    }

    fn parse_if(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let cond = self.parse_expr()?;
        let then = self.parse_block()?;

        let otherwise = if self.is(TokenKind::Else) {
            if self.peek_is(TokenKind::OpenCurly) {
                Some(Box::new(self.parse_block()?))
            } else if self.is(TokenKind::If) {
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

    fn parse_match(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let expr = self.parse_expr()?;

        let (arms, _) = self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, |this| {
            let case = this.parse_match_arm()?;
            Ok(ControlFlow::Continue(case))
        })?;

        let span = start.merge(self.last_span());

        Ok(Expr::Match { expr: Box::new(expr), arms, span })
    }

    fn parse_match_arm(&mut self) -> DiagnosticResult<MatchArm> {
        let pat = self.parse_match_pat()?;

        let guard =
            self.is_and(TokenKind::If, |this, _| this.parse_expr()).transpose()?.map(Box::new);

        self.eat(TokenKind::Arrow)?;
        let expr = self.parse_expr()?;

        Ok(MatchArm { pat, guard, expr: Box::new(expr) })
    }

    fn parse_match_pat(&mut self) -> DiagnosticResult<MatchPat> {
        self.skip(TokenKind::Pipe);

        let pat = self.parse_match_pat_atom()?;

        if self.peek_is(TokenKind::Pipe) {
            let start_span = pat.span();
            let mut pats = vec![pat];

            while self.is(TokenKind::Pipe) {
                pats.push(self.parse_match_pat_atom()?);
            }

            Ok(MatchPat::Or(pats, start_span.merge(self.last_span())))
        } else {
            Ok(pat)
        }
    }

    fn parse_match_pat_atom(&mut self) -> DiagnosticResult<MatchPat> {
        if let Some(mutability) = self.parse_optional_mutability() {
            let word = self.eat_ident()?.word();
            Ok(MatchPat::Name(word, mutability))
        } else if self.is_ident() {
            let start_tok = self.last_token();
            let start_word = start_tok.word();

            if self.is(TokenKind::Dot) {
                let mut path = vec![start_word];

                path.push(self.eat_ident()?.word());
                while self.is(TokenKind::Dot) {
                    path.push(self.eat_ident()?.word());
                }

                let (subpats, is_exhaustive) = if self.peek_is(TokenKind::OpenParen) {
                    self.parse_match_adt_subpats()?
                } else {
                    (vec![], true)
                };

                let span = start_word.span().merge(self.last_span());

                Ok(MatchPat::Adt(MatchPatAdt { path, subpats, is_exhaustive, span }))
            } else if self.peek_is(TokenKind::OpenParen) {
                let path = vec![start_word];
                let (subpats, is_exhaustive) = self.parse_match_adt_subpats()?;
                let span = start_word.span().merge(self.last_span());
                Ok(MatchPat::Adt(MatchPatAdt { path, subpats, is_exhaustive, span }))
            } else {
                Ok(MatchPat::Name(start_word, Mutability::Imm))
            }
        } else if self.is(TokenKind::Underscore) {
            Ok(MatchPat::Wildcard(self.last_span()))
        } else if self.is(TokenKind::OpenCurly) {
            let start_span = self.last_span();
            let last_span = self.eat(TokenKind::CloseCurly)?.span;
            Ok(MatchPat::Unit(start_span.merge(last_span)))
        } else if self.is(TokenKind::Minus) {
            let start_span = self.last_span();
            let tok = self.eat(TokenKind::Int(ustr("")))?;
            let value: i128 = Self::int_lit(&tok.str_value());
            Ok(MatchPat::Int(-value, start_span.merge(tok.span)))
        } else if self.is(TokenKind::Int(ustr(""))) {
            let tok = self.last_token();
            let value = Self::int_lit(&tok.str_value());
            Ok(MatchPat::Int(value, tok.span))
        } else if self.is(TokenKind::empty_str()) {
            let tok = self.last_token();
            Ok(MatchPat::Str(tok.str_value(), tok.span))
        } else if self.is(TokenKind::True) {
            Ok(MatchPat::Bool(true, self.last_span()))
        } else if self.is(TokenKind::False) {
            Ok(MatchPat::Bool(false, self.last_span()))
        } else {
            Err(self.unexpected_token("a pattern"))
        }
    }

    fn parse_match_adt_subpats(&mut self) -> DiagnosticResult<(Vec<Subpat>, bool)> {
        let mut is_exhaustive = true;
        let mut passed_named_pat = false;

        let (subpats, _) =
            self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
                if this.is(TokenKind::DotDot) {
                    is_exhaustive = false;
                    return Ok(ControlFlow::Break(()));
                }

                let subpat = this.parse_match_adt_subpat()?;

                match subpat {
                    Subpat::Positional(_) if passed_named_pat => {
                        return Err(Diagnostic::error(
                            "positional patterns are not allowed after named patterns",
                        )
                        .with_label(Label::primary(
                            subpat.span(),
                            "unexpected positional pattern",
                        )));
                    }
                    Subpat::Positional(_) => (),
                    Subpat::Named(_, _) => passed_named_pat = true,
                }

                Ok(ControlFlow::Continue(subpat))
            })?;

        Ok((subpats, is_exhaustive))
    }

    fn parse_match_adt_subpat(&mut self) -> DiagnosticResult<Subpat> {
        if self.is_ident() {
            let name = self.last_token().word();

            if self.is(TokenKind::Colon) {
                let pat = self.parse_match_pat()?;
                return Ok(Subpat::Named(name, pat));
            }

            self.back();
        }

        let pat = self.parse_match_pat()?;
        Ok(Subpat::Positional(pat))
    }

    fn parse_loop(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let cond = if self.is(TokenKind::If) { Some(Box::new(self.parse_expr()?)) } else { None };
        let expr = self.parse_block()?;
        let span = start.merge(expr.span());
        Ok(Expr::Loop { cond, expr: Box::new(expr), span })
    }

    fn parse_transmute(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();

        self.eat(TokenKind::OpenBracket)?;
        let target = self.parse_ty()?;
        self.eat(TokenKind::CloseBracket)?;

        self.eat(TokenKind::OpenParen)?;
        let expr = self.parse_expr()?;
        self.eat(TokenKind::CloseParen)?;

        let span = start.merge(self.last_span());

        Ok(Expr::Transmute { expr: Box::new(expr), target, span })
    }

    fn parse_postfix(&mut self, mut expr: Expr) -> DiagnosticResult<Expr> {
        while let Some(tok) = self.token() {
            expr = match tok.kind {
                TokenKind::OpenParen => self.parse_call(expr)?,
                TokenKind::As => {
                    self.next();
                    let target = self.parse_ty()?;
                    let span = expr.span().merge(target.span());
                    Expr::Cast { expr: Box::new(expr), target, span }
                }
                TokenKind::Dot => {
                    self.next();

                    if self.is(TokenKind::OpenBracket) {
                        self.parse_index(expr)?
                    } else if self.is(TokenKind::Star) {
                        let span = expr.span().merge(self.last_span());
                        Expr::Deref { expr: Box::new(expr), span }
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
                TokenKind::Fn if self.spans_are_on_same_line(expr.span(), tok.span) => {
                    self.next();

                    let fn_expr = self.parse_fn_expr()?;
                    let span = expr.span().merge(fn_expr.span());
                    let fn_arg = CallArg::Positional(fn_expr);

                    match &mut expr {
                        Expr::Call { args, .. } | Expr::MethodCall { args, .. } => {
                            args.push(fn_arg);
                            expr
                        }
                        _ => Expr::Call { callee: Box::new(expr), args: vec![fn_arg], span },
                    }
                }
                _ => {
                    if let Some(op) = BinOp::from_assign_op(tok.kind) {
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

        self.eat(TokenKind::CloseBracket)?;
        let span = expr.span().merge(self.last_span());
        Ok(Expr::Index { expr: Box::new(expr), index: Box::new(index), span })
    }

    fn parse_slice_high_helper(
        &mut self,
        expr: Expr,
        low: Option<Box<Expr>>,
    ) -> DiagnosticResult<Expr> {
        let high = if self.peek_is(TokenKind::CloseBracket) {
            None
        } else {
            Some(Box::new(self.parse_expr()?))
        };

        self.eat(TokenKind::CloseBracket)?;
        let span = expr.span().merge(self.last_span());
        Ok(Expr::Slice { expr: Box::new(expr), low, high, span })
    }

    fn parse_field(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let name = self.eat_ident()?.word();

        let ty_args = self.parse_optional_ty_args()?;

        if ty_args.is_some() || self.peek_is(TokenKind::OpenParen) {
            let (args, args_span) = self.parse_call_args()?;
            let span = expr.span().merge(args_span);
            Ok(Expr::MethodCall { expr: Box::new(expr), method: name, targs: ty_args, args, span })
        } else {
            let span = expr.span().merge(name.span());
            Ok(Expr::Field { expr: Box::new(expr), field: name, span })
        }
    }

    pub(super) fn parse_pat(&mut self) -> DiagnosticResult<Pat> {
        let mutability = self.parse_mutability();
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::Ident(_) => {
                let vis = self.parse_vis();

                Ok(Pat::Name(NamePat {
                    id: DefId::null(),
                    word: tok.word(),
                    vis,
                    mutability,
                    ty: TyKind::Unknown.into(),
                }))
            }
            TokenKind::Underscore => Ok(Pat::Discard(tok.span)),
            _ => Err(errors::unexpected_token_err("a pattern", tok.kind, tok.span)),
        }
    }

    fn parse_block(&mut self) -> DiagnosticResult<Expr> {
        self.parse_list_with_sep(
            TokenKind::OpenCurly,
            TokenKind::CloseCurly,
            TokenKind::Semi(false),
            |this| this.parse_stmt().map(ControlFlow::Continue),
        )
        .map(|(exprs, span)| Expr::Block { exprs, span })
    }

    fn parse_slice_lit(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();

        if self.is(TokenKind::Colon) {
            let cap = Box::new(self.parse_expr()?);
            self.eat(TokenKind::CloseBracket)?;
            return Ok(Expr::SliceLitCap { cap, span: start.merge(self.last_span()) });
        }

        self.back();

        let (exprs, span) =
            self.parse_list(TokenKind::OpenBracket, TokenKind::CloseBracket, |this| {
                this.parse_expr().map(ControlFlow::Continue)
            })?;

        Ok(Expr::SliceLit { exprs, span })
    }

    fn parse_stmt(&mut self) -> DiagnosticResult<Expr> {
        if self.is(TokenKind::Let) {
            let let_ = self.parse_let(Attrs::new(), RequireTy::No)?;
            Ok(Expr::Let(let_))
        } else {
            self.parse_expr()
        }
    }

    fn parse_return(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();

        let expr = match self.token() {
            Some(tok) if self.spans_are_on_same_line(start, tok.span) => {
                Some(Box::new(self.parse_expr()?))
            }
            _ => None,
        };

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
}
