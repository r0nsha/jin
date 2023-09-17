use std::collections::HashMap;

use ustr::Ustr;

use crate::{
    ast::{BinOp, CmpOp, UnOp},
    db::DefId,
    hir::{Expr, ExprId, ExprKind, Lit},
};

#[derive(Debug)]
pub struct ConstStorage {
    exprs: HashMap<ExprId, Const>,
    defs: HashMap<DefId, Const>,
}

impl Default for ConstStorage {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstStorage {
    pub fn new() -> Self {
        Self { exprs: HashMap::new(), defs: HashMap::new() }
    }

    #[inline]
    pub fn expr(&self, id: ExprId) -> Option<&Const> {
        self.exprs.get(&id)
    }

    #[inline]
    pub fn def(&self, id: DefId) -> Option<&Const> {
        self.defs.get(&id)
    }

    #[inline]
    #[track_caller]
    pub fn insert_def(&mut self, id: DefId, value: Const) {
        assert!(self.defs.insert(id, value).is_none(), "def const value set twice");
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<(), ConstEvalError> {
        let result = match &expr.kind {
            ExprKind::Name(name) => self.def(name.id).cloned(),
            ExprKind::Block(blk) => (blk.exprs.len() == 1)
                .then(|| self.expr(blk.exprs.last().unwrap().id).cloned())
                .flatten(),
            ExprKind::Unary(un) => self.expr(un.expr.id).map(|val| val.apply_unary(un.op)),
            ExprKind::Binary(bin) => self
                .expr(bin.lhs.id)
                .zip(self.expr(bin.rhs.id))
                .map(|(lhs, rhs)| lhs.apply_binary(rhs, bin.op))
                .transpose()?,
            ExprKind::Lit(lit) => Some(match lit {
                Lit::Str(value) => Const::Str(*value),
                Lit::Int(value) => Const::Int(i128::try_from(*value).unwrap()),
                Lit::Bool(value) => Const::Bool(*value),
                Lit::Unit => Const::Unit,
            }),
            ExprKind::Let(_)
            | ExprKind::If(_)
            | ExprKind::Return(_)
            | ExprKind::Call(_)
            | ExprKind::MemberAccess(_)
            | ExprKind::Cast(_) => None,
        };

        if let Some(result) = result {
            self.exprs.insert(expr.id, result);
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Const {
    Str(Ustr),
    Int(i128),
    Bool(bool),
    Unit,
}

impl Const {
    fn apply_unary(&self, op: UnOp) -> Self {
        match op {
            UnOp::Neg => self.neg(),
            UnOp::Not => self.not(),
        }
    }

    fn neg(&self) -> Self {
        match self {
            Self::Int(v) => Self::Int(-v),
            _ => unreachable!("invalid input in const neg: {:?}", self),
        }
    }

    fn not(&self) -> Self {
        match self {
            Self::Bool(v) => Self::Bool(!v),
            Self::Int(v) => Self::Int(!v),
            _ => unreachable!("invalid input in const not: {:?}", self),
        }
    }

    fn apply_binary(&self, other: &Self, op: BinOp) -> ConstEvalResult {
        match op {
            BinOp::Add => self.add(other),
            BinOp::Sub => self.sub(other),
            BinOp::Mul => self.mul(other),
            BinOp::Div => self.div(other),
            BinOp::Rem => self.rem(other),
            BinOp::Shl => self.shl(other),
            BinOp::Shr => self.shr(other),
            BinOp::BitAnd => Ok(self.bitand(other)),
            BinOp::BitOr => Ok(self.bitor(other)),
            BinOp::BitXor => Ok(self.bitxor(other)),
            BinOp::And => Ok(self.and(other)),
            BinOp::Or => Ok(self.or(other)),
            BinOp::Cmp(cmp) => Ok(self.cmp(other, cmp)),
        }
    }

    fn add(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                Ok(Self::Int(a.checked_add(*b).ok_or(ConstEvalError::Overflow)?))
            }
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn sub(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                Ok(Self::Int(a.checked_sub(*b).ok_or(ConstEvalError::Overflow)?))
            }
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn mul(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                Ok(Self::Int(a.checked_mul(*b).ok_or(ConstEvalError::Overflow)?))
            }
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn div(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                Ok(Self::Int(a.checked_div(*b).ok_or(ConstEvalError::DivByZero)?))
            }
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn rem(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                Ok(Self::Int(a.checked_rem(*b).ok_or(ConstEvalError::RemByZero)?))
            }
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn shl(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Ok(Self::Int(
                u32::try_from(*b)
                    .map_err(|_| ConstEvalError::Overflow)
                    .and_then(|b| a.checked_shl(b).ok_or(ConstEvalError::Overflow))?,
            )),
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn shr(&self, other: &Self) -> ConstEvalResult {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Ok(Self::Int(
                u32::try_from(*b)
                    .map_err(|_| ConstEvalError::Overflow)
                    .and_then(|b| a.checked_shr(b).ok_or(ConstEvalError::Overflow))?,
            )),
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn bitand(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(*a & *b),
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn bitor(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(*a | *b),
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn bitxor(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(*a ^ *b),
            _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
        }
    }

    fn and(&self, other: &Self) -> Const {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => Const::Bool(*a && *b),
            _ => unreachable!("invalid && op on {:?} and {:?}", self, other),
        }
    }

    fn or(&self, other: &Self) -> Const {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => Const::Bool(*a || *b),
            _ => unreachable!("invalid || op on {:?} and {:?}", self, other),
        }
    }

    fn cmp(&self, other: &Self, op: CmpOp) -> Const {
        match (self, other) {
            (&Self::Int(a), &Self::Int(b)) => Const::Bool(match op {
                CmpOp::Eq => a == b,
                CmpOp::Ne => a != b,
                CmpOp::Lt => a < b,
                CmpOp::Le => a <= b,
                CmpOp::Gt => a > b,
                CmpOp::Ge => a >= b,
            }),
            (&Self::Bool(a), &Self::Bool(b)) => Const::Bool(match op {
                CmpOp::Eq => a == b,
                CmpOp::Ne => a != b,
                CmpOp::Lt | CmpOp::Le | CmpOp::Gt | CmpOp::Ge => {
                    unreachable!("invalid op {:?} on {:?} and {:?}", op, self, other)
                }
            }),
            _ => unreachable!("invalid op {:?} on {:?} and {:?}", op, self, other),
        }
    }
}

pub type ConstEvalResult = Result<Const, ConstEvalError>;

#[derive(Debug)]
pub enum ConstEvalError {
    DivByZero,
    RemByZero,
    Overflow,
}
