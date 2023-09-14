use std::collections::HashMap;

use crate::{
    ast::{BinOp, CmpOp, UnOp},
    hir::{Expr, ExprId, ExprKind, Lit},
};

#[derive(Debug)]
pub struct ConstStorage {
    exprs: HashMap<ExprId, Const>,
}

impl Default for ConstStorage {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstStorage {
    pub fn new() -> Self {
        Self { exprs: HashMap::new() }
    }

    #[inline]
    pub fn expr(&self, id: ExprId) -> Option<&Const> {
        self.exprs.get(&id)
    }

    // TODO: errors: divide by zero
    // TODO: errors: remainder by zero
    // TODO: errors: overflow
    pub fn eval_expr(&mut self, expr: &Expr) {
        // TODO: const eval block
        // TODO: const eval cast
        // TODO: const eval name

        let result = match &expr.kind {
            ExprKind::Let(_)
            | ExprKind::If(_)
            | ExprKind::Return(_)
            | ExprKind::Call(_)
            | ExprKind::Cast(_)
            | ExprKind::Block(_)
            | ExprKind::Name(_) => None,
            ExprKind::Unary(un) => self.expr(un.expr.id).map(|val| val.apply_unary(un.op)),
            ExprKind::Binary(bin) => self
                .expr(bin.lhs.id)
                .zip(self.expr(bin.rhs.id))
                .map(|(lhs, rhs)| lhs.apply_binary(rhs, bin.op)),
            ExprKind::Lit(lit) => Some(match lit {
                Lit::Int(value) => Const::Int(i128::try_from(*value).unwrap()),
                Lit::Bool(value) => Const::Bool(*value),
                Lit::Unit => Const::Unit,
            }),
        };

        if let Some(result) = result {
            self.exprs.insert(expr.id, result);
        }
    }
}

#[derive(Debug, Clone)]
pub enum Const {
    Int(i128),
    Bool(bool),
    Unit,
}

macro_rules! impl_const_op {
    ($name: ident, $op: tt) => {
        fn $name(&self, other: &Self) -> Self {
            match (self, other) {
                (Self::Int(a), Self::Int(b)) => Self::Int(*a $op *b),
                _ => unreachable!("invalid const binary op on {:?} and {:?}", self, other),
            }
        }
    };
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
            Self::Unit => unreachable!("invalid input in const not: {:?}", self),
        }
    }

    fn apply_binary(&self, other: &Self, op: BinOp) -> Self {
        match op {
            BinOp::Add => self.add(other),
            BinOp::Sub => self.sub(other),
            BinOp::Mul => self.mul(other),
            BinOp::Div => self.div(other),
            BinOp::Rem => self.rem(other),
            BinOp::Shl => self.shl(other),
            BinOp::Shr => self.shr(other),
            BinOp::BitAnd => self.bitand(other),
            BinOp::BitOr => self.bitor(other),
            BinOp::BitXor => self.bitxor(other),
            BinOp::And => self.and(other),
            BinOp::Or => self.or(other),
            BinOp::Cmp(cmp) => self.cmp(other, cmp),
        }
    }

    impl_const_op!(add, +);
    impl_const_op!(sub, -);
    impl_const_op!(mul,*);
    impl_const_op!(div, /);
    impl_const_op!(rem, %);
    impl_const_op!(shl, <<);
    impl_const_op!(shr, >>);
    impl_const_op!(bitand, &);
    impl_const_op!(bitor, |);
    impl_const_op!(bitxor , ^);

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
