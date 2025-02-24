use crate::diagnostic::Span;

use super::{Path, Ty};

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    List(ListExpr),
    Record(RecordExpr),
    Tuple(TupleExpr),
    Path(Path),
    Index(IndexExpr),
    Field(FieldExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
    Assign(AssignExpr),
    Ref(RefExpr),
    Match(MatchExpr),
    Loop(LoopExpr),
    Break(BreakExpr),
    Let(LetExpr),
    Block(BlockExpr),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::Literal(e) => e.span(),
            Self::List(e) => e.span,
            Self::Record(e) => e.span,
            Self::Tuple(e) => e.span,
            Self::Path(e) => e.span,
            Self::Index(e) => e.span,
            Self::Field(e) => e.span,
            Self::Unary(e) => e.span,
            Self::Binary(e) => e.span,
            Self::Call(e) => e.span,
            Self::Assign(e) => e.span,
            Self::Ref(e) => e.span,
            Self::Match(e) => e.span,
            Self::Loop(e) => e.span,
            Self::Break(e) => e.span,
            Self::Let(e) => e.span,
            Self::Block(e) => e.span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Int { value: i64, span: Span },
    Float { value: f64, span: Span },
    String { value: &'static str, span: Span },
    True { span: Span },
    False { span: Span },
    None { span: Span },
}

impl Literal {
    pub fn span(&self) -> Span {
        match self {
            Self::Int { span, .. } => *span,
            Self::Float { span, .. } => *span,
            Self::String { span, .. } => *span,
            Self::True { span } => *span,
            Self::False { span } => *span,
            Self::None { span } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ListExpr {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct RecordExpr {
    pub fields: Vec<FieldInit>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FieldInit {
    pub name: &'static str,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FieldExpr {
    pub target: Box<Expr>,
    pub name: &'static str,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Deref,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl BinaryOp {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Mul | Self::Div | Self::Mod => 10,
            Self::Add | Self::Sub => 9,
            Self::BitAnd => 6,
            Self::BitOr => 5,
            Self::BitXor => 4,
            Self::Shl | Self::Shr => 3,
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge => 2,
            Self::And => 1,
            Self::Or => 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: Box<Expr>,
    pub input: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct RefExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MatchExpr {
    pub target: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Ty {
        ty: Ty,
        binding: Option<Binding>,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Self::Ty { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Binding {
    Wild {
        span: Span,
    },

    Bind {
        mutable: bool,
        name: &'static str,
        span: Span,
    },

    Tuple {
        bindings: Vec<Binding>,
        span: Span,
    },
}

impl Binding {
    pub fn span(&self) -> Span {
        match self {
            Self::Wild { span } => *span,
            Self::Bind { span, .. } => *span,
            Self::Tuple { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LoopExpr {
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct BreakExpr {
    pub value: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LetExpr {
    pub binding: Binding,
    pub ty: Option<Ty>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub exprs: Vec<Expr>,
    pub span: Span,
}
