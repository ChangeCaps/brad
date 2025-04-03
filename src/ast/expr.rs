use crate::diagnostic::Span;

use super::{Path, Spanned, Ty};

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
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
    Lambda(LambdaExpr),
    Assign(AssignExpr),
    Ref(RefExpr),
    Match(MatchExpr),
    Loop(LoopExpr),
    Break(BreakExpr),
    Let(LetExpr),
    Block(BlockExpr),
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span(),
            Self::List(list) => list.span(),
            Self::Record(record) => record.span(),
            Self::Tuple(tuple) => tuple.span(),
            Self::Path(path) => path.span(),
            Self::Index(index) => index.span(),
            Self::Field(field) => field.span(),
            Self::Unary(unary) => unary.span(),
            Self::Binary(binary) => binary.span(),
            Self::Call(call) => call.span(),
            Self::Lambda(lambda) => lambda.span(),
            Self::Assign(assign) => assign.span(),
            Self::Ref(ref_expr) => ref_expr.span(),
            Self::Match(match_expr) => match_expr.span(),
            Self::Loop(loop_expr) => loop_expr.span(),
            Self::Break(break_expr) => break_expr.span(),
            Self::Let(let_expr) => let_expr.span(),
            Self::Block(block) => block.span(),
        }
    }

    fn reset_spans(&mut self) {
        match self {
            Self::Literal(lit) => lit.reset_spans(),
            Self::List(list) => list.reset_spans(),
            Self::Record(record) => record.reset_spans(),
            Self::Tuple(tuple) => tuple.reset_spans(),
            Self::Path(path) => path.reset_spans(),
            Self::Index(index) => index.reset_spans(),
            Self::Field(field) => field.reset_spans(),
            Self::Unary(unary) => unary.reset_spans(),
            Self::Binary(binary) => binary.reset_spans(),
            Self::Call(call) => call.reset_spans(),
            Self::Lambda(lambda) => lambda.reset_spans(),
            Self::Assign(assign) => assign.reset_spans(),
            Self::Ref(ref_expr) => ref_expr.reset_spans(),
            Self::Match(match_expr) => match_expr.reset_spans(),
            Self::Loop(loop_expr) => loop_expr.reset_spans(),
            Self::Break(break_expr) => break_expr.reset_spans(),
            Self::Let(let_expr) => let_expr.reset_spans(),
            Self::Block(block) => block.reset_spans(),
        }
    }
}

impl Expr {
    pub fn empty(span: Span) -> Self {
        Self::Block(BlockExpr {
            exprs: Vec::new(),
            span,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int { value: i64, span: Span },
    Float { value: f64, span: Span },
    String { value: &'static str, span: Span },
    True { span: Span },
    False { span: Span },
    None { span: Span },
}

impl Spanned for Literal {
    fn span(&self) -> Span {
        match self {
            Self::Int { span, .. } => *span,
            Self::Float { span, .. } => *span,
            Self::String { span, .. } => *span,
            Self::True { span } => *span,
            Self::False { span } => *span,
            Self::None { span } => *span,
        }
    }

    fn reset_spans(&mut self) {
        match self {
            Self::Int { span, .. } => *span = Span::default(),
            Self::Float { span, .. } => *span = Span::default(),
            Self::String { span, .. } => *span = Span::default(),
            Self::True { span } => *span = Span::default(),
            Self::False { span } => *span = Span::default(),
            Self::None { span } => *span = Span::default(),
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "{}", value),
            Self::Float { value, .. } => write!(f, "{}", value),
            Self::String { value, .. } => write!(f, "{:?}", value),
            Self::True { .. } => write!(f, "true"),
            Self::False { .. } => write!(f, "false"),
            Self::None { .. } => write!(f, "none"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ListExpr {
    pub items: Vec<Expr>,
    pub span: Span,
}

impl Spanned for ListExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.items.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordExpr {
    pub fields: Vec<FieldInit>,
    pub span: Span,
}

impl Spanned for RecordExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.fields.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldInit {
    pub name: &'static str,
    pub value: Expr,
    pub span: Span,
}

impl Spanned for FieldInit {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.value.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

impl Spanned for IndexExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
        self.index.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
    pub span: Span,
}

impl Spanned for TupleExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.items.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldExpr {
    pub target: Box<Expr>,
    pub name: &'static str,
    pub span: Span,
}

impl FieldExpr {
    pub fn new(target: Expr, name: &'static str, span: Span) -> Self {
        Self {
            target: Box::new(target),
            name,
            span,
        }
    }
}

impl Spanned for FieldExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub target: Box<Expr>,
    pub span: Span,
}

impl Spanned for UnaryExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Deref,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::BitNot => write!(f, "~"),
            Self::Deref => write!(f, "*"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
    pub span: Span,
}

impl Spanned for BinaryExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.lhs.reset_spans();
        self.rhs.reset_spans();
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpr {
    pub target: Box<Expr>,
    pub input: Box<Expr>,
    pub span: Span,
}

impl Spanned for CallExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
        self.input.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaExpr {
    pub args: Vec<Binding>,
    pub body: Box<Expr>,
    pub span: Span,
}

impl Spanned for LambdaExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.args.reset_spans();
        self.body.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

impl Spanned for AssignExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
        self.value.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefExpr {
    pub target: Box<Expr>,
    pub span: Span,
}

impl Spanned for RefExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr {
    pub target: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

impl Spanned for MatchExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.target.reset_spans();
        self.arms.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

impl Spanned for MatchArm {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.pattern.reset_spans();
        self.body.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Ty {
        ty: Ty,
        binding: Option<Binding>,
        span: Span,
    },
}

impl Spanned for Pattern {
    fn span(&self) -> Span {
        match self {
            Self::Ty { span, .. } => *span,
        }
    }

    fn reset_spans(&mut self) {
        match self {
            Self::Ty { span, ty, binding } => {
                *span = Span::default();
                ty.reset_spans();
                binding.reset_spans();
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl Spanned for Binding {
    fn span(&self) -> Span {
        match self {
            Self::Wild { span } => *span,
            Self::Bind { span, .. } => *span,
            Self::Tuple { span, .. } => *span,
        }
    }

    fn reset_spans(&mut self) {
        match self {
            Self::Wild { span } => *span = Span::default(),
            Self::Bind { span, .. } => *span = Span::default(),
            Self::Tuple { span, bindings } => {
                *span = Span::default();
                bindings.reset_spans();
            }
        }
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct LoopExpr {
    pub body: Box<Expr>,
    pub span: Span,
}

impl Spanned for LoopExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.body.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BreakExpr {
    pub value: Option<Box<Expr>>,
    pub span: Span,
}

impl Spanned for BreakExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.value.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetExpr {
    pub binding: Binding,
    pub ty: Option<Ty>,
    pub value: Box<Expr>,
    pub span: Span,
}

impl Spanned for LetExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.binding.reset_spans();
        self.ty.reset_spans();
        self.value.reset_spans();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockExpr {
    pub exprs: Vec<Expr>,
    pub span: Span,
}

impl Spanned for BlockExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn reset_spans(&mut self) {
        self.span = Span::default();
        self.exprs.reset_spans();
    }
}
