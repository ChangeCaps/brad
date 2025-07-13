use diagnostic::Span;
use solve::{Tag, Type};

use super::{BodyId, LocalId, Locals};

#[derive(Clone, Debug)]
pub struct Expr<T = Type> {
    pub kind: ExprKind<T>,
    pub ty: T,
    pub span: Span,
}

impl Expr<Type> {
    pub fn none(span: Span) -> Self {
        Self {
            kind: ExprKind::ZeroSize(Tag::NONE),
            ty: Type::none(),
            span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind<T = Type> {
    Int(i64),
    Float(f64),
    ZeroSize(Tag),
    String(&'static str),
    Local(LocalId),
    Tag(Tag, Box<Expr<T>>),
    Func(BodyId),
    Array(Vec<Expr<T>>),
    Tuple(Vec<Expr<T>>),
    Record(Vec<Init<T>>),
    Index(Box<Expr<T>>, Box<Expr<T>>),
    Field(Box<Expr<T>>, &'static str),
    Unary(UnaryOp, Box<Expr<T>>),
    Binary(BinaryOp, Box<Expr<T>>, Box<Expr<T>>),
    Call(Box<Expr<T>>, Box<Expr<T>>),

    Lambda {
        /// The locals that are captured by the lambda.
        captures: Vec<Capture>,

        /// The arguments to the lambda.
        args: Vec<Binding>,

        /// The locals that are defined by the lambda.
        locals: Locals<T>,

        /// The body of the lambda.
        body: Box<Expr<T>>,
    },

    Assign(Box<Expr<T>>, Box<Expr<T>>),
    Ref(Box<Expr<T>>),
    Match(Box<Expr<T>>, MatchBody<T>),
    Loop(Box<Expr<T>>),
    Break(Option<Box<Expr<T>>>),
    Let(Binding, Box<Expr<T>>),
    Block(Vec<Expr<T>>),
}

#[derive(Clone, Debug)]
pub struct Capture {
    /// The local in the inner context.
    pub inner: LocalId,

    /// The captured local in the outer context.
    pub outer: LocalId,
}

#[derive(Clone, Debug)]
pub struct Init<T = Type> {
    pub name: &'static str,
    pub value: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MatchBody<T = Type> {
    pub arms: Vec<Arm<T>>,
    pub default: Option<Box<(Binding, Expr<T>)>>,
}

#[derive(Clone, Debug)]
pub struct Arm<T = Type> {
    pub pattern: Pattern,
    pub body: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Tag {
        tag: Tag,
        binding: Binding,
        span: Span,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Deref,
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

#[derive(Clone, Debug)]
pub enum Binding {
    Wild { span: Span },

    Bind { local: LocalId, span: Span },

    Tuple { bindings: Vec<Binding>, span: Span },
}
