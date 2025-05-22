use solve::Tag;

#[derive(Clone, Debug)]
pub struct Expr {}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    String(&'static str),
    ZeroSize(Tag),
    Tag(Tag, Box<Expr>),
}
