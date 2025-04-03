#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarAnnotation {
    Ethereal,
    LoopVar,
    LoopVarDep,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum StmtAnnotation {
    Generated,
    Source(&'static str),
}
