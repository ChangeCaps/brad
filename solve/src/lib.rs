mod tcx;
mod ty;

pub use tcx::Tcx;
pub use ty::{
    App, Array, Base, Bounds, Conjunct, Function, Record, Tag, Tags, Term, Tuple, Type, Var,
};
