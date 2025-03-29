use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    fmt,
};

use super::Var;

/// A type in disjunctive normal form.
///
/// ```text
/// t0 | t1 | t2 | ...
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty(pub Vec<Conj>);

impl Ty {
    pub fn int() -> Self {
        Self::tag(Tag::INT)
    }

    pub fn float() -> Self {
        Self::tag(Tag::FLOAT)
    }

    pub fn str() -> Self {
        Self::tag(Tag::STR)
    }

    pub fn true_() -> Self {
        Self::tag(Tag::TRUE)
    }

    pub fn false_() -> Self {
        Self::tag(Tag::FALSE)
    }

    pub fn none() -> Self {
        Self::tag(Tag::NONE)
    }

    pub fn never() -> Self {
        Self(Vec::new())
    }

    pub fn always() -> Self {
        Self(vec![Conj::pos(Term::extreme())])
    }

    pub fn var(var: Var) -> Self {
        Self::term_pos(Term::var(var))
    }

    pub fn tag(tag: Tag) -> Self {
        Self::term_pos(Term::tag(tag))
    }

    pub fn app(tag: Tag, args: impl Into<Vec<Ty>>) -> Self {
        Self::term_pos(Term::app(tag, args))
    }

    pub fn base(base: Base) -> Self {
        Self::term_pos(Term::base(base))
    }

    pub fn record(fields: impl Into<BTreeMap<&'static str, Ty>>) -> Self {
        Self::term_pos(Term::record(fields))
    }

    pub fn tuple(elems: impl Into<Vec<Ty>>) -> Self {
        Self::term_pos(Term::tuple(elems))
    }

    pub fn array(self) -> Self {
        Self::term_pos(Term::array(self))
    }

    pub fn func(self: Ty, output: Ty) -> Self {
        Self::term_pos(Term::func(self, output))
    }

    pub fn ref_(self) -> Self {
        Self::term_pos(Term::ref_(self))
    }

    pub fn union(&mut self, Self(other): Self) {
        self.0.extend(other);
        self.simplify();
    }

    pub fn union_with(mut self, other: Self) -> Self {
        self.union(other);
        self
    }

    pub fn inter(&mut self, Self(other): Self) {
        if other.is_empty() {
            self.0.clear();
            return;
        }

        let mut conjs = Vec::new();

        for conj in self.0.drain(..) {
            for other in other.iter().cloned() {
                let mut conj = conj.clone();
                conj.inter(other);
                conjs.push(conj);
            }
        }

        self.0 = conjs;
        self.simplify();
    }

    pub fn inter_with(mut self, other: Self) -> Self {
        self.inter(other);
        self
    }

    /// Heuristically simplify the type.
    ///
    /// This function will try to remove redundant conjuncts.
    pub fn simplify(&mut self) {
        self.0.sort_unstable();
        self.0.dedup();
    }

    /// Negate the type.
    ///
    /// ```text
    /// ~((a & ~b) |  (c & ~d) | ...) <=>
    ///  ~(a & ~b) & ~(c & ~d) & ...  <=>
    ///   (b | ~a) &  (d | ~c) & ...
    /// ```
    pub fn neg(self) -> Self {
        let mut ty = Self::always();

        for conj in self.0 {
            let mut conjs = Vec::new();

            for var in conj.neg.vars {
                conjs.push(Conj::pos(Term::var(var)));
            }

            for tag in conj.neg.tags {
                conjs.push(Conj::pos(Term::tag(tag)));
            }

            for app in conj.neg.apps {
                conjs.push(Conj::pos(Term::app(app.tag, app.args)));
            }

            if let Some(kind) = conj.neg.base {
                conjs.push(Conj::pos(Term::base(kind)));
            }

            for var in conj.pos.vars {
                conjs.push(Conj::neg(Term::var(var)));
            }

            for tag in conj.pos.tags {
                conjs.push(Conj::neg(Term::tag(tag)));
            }

            for app in conj.pos.apps {
                conjs.push(Conj::neg(Term::app(app.tag, app.args)));
            }

            if let Some(kind) = conj.pos.base {
                conjs.push(Conj::neg(Term::base(kind)));
            }

            ty.inter(Ty(conjs));
        }

        ty
    }

    pub fn conj(pos: Term, neg: Term) -> Self {
        Self(vec![Conj { pos, neg }])
    }

    pub fn term_pos(term: Term) -> Self {
        Self(vec![Conj::pos(term)])
    }

    pub fn term_neg(term: Term) -> Self {
        Self(vec![Conj::neg(term)])
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "⊥");
        }

        let conjs: Vec<_> = self.0.iter().map(ToString::to_string).collect();

        write!(f, "{}", conjs.join(" | "))
    }
}

impl From<Conj> for Ty {
    fn from(conj: Conj) -> Self {
        Self(vec![conj])
    }
}

/// A conjunction in the disjunctive normal form of a [`Ty`].
///
/// ```text
/// pos & ~neg
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Conj {
    pub pos: Term,
    pub neg: Term,
}

impl Conj {
    pub const fn pos(term: Term) -> Self {
        Self {
            pos: term,
            neg: Term::extreme(),
        }
    }

    pub const fn neg(term: Term) -> Self {
        Self {
            pos: Term::extreme(),
            neg: term,
        }
    }

    pub fn is_top(&self) -> bool {
        self.pos.is_extreme() && self.neg.is_extreme()
    }

    /// ```text
    /// (a & ~b) & (c & ~d) <=>
    /// a & c & ~(b | d)
    /// ```
    pub fn inter(&mut self, other: Self) {
        self.pos.inter(other.pos);
        self.neg.union(other.neg);
    }
}

impl fmt::Display for Conj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.pos.is_extreme() {
            return write!(f, "~({})", self.neg.display(false));
        }

        if self.neg.is_extreme() {
            return write!(f, "{}", self.pos.display(true));
        }

        write!(
            f,
            "{} & ~({})",
            self.pos.display(true),
            self.neg.display(false),
        )
    }
}

/// A term in a [`Conj`].
///
/// ```text
/// v0 & v1 & ... & t0 & t1 & ... & a0 & a1 & ... & k
///
/// or
///
/// v0 | v1 | ... | t0 | t1 | ... | a0 | a1 | ... | k
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Term {
    pub vars: BTreeSet<Var>,
    pub tags: BTreeSet<Tag>,
    pub apps: BTreeSet<App>,
    pub base: Option<Base>,
}

impl Term {
    pub const fn extreme() -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: None,
        }
    }

    pub fn is_extreme(&self) -> bool {
        self.vars.is_empty() && self.tags.is_empty() && self.apps.is_empty() && self.base.is_none()
    }

    pub fn var(var: Var) -> Self {
        Self {
            vars: BTreeSet::from([var]),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: None,
        }
    }

    pub fn tag(tag: Tag) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::from([tag]),
            apps: BTreeSet::new(),
            base: None,
        }
    }

    pub fn app(tag: Tag, args: impl Into<Vec<Ty>>) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::from([App::new(tag, args.into())]),
            base: None,
        }
    }

    pub fn base(kind: Base) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: Some(kind),
        }
    }

    pub fn record(fields: impl Into<BTreeMap<&'static str, Ty>>) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: Some(Base::Record(fields.into())),
        }
    }

    pub fn tuple(elems: impl Into<Vec<Ty>>) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: Some(Base::Tuple(elems.into())),
        }
    }

    pub fn array(elem: Ty) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: Some(Base::Array(elem)),
        }
    }

    pub fn func(param: Ty, ret: Ty) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: Some(Base::Func(param, ret)),
        }
    }

    pub fn ref_(elem: Ty) -> Self {
        Self {
            vars: BTreeSet::new(),
            tags: BTreeSet::new(),
            apps: BTreeSet::new(),
            base: Some(Base::Ref(elem)),
        }
    }

    pub fn inter(&mut self, other: Self) {
        self.vars.extend(other.vars);
        self.tags.extend(other.tags);
        self.apps.extend(other.apps);

        if other.base.is_none() {
            return;
        }

        if self.base.is_none() {
            self.base = other.base;
            return;
        }

        let (Some(kind), Some(other)) = (&mut self.base, other.base) else {
            return;
        };

        match (kind, other) {
            (Base::Record(fields), Base::Record(other)) => {
                for (field, other) in other {
                    match fields.entry(field) {
                        Entry::Vacant(entry) => {
                            entry.insert(other);
                        }

                        Entry::Occupied(entry) => {
                            let field = entry.into_mut();
                            field.inter(other);
                        }
                    }
                }
            }

            (Base::Tuple(items), Base::Tuple(other)) if items.len() == other.len() => {
                for (item, other) in items.iter_mut().zip(other) {
                    item.inter(other);
                }
            }

            (Base::Array(item), Base::Array(other)) => {
                item.inter(other);
            }

            (Base::Func(input, output), Base::Func(other_input, other_output)) => {
                input.inter(other_input);
                output.union(other_output);
            }

            (Base::Ref(elem), Base::Ref(other)) => {
                elem.inter(other);
            }

            (_, _) => self.base = None,
        }
    }

    pub fn union(&mut self, other: Self) {
        self.vars.extend(other.vars);
        self.tags.extend(other.tags);
        self.apps.extend(other.apps);

        if other.base.is_none() {
            return;
        }

        if self.base.is_none() {
            self.base = other.base;
            return;
        }

        let (Some(kind), Some(other)) = (&mut self.base, other.base) else {
            return;
        };

        match (kind, other) {
            (Base::Record(fields), Base::Record(other)) => {
                fields.retain(|field, _| !other.contains_key(field));

                for (field, other) in other {
                    if let Some(field) = fields.get_mut(field) {
                        field.union(other);
                    }
                }

                if fields.is_empty() {
                    self.base = None;
                }
            }

            (Base::Tuple(items), Base::Tuple(other)) if items.len() == other.len() => {
                for (item, other) in items.iter_mut().zip(other) {
                    item.union(other);
                }
            }

            (Base::Array(item), Base::Array(other)) => {
                item.union(other);
            }

            (Base::Func(input, output), Base::Func(other_input, other_output)) => {
                input.union(other_input);
                output.inter(other_output);
            }

            (Base::Ref(elem), Base::Ref(other)) => {
                elem.union(other);
            }

            (_, _) => self.base = None,
        }
    }

    pub fn display(&self, positive: bool) -> impl fmt::Display + use<'_> {
        DisplayTerm(self, positive)
    }

    fn fmt_polarized(&self, f: &mut fmt::Formatter<'_>, positive: bool) -> fmt::Result {
        if self.is_extreme() {
            match positive {
                true => return write!(f, "⊤"),
                false => return write!(f, "⊥"),
            }
        }

        let vars: Vec<_> = self.vars.iter().map(ToString::to_string).collect();
        let tags: Vec<_> = self.tags.iter().map(ToString::to_string).collect();
        let apps: Vec<_> = self.apps.iter().map(ToString::to_string).collect();

        let mut items = Vec::new();

        let infix = if positive { " & " } else { " | " };

        if !vars.is_empty() {
            items.push(vars.join(infix));
        }

        if !tags.is_empty() {
            items.push(tags.join(infix));
        }

        if !apps.is_empty() {
            items.push(apps.join(infix));
        }

        if let Some(kind) = &self.base {
            items.push(kind.to_string());
        }

        write!(f, "{}", items.join(infix))
    }
}

struct DisplayTerm<'a>(&'a Term, bool);

impl fmt::Display for DisplayTerm<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt_polarized(f, self.1)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Base {
    Record(BTreeMap<&'static str, Ty>),
    Tuple(Vec<Ty>),
    Array(Ty),
    Func(Ty, Ty),
    Ref(Ty),
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Base::Record(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|(field, ty)| format!("{}: {}", field, ty))
                    .collect();

                write!(f, "{{{}}}", fields.join(", "))
            }

            Base::Tuple(items) => {
                let items: Vec<_> = items.iter().map(ToString::to_string).collect();

                write!(f, "{}", items.join(" * "))
            }

            Base::Array(ty) => write!(f, "[{}]", ty),

            Base::Func(input, output) => write!(f, "{} -> {}", input, output),

            Base::Ref(ty) => write!(f, "ref ({})", ty),
        }
    }
}

/// A type tag.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag {
    pub name: &'static str,
    pub data: u64,
}

impl Tag {
    pub const INT: Self = Self::new("int", u64::MAX);
    pub const FLOAT: Self = Self::new("float", u64::MAX);
    pub const STR: Self = Self::new("str", u64::MAX);

    pub const TRUE: Self = Self::new("true", u64::MAX);
    pub const FALSE: Self = Self::new("false", u64::MAX);
    pub const NONE: Self = Self::new("none", u64::MAX);

    pub const fn new(name: &'static str, data: u64) -> Self {
        Self { name, data }
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.name)
    }
}

/// A type application.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct App {
    pub tag: Tag,
    pub args: Vec<Ty>,
}

impl App {
    pub fn new(tag: Tag, args: Vec<Ty>) -> Self {
        Self { tag, args }
    }
}

impl fmt::Display for App {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args: Vec<_> = self.args.iter().map(ToString::to_string).collect();

        write!(f, "{}<{}>", self.tag.name, args.join(", "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int() -> Term {
        Term::tag(Tag::INT)
    }

    fn float() -> Term {
        Term::tag(Tag::FLOAT)
    }

    fn none() -> Term {
        Term::tag(Tag::NONE)
    }

    fn top() -> Term {
        Term::extreme()
    }

    fn pos(term: Term) -> Conj {
        Conj::pos(term)
    }

    fn neg(term: Term) -> Conj {
        Conj::neg(term)
    }

    impl std::ops::BitAnd for Term {
        type Output = Term;

        fn bitand(mut self, rhs: Self) -> Self::Output {
            self.inter(rhs);
            self
        }
    }

    impl std::ops::BitAnd for Conj {
        type Output = Conj;

        fn bitand(mut self, rhs: Self) -> Self::Output {
            self.inter(rhs);
            self
        }
    }

    #[test]
    fn test_term_subty() {
        assert!(int().is_subty_pos(&top()));
        assert!(int().is_subty_pos(&int()));
        assert!((int() & float()).is_subty_pos(&int()));
        assert!(!int().is_subty_pos(&(int() & float())));
    }

    #[test]
    fn test_conj_subty() {
        assert!(pos(int()).is_subty(&pos(top())));
        assert!(pos(int()).is_subty(&pos(int())));
        assert!((pos(int()) & pos(float())).is_subty(&pos(int())));
        assert!(!pos(int()).is_subty(&neg(int())));
    }
}
