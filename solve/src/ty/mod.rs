use std::{
    cell::RefCell,
    cmp,
    collections::HashMap,
    fmt,
    hash::{BuildHasherDefault, Hash, Hasher},
    mem,
    sync::{
        Arc,
        atomic::{self, AtomicU64},
    },
};

mod app;
mod base;
mod tag;
mod var;

pub use app::App;
pub use base::Base;
pub use tag::{Tag, Tags};
pub use var::{Bounds, Var};

use seahash::SeaHasher;

macro_rules! cache {
    ($key:expr, $key_type:ty, $complexity:expr, $result:expr $(,)?) => {{
        thread_local! {
            static CACHE: RefCell<HashMap<$key_type, Type, BuildHasherDefault<SeaHasher>>> = Default::default();
        }

        if $complexity < 16 {
            return $result;
        }

        if let Some(cached) = CACHE.with_borrow(|cache| cache.get(&$key).cloned()) {
            return cached;
        }

        let key = $key.clone();
        let result = $result;

        CACHE.with_borrow_mut(|cache| {
            cache.insert(key, result.clone());
        });

        result
    }};
}

/// A type in disjunctive normal form.
#[derive(Debug)]
pub struct Type {
    hash: AtomicU64,
    conjuncts: Arc<Vec<Conjunct>>,
}

impl Type {
    pub fn bottom() -> Self {
        Self {
            hash: AtomicU64::new(0),
            conjuncts: Arc::new(Vec::new()),
        }
    }

    pub fn top() -> Self {
        Self {
            hash: AtomicU64::new(0),
            conjuncts: Arc::new(vec![Conjunct::top()]),
        }
    }

    pub fn fresh_var() -> Self {
        Self::var(Var::fresh())
    }

    pub fn int() -> Self {
        Self::tag(Tag::INT)
    }

    pub fn float() -> Self {
        Self::tag(Tag::FLOAT)
    }

    pub fn str() -> Self {
        Self::tag(Tag::STR)
    }

    pub fn none() -> Self {
        Self::tag(Tag::NONE)
    }

    pub fn true_() -> Self {
        Self::tag(Tag::TRUE)
    }

    pub fn false_() -> Self {
        Self::tag(Tag::FALSE)
    }

    pub fn boolean() -> Self {
        Self::union(Self::true_(), Self::false_())
    }

    pub fn tag(tag: Tag) -> Self {
        Self::from(Term::from(tag))
    }

    pub fn var(var: Var) -> Self {
        Self::from(Term::from(var))
    }

    pub fn app(app: App) -> Self {
        Self::from(Term::from(app))
    }

    pub fn record(fields: Vec<(&'static str, Self)>) -> Self {
        Self::from(Term::from(Base::Record { fields }))
    }

    pub fn tuple(fields: Vec<Self>) -> Self {
        Self::from(Term::from(Base::Tuple { fields }))
    }

    pub fn function(self, other: Self) -> Self {
        Self::from(Term::from(Base::Function {
            input: self,
            output: other,
        }))
    }

    pub fn array(self) -> Self {
        Self::from(Term::from(Base::Array { element: self }))
    }

    pub fn conjuncts(&self) -> &[Conjunct] {
        &self.conjuncts
    }

    pub fn conjuncts_mut(&mut self) -> &mut Vec<Conjunct> {
        self.invalidate_hash();
        Arc::make_mut(&mut self.conjuncts)
    }

    pub fn into_conjuncts(self) -> Vec<Conjunct> {
        Arc::unwrap_or_clone(self.conjuncts)
    }

    pub fn union_mut(&mut self, other: Self) {
        *self = self.take().union(other);
    }

    pub fn inter_mut(&mut self, other: Self) {
        *self = self.take().inter(other);
    }

    pub fn neg_mut(&mut self) {
        *self = self.take().neg();
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        cache!(
            (self.clone(), other.clone()),
            (Type, Type),
            (self.complexity() + other.complexity()),
            self.union_impl(other)
        )
    }

    fn union_impl(mut self, other: Self) -> Self {
        self.conjuncts_mut().extend(other.into_conjuncts());
        self.simplify_heuristic()
    }

    #[must_use]
    pub fn inter(self, other: Self) -> Self {
        cache!(
            (self.clone(), other.clone()),
            (Type, Type),
            (self.complexity() + other.complexity()),
            self.inter_impl(other)
        )
    }

    fn inter_impl(self, other: Self) -> Self {
        let mut conjuncts = Vec::new();

        for self_conjunct in self.into_conjuncts() {
            for other_conjunct in other.conjuncts() {
                let conjunct = self_conjunct.clone().inter(other_conjunct.clone());
                conjuncts.push(conjunct);
            }
        }

        Self {
            hash: AtomicU64::new(0),
            conjuncts: Arc::new(conjuncts),
        }
        .simplify_heuristic()
    }

    /// Negate the type.
    #[must_use]
    #[allow(clippy::should_implement_trait)]
    pub fn neg(self) -> Self {
        cache!(self, Type, self.complexity(), self.neg_impl())
    }

    fn neg_impl(self) -> Self {
        let mut negative = Self::top();

        for conjunct in self.into_conjuncts() {
            let mut conjuncts = Vec::new();

            for tag in conjunct.positive.tags.iter() {
                conjuncts.push(Conjunct {
                    positive: Term::extreme(),
                    negative: Term::from(tag),
                })
            }

            for var in conjunct.positive.vars {
                conjuncts.push(Conjunct {
                    positive: Term::extreme(),
                    negative: Term::from(var),
                })
            }

            for app in conjunct.positive.apps {
                conjuncts.push(Conjunct {
                    positive: Term::extreme(),
                    negative: Term::from(app),
                })
            }

            if !conjunct.positive.base.is_none() {
                conjuncts.push(Conjunct {
                    positive: Term::extreme(),
                    negative: Term::from(conjunct.positive.base),
                });
            }

            for tag in conjunct.negative.tags.iter() {
                conjuncts.push(Conjunct {
                    positive: Term::from(tag),
                    negative: Term::extreme(),
                })
            }

            for var in conjunct.negative.vars {
                conjuncts.push(Conjunct {
                    positive: Term::from(var),
                    negative: Term::extreme(),
                })
            }

            for app in conjunct.negative.apps {
                conjuncts.push(Conjunct {
                    positive: Term::from(app),
                    negative: Term::extreme(),
                })
            }

            if !conjunct.negative.base.is_none() {
                conjuncts.push(Conjunct {
                    positive: Term::from(conjunct.negative.base),
                    negative: Term::extreme(),
                });
            }

            let term = Self {
                hash: AtomicU64::new(0),
                conjuncts: Arc::new(conjuncts),
            };

            negative = negative.inter(term);
        }

        negative
    }

    #[inline(always)]
    pub fn simplify_heuristic(self) -> Self {
        cache!(
            self,
            Type,
            self.complexity(),
            self.simplify_heuristic_impl(),
        )
    }

    fn simplify_heuristic_impl(mut self) -> Self {
        let conjuncts = self.conjuncts_mut();

        for i in 0..conjuncts.len() {
            let mut j = i + 1;

            while j < conjuncts.len() {
                if conjuncts[i].is_subtype_heuristic(&conjuncts[j]) {
                    conjuncts.remove(i);
                    break;
                }

                if conjuncts[j].is_subtype_heuristic(&conjuncts[i]) {
                    conjuncts.remove(j);
                } else {
                    j += 1;
                }
            }
        }

        self
    }

    pub fn is_subtype_heuristic(&self, other: &Self) -> bool {
        for self_conjunct in self.conjuncts() {
            let mut is_subtype = false;

            for other_conjunct in other.conjuncts() {
                if self_conjunct.is_subtype_heuristic(other_conjunct) {
                    is_subtype = true;
                    break;
                }
            }

            if !is_subtype {
                return false;
            }
        }

        true
    }

    pub fn complexity(&self) -> usize {
        self.conjuncts.iter().map(Conjunct::complexity).sum()
    }

    #[inline(always)]
    pub fn hash(&self) -> u64 {
        let hash = self.hash.load(atomic::Ordering::Relaxed);

        if hash == 0 {
            let mut hasher = SeaHasher::new();

            for conjunct in self.conjuncts() {
                Hash::hash(conjunct, &mut hasher);
            }

            let hash = hasher.finish();
            self.hash.store(hash, atomic::Ordering::Relaxed);

            hash
        } else {
            hash
        }
    }

    #[inline(always)]
    fn invalidate_hash(&self) {
        self.hash.store(0, atomic::Ordering::Relaxed);
    }

    pub(crate) fn take(&mut self) -> Self {
        mem::replace(self, Self::bottom())
    }
}

impl Clone for Type {
    #[inline(always)]
    fn clone(&self) -> Self {
        let hash = self.hash.load(atomic::Ordering::Relaxed);

        Self {
            hash: AtomicU64::new(hash),
            conjuncts: Arc::clone(&self.conjuncts),
        }
    }
}

impl PartialEq for Type {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.conjuncts == other.conjuncts
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.hash().cmp(&other.hash())
    }
}

impl Hash for Type {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash());
    }
}

impl From<Vec<Conjunct>> for Type {
    fn from(conjuncts: Vec<Conjunct>) -> Self {
        Self {
            hash: AtomicU64::new(0),
            conjuncts: Arc::new(conjuncts),
        }
    }
}

impl From<Conjunct> for Type {
    fn from(conjunct: Conjunct) -> Self {
        Self::from(vec![conjunct])
    }
}

impl From<Term> for Type {
    fn from(term: Term) -> Self {
        Self::from(vec![Conjunct {
            positive: term,
            negative: Term::extreme(),
        }])
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Conjunct {
    pub positive: Term,
    pub negative: Term,
}

impl Conjunct {
    pub const fn top() -> Self {
        Self {
            positive: Term::extreme(),
            negative: Term::extreme(),
        }
    }

    pub fn is_top(&self) -> bool {
        self.positive.is_extreme() && self.negative.is_extreme()
    }

    pub fn inter(self, other: Self) -> Self {
        let positive = self.positive.inter_positive(other.positive);
        let negative = self.negative.inter_negative(other.negative);

        Self { positive, negative }
    }

    pub fn vars(&self) -> impl DoubleEndedIterator<Item = Var> + '_ {
        let pos = self.positive.vars.iter();
        let neg = self.negative.vars.iter();

        pos.chain(neg).copied()
    }

    pub fn vars_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Var> + '_ {
        let pos = self.positive.vars.iter_mut();
        let neg = self.negative.vars.iter_mut();

        pos.chain(neg)
    }

    pub fn apps(&self) -> impl DoubleEndedIterator<Item = &App> + '_ {
        let pos = self.positive.apps.iter();
        let neg = self.negative.apps.iter();

        pos.chain(neg)
    }

    pub fn apps_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut App> + '_ {
        let pos = self.positive.apps.iter_mut();
        let neg = self.negative.apps.iter_mut();

        pos.chain(neg)
    }

    pub fn bases(&self) -> impl DoubleEndedIterator<Item = &Base> + '_ {
        [&self.positive.base, &self.negative.base].into_iter()
    }

    pub fn bases_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Base> + '_ {
        [&mut self.positive.base, &mut self.negative.base].into_iter()
    }

    pub fn is_subtype_heuristic(&self, other: &Self) -> bool {
        (self.positive.is_extreme() && other.positive.is_extreme()
            || self.negative.is_extreme() && other.negative.is_extreme())
            && self.positive.is_subtype_heuristic(&other.positive)
            && self.negative.is_subtype_heuristic(&other.negative)
    }

    pub fn complexity(&self) -> usize {
        self.positive.complexity() + self.negative.complexity()
    }

    pub fn hash(&self) -> u64 {
        let mut hasher = SeaHasher::new();
        Hash::hash(self, &mut hasher);
        hasher.finish()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Term {
    pub vars: Vec<Var>,
    pub apps: Vec<App>,

    pub tags: Tags,
    pub base: Base,
}

impl Term {
    pub const fn extreme() -> Self {
        Self {
            vars: Vec::new(),
            apps: Vec::new(),

            tags: Tags::new(),
            base: Base::None,
        }
    }

    pub fn complexity(&self) -> usize {
        self.vars.len() + self.apps.len() + self.tags.len() + self.base.complexity()
    }

    pub fn is_extreme(&self) -> bool {
        self.vars.is_empty() && self.apps.is_empty() && self.tags.is_empty() && self.base.is_none()
    }

    /// Check if `self` is a subtype of `other` using a heuristic.
    pub fn is_subtype_heuristic(&self, other: &Self) -> bool {
        self.tags.is_subset(&other.tags)
            && other.vars.iter().all(|v| self.vars.contains(v))
            && other.apps.iter().all(|a| self.apps.contains(a))
            && self.base.is_subtype_heuristic(&other.base)
    }

    pub fn display(&self, polarity: bool) -> impl fmt::Display + '_ {
        TermDisplay(self, polarity)
    }

    /// Compute the intersection of two terms in positive position.
    fn inter_positive(mut self, other: Self) -> Self {
        self.vars.extend(other.vars);
        self.apps.extend(other.apps);

        self.vars.sort_unstable();
        self.apps.sort_unstable();
        self.vars.dedup();
        self.apps.dedup();

        self.tags.union(&other.tags);
        self.base = self.base.inter(other.base);

        self
    }

    /// Compute the intersection of two terms in negative position.
    fn inter_negative(mut self, other: Self) -> Self {
        self.vars.extend(other.vars);
        self.apps.extend(other.apps);

        self.vars.sort_unstable();
        self.apps.sort_unstable();
        self.vars.dedup();
        self.apps.dedup();

        self.tags.union(&other.tags);
        self.base = self.base.union(other.base);

        self
    }
}

impl From<Var> for Term {
    fn from(var: Var) -> Self {
        Self {
            vars: vec![var],
            ..Self::extreme()
        }
    }
}

impl From<App> for Term {
    fn from(app: App) -> Self {
        Self {
            apps: vec![app],
            ..Self::extreme()
        }
    }
}

impl From<Tag> for Term {
    fn from(tag: Tag) -> Self {
        Self {
            tags: Tags::from(tag),
            ..Self::extreme()
        }
    }
}

impl From<Base> for Term {
    fn from(base: Base) -> Self {
        Self {
            base,
            ..Self::extreme()
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.conjuncts.is_empty() {
            return write!(f, "⊥");
        }

        let conjuncts = self
            .conjuncts
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(" ⊔ ");

        write!(f, "{}", conjuncts)
    }
}

impl fmt::Display for Conjunct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.positive.is_extreme(), self.negative.is_extreme()) {
            (true, true) => write!(f, "⊤"),

            (false, true) => write!(f, "{}", self.positive.display(true)),
            (true, false) => write!(f, "~({})", self.negative.display(false)),

            (false, false) => {
                let pos = self.positive.display(true);
                let neg = self.negative.display(false);

                write!(f, "{} ⊓ ~({})", pos, neg)
            }
        }
    }
}

struct TermDisplay<'a>(&'a Term, bool);

impl fmt::Display for TermDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TermDisplay(term, polarity) = self;

        if term.is_extreme() {
            match polarity {
                true => return write!(f, "⊤"),
                false => return write!(f, "⊥"),
            }
        }

        let infix = if *polarity { " ⊓ " } else { " ⊔ " };

        let vars = term
            .vars
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(infix);

        let apps = term
            .apps
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(infix);

        let tags = term
            .tags
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(infix);

        let mut needs_infix = false;

        if !vars.is_empty() {
            write!(f, "{vars}")?;
            needs_infix = true;
        }

        if !apps.is_empty() {
            if needs_infix {
                write!(f, "{infix}")?;
            }

            write!(f, "{apps}")?;
            needs_infix = true;
        }

        if !tags.is_empty() {
            if needs_infix {
                write!(f, "{infix}")?;
            }

            write!(f, "{tags}")?;
            needs_infix = true;
        }

        if !term.base.is_none() {
            if needs_infix {
                write!(f, "{infix}")?;
            }

            write!(f, "{}", term.base)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_subtype_heuristic() {
        let a = Type::int();
        let b = Type::float();

        assert!(a.is_subtype_heuristic(&a));
        assert!(!a.is_subtype_heuristic(&b));
        assert!(!b.is_subtype_heuristic(&a));

        let b = b.union(a.clone());

        assert!(a.is_subtype_heuristic(&b));
        assert!(!b.is_subtype_heuristic(&a));

        let b = Type::int().inter(Type::float());

        assert!(!a.is_subtype_heuristic(&b));
        assert!(b.is_subtype_heuristic(&a));
    }
}
