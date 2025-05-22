use std::{fmt, mem};

mod app;
mod base;
mod tag;
mod var;

pub use app::App;
pub use base::Base;
pub use tag::{Tag, Tags};
pub use var::{Bounds, Var};

/// A type in disjunctive normal form.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub conjuncts: Vec<Conjunct>,
}

impl Type {
    pub const fn bottom() -> Self {
        Self {
            conjuncts: Vec::new(),
        }
    }

    pub fn top() -> Self {
        Self {
            conjuncts: vec![Conjunct::top()],
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
    pub fn union(mut self, other: Self) -> Self {
        self.conjuncts.extend(other.conjuncts);
        self.simplify_heuristic()
    }

    #[must_use]
    pub fn inter(self, other: Self) -> Self {
        let mut conjuncts = Vec::new();

        for self_conjunct in self.conjuncts {
            for other_conjunct in &other.conjuncts {
                let conjunct = self_conjunct.clone().inter(other_conjunct.clone());
                conjuncts.push(conjunct);
            }
        }

        Self { conjuncts }.simplify_heuristic()
    }

    /// Negate the type.
    #[must_use]
    #[allow(clippy::should_implement_trait)]
    pub fn neg(self) -> Self {
        let mut negative = Self::top();

        for conjunct in self.conjuncts {
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

            negative = negative.inter(Self { conjuncts });
        }

        negative.simplify_heuristic()
    }

    pub fn simplify_heuristic(mut self) -> Self {
        // if we have a conjunct that is ⊤, we can remove all other conjuncts
        if self.conjuncts.iter().any(Conjunct::is_top) {
            self.conjuncts.clear();
            self.conjuncts.push(Conjunct::top());
            return self;
        }

        self.conjuncts.sort_unstable();
        self.conjuncts.dedup();

        self
    }

    pub(crate) fn take(&mut self) -> Self {
        mem::replace(self, Self::bottom())
    }
}

impl From<Conjunct> for Type {
    fn from(conjunct: Conjunct) -> Self {
        Self {
            conjuncts: vec![conjunct],
        }
    }
}

impl From<Term> for Type {
    fn from(term: Term) -> Self {
        Self {
            conjuncts: vec![Conjunct {
                positive: term,
                negative: Term::extreme(),
            }],
        }
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

    pub fn is_extreme(&self) -> bool {
        self.vars.is_empty() && self.apps.is_empty() && self.tags.is_empty() && self.base.is_none()
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
            (true, false) => write!(f, "~{}", self.negative.display(false)),

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
