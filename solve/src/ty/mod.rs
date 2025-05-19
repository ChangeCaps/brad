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

    pub fn union(mut self, other: Self) -> Self {
        self.conjuncts.extend(other.conjuncts);
        self
    }

    pub fn inter(self, other: Self) -> Self {
        let mut conjuncts = Vec::new();

        for self_conjunct in self.conjuncts {
            for other_conjunct in &other.conjuncts {
                let conjunct = self_conjunct.clone().intersection(other_conjunct.clone());
                conjuncts.push(conjunct);
            }
        }

        Self { conjuncts }
    }

    /// Negate the type.
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

        negative
    }

    fn take(&mut self) -> Self {
        mem::replace(self, Self::bottom())
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

    pub fn intersection(self, other: Self) -> Self {
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
        self.tags.is_empty() && self.base == Base::None
    }

    pub fn display(&self, polarity: bool) -> impl fmt::Display + '_ {
        TermDisplay(self, polarity)
    }

    /// Compute the intersection of two terms in positive position.
    fn inter_positive(mut self, other: Self) -> Self {
        self.vars.extend(other.vars);
        self.apps.extend(other.apps);

        self.tags.union(&other.tags);
        self.base = self.base.inter(other.base);

        self
    }

    /// Compute the intersection of two terms in negative position.
    fn inter_negative(mut self, other: Self) -> Self {
        self.vars.extend(other.vars);
        self.apps.extend(other.apps);

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
            (true, false) => write!(f, "{}", self.negative.display(false)),

            (false, false) => {
                let pos = self.positive.display(true);
                let neg = self.negative.display(false);

                write!(f, "{} ⊓ {}", pos, neg)
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

        write!(f, "{vars}")?;

        if !apps.is_empty() {
            write!(f, "{infix}{apps}")?;
        }

        if !tags.is_empty() {
            write!(f, "{infix}{tags}")?;
        }

        if !term.base.is_none() {
            write!(f, "{infix}{}", term.base)?;
        }

        Ok(())
    }
}
