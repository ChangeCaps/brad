use std::{collections::HashMap, fmt, path::Path};

use crate::{
    diagnostic::{Diagnostic, Source, Sources},
    parse::{Interner, Token, Tokens},
};

mod parse;

pub fn solve(sources: &mut Sources, path: impl AsRef<Path>) -> Result<(), Diagnostic> {
    let content = std::fs::read_to_string(path.as_ref()).unwrap();

    let source = Source {
        content,
        file: path.as_ref().to_path_buf(),
    };

    let source = sources.push(source);

    let mut interner = Interner::new();
    let mut tokens = Tokens::tokenize(
        &mut interner,
        source,                   // source id
        &sources[source].content, // source content
    )?;

    let mut context = Solver::new();

    while !tokens.is(Token::Eof) {
        let lhs = parse::ty(&mut tokens)?;

        tokens.expect(Token::Lt)?;
        tokens.expect(Token::Colon)?;

        let rhs = parse::ty(&mut tokens)?;

        tokens.expect(Token::Newline)?;

        context.subty(&lhs, &rhs)?;
    }

    println!("\n--- constraints ---\n");

    for (ident, var) in &context.variables {
        let lbs: Vec<_> = var.lbs.iter().map(|ty| ty.to_string()).collect();
        let ubs: Vec<_> = var.ubs.iter().map(|ty| ty.to_string()).collect();

        if !lbs.is_empty() {
            print!("{} <: ", lbs.join(", "));
        }

        print!("'{}", ident);

        if !ubs.is_empty() {
            print!(" <: {}", ubs.join(", "));
        }

        println!();
    }

    Ok(())
}

pub type Name = &'static str;
pub type Field = &'static str;
pub type Var = &'static str;

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    /* algebraic data types */
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    Neg(Box<Ty>),

    /* type constructors */
    Name(Name),
    Record(HashMap<Field, Ty>),
    Func(Box<Ty>, Box<Ty>),
    Always,
    Never,

    /* type variables */
    Var(Var),
}

impl Ty {
    pub fn add(&mut self, other: Ty) {
        let inter = Ty::Inter(Box::new(self.clone()), Box::new(other));
        *self = inter.normalize();
    }

    fn normalize(&self) -> Self {
        match self {
            Ty::Union(lhs, rhs) => Self::normalize_union(lhs, rhs),
            Ty::Inter(lhs, rhs) => Self::normalize_inter(lhs, rhs),

            Ty::Neg(ty) => match ty.as_ref() {
                Ty::Neg(ty) => ty.normalize(),
                _ => Ty::Neg(Box::new(ty.normalize())),
            },

            Ty::Name(name) => Ty::Name(name),

            Ty::Record(fields) => {
                let fields: HashMap<_, _> = fields
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.normalize()))
                    .collect();

                match fields.is_empty() {
                    true => Ty::Never,
                    false => Ty::Record(fields),
                }
            }

            Ty::Func(lhs, rhs) => Ty::Func(Box::new(lhs.normalize()), Box::new(rhs.normalize())),

            Ty::Always => Ty::Always,
            Ty::Never => Ty::Never,

            Ty::Var(ident) => Ty::Var(ident),
        }
    }

    fn normalize_union(lhs: &Self, rhs: &Self) -> Self {
        let lhs = lhs.normalize();
        let rhs = rhs.normalize();

        if lhs == rhs {
            return lhs;
        }

        match (lhs, rhs) {
            (Ty::Neg(neg), ty) | (ty, Ty::Neg(neg)) if *neg == ty => Ty::Always,

            (Ty::Record(lfields), Ty::Record(rfields)) => {
                let mut fields = HashMap::new();

                for (lid, lty) in lfields {
                    if let Some(rty) = rfields.get(lid) {
                        let union =
                            Ty::Union(Box::new(lty.clone()), Box::new(rty.clone())).normalize();

                        fields.insert(lid, union);
                    }
                }

                match fields.is_empty() {
                    true => Ty::Never,
                    false => Ty::Record(fields),
                }
            }

            (Ty::Func(i1, o1), Ty::Func(i2, o2)) => {
                let input = Ty::Union(i1, i2);
                let output = Ty::Union(o1, o2);

                Ty::Func(Box::new(input), Box::new(output))
            }

            (lhs, rhs) => Ty::Union(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn normalize_inter(lhs: &Self, rhs: &Self) -> Self {
        let lhs = lhs.normalize();
        let rhs = rhs.normalize();

        if lhs == rhs {
            return lhs;
        }

        match (lhs, rhs) {
            (Ty::Neg(neg), ty) | (ty, Ty::Neg(neg)) if *neg == ty => Ty::Never,

            (Ty::Record(lfields), Ty::Record(rfields)) => {
                let mut fields = HashMap::new();

                for (lid, lty) in lfields {
                    if let Some(rty) = rfields.get(lid) {
                        let inter =
                            Ty::Inter(Box::new(lty.clone()), Box::new(rty.clone())).normalize();

                        fields.insert(lid, inter);
                    }
                }

                match fields.is_empty() {
                    true => Ty::Never,
                    false => Ty::Record(fields),
                }
            }

            (Ty::Func(i1, o1), Ty::Func(i2, o2)) => {
                let input = Ty::Inter(i1, i2);
                let output = Ty::Inter(o1, o2);

                Ty::Func(Box::new(input), Box::new(output))
            }

            (lhs, rhs) => Ty::Inter(Box::new(lhs), Box::new(rhs)),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Union(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
            Ty::Inter(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            Ty::Neg(ty) => write!(f, "~{}", ty),
            Ty::Func(lhs, rhs) => write!(f, "({} -> {})", lhs, rhs),
            Ty::Always => write!(f, "1"),
            Ty::Never => write!(f, "!"),
            Ty::Var(name) => write!(f, "'{}", name),

            Ty::Name(name) => write!(f, "{}", name),

            Ty::Record(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|(ident, ty)| format!("{}: {}", ident, ty))
                    .collect();

                write!(f, "{{{}}}", fields.join(", "))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Solver {
    /// Type variable constraints.
    pub variables: HashMap<Var, Variable>,

    /// Name subtype relations.
    ///
    /// Note that this imples single inheritance.
    pub subtypes: HashMap<Name, Name>,
}

/// Constraints for a type variable.
#[derive(Clone, Debug)]
pub struct Variable {
    pub lbs: Vec<Ty>,
    pub ubs: Vec<Ty>,
}

impl Solver {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            subtypes: HashMap::new(),
        }
    }

    pub fn name(&mut self, lhs: &'static str, rhs: &'static str) {
        self.subtypes.insert(lhs, rhs);
    }

    pub fn var(&mut self, ident: &'static str) -> &mut Variable {
        self.variables.entry(ident).or_insert_with(|| Variable {
            lbs: Vec::new(),
            ubs: Vec::new(),
        })
    }

    pub fn subty(&mut self, lhs: &Ty, rhs: &Ty) -> Result<(), Diagnostic> {
        println!("denm: {} <: {}", lhs, rhs);

        let lhs = lhs.normalize();
        let rhs = rhs.normalize();

        println!("norm: {} <: {}", lhs, rhs);

        if lhs == rhs {
            return Ok(());
        }

        match (&lhs, &rhs) {
            (Ty::Never, _) => Ok(()),
            (_, Ty::Always) => Ok(()),

            // { .. } <: { .. }
            (Ty::Record(lfields), Ty::Record(rfields)) => {
                for (lid, lty) in lfields {
                    match rfields.get(lid) {
                        Some(rty) => self.subty(lty, rty)?,
                        None => {
                            let diagnostic = Diagnostic::error("missing::field")
                                .message(format!("missing field `{}`", lid));

                            return Err(diagnostic);
                        }
                    }
                }

                Ok(())
            }

            // a <: b & c => a <: b, a <: c
            (lhs, Ty::Inter(r1, r2)) => {
                self.subty(lhs, r1)?;
                self.subty(lhs, r2)?;

                Ok(())
            }

            // a | b <: c => a <: c, b <: c
            (Ty::Union(l1, l2), rhs) => {
                self.subty(l1, rhs)?;
                self.subty(l2, rhs)?;

                Ok(())
            }

            // ~lhs <: ~rhs => rhs <: lhs
            (Ty::Neg(lhs), Ty::Neg(rhs)) => self.subty(rhs, lhs),

            // 'a <: b
            (Ty::Var(lhs), rhs) => self.subty_var_ty(lhs, rhs),

            // a <: 'b
            (lhs, Ty::Var(rhs)) => self.subty_ty_var(lhs, rhs),

            // a & b <: c
            (Ty::Inter(l1, l2), rhs) => self.subty_inter_ty(l1, l2, rhs),

            // a <: b | c
            (lhs, Ty::Union(r1, r2)) => self.subty_ty_union(lhs, r1, r2),

            // a <: ~b
            (lhs, Ty::Neg(rhs)) => self.subty_ty_neg(lhs, rhs),

            // a -> b <: c -> d => c <: a, b <: d
            (Ty::Func(l1, l2), Ty::Func(r1, r2)) => {
                self.subty(r1, l1)?;
                self.subty(l2, r2)?;

                Ok(())
            }

            (lhs, rhs) => {
                let diagnostic = Diagnostic::error("invalid::subtype")
                    .message(format!("constraint `{} <: {}` is invalid", lhs, rhs));

                Err(diagnostic)
            }
        }
    }

    fn subty_var_ty(&mut self, lhs: Name, rhs: &Ty) -> Result<(), Diagnostic> {
        let var = self.var(lhs);
        for ub in var.ubs.clone() {
            self.subty(rhs, &ub)?;
        }

        let var = self.var(lhs);
        var.ubs.push(rhs.clone());

        for lb in var.lbs.clone() {
            self.subty(&lb, rhs)?;
        }

        Ok(())
    }

    fn subty_ty_var(&mut self, lhs: &Ty, rhs: Name) -> Result<(), Diagnostic> {
        let var = self.var(rhs);
        for lb in var.lbs.clone() {
            self.subty(lhs, &lb)?;
        }

        let var = self.var(rhs);
        var.lbs.push(lhs.clone());

        for rhs in var.ubs.clone() {
            self.subty(lhs, &rhs)?;
        }

        Ok(())
    }

    fn subty_inter_ty(&mut self, l1: &Ty, l2: &Ty, rhs: &Ty) -> Result<(), Diagnostic> {
        if l1 == rhs || l2 == rhs {
            return Ok(());
        }

        match (l1, l2, rhs) {
            (rhs @ Ty::Var(_), neg, lhs) | (neg, rhs @ Ty::Var(_), lhs) | (rhs, neg, lhs) => {
                let lhs = Ty::Inter(
                    Box::new(Ty::Neg(Box::new(neg.clone()))),
                    Box::new(lhs.clone()),
                );

                self.subty(&lhs, rhs)
            }
        }
    }

    fn subty_ty_union(&mut self, lhs: &Ty, r1: &Ty, r2: &Ty) -> Result<(), Diagnostic> {
        if r1 == lhs || r2 == lhs {
            return Ok(());
        }

        match (lhs, r1, r2) {
            // a <: b | c => a & ~b <: c
            (rhs, lhs @ Ty::Var(_), neg) | (rhs, neg, lhs @ Ty::Var(_)) | (rhs, lhs, neg) => {
                let rhs = Ty::Inter(
                    Box::new(rhs.clone()),
                    Box::new(Ty::Neg(Box::new(neg.clone()))),
                );

                self.subty(&rhs, lhs)
            }
        }
    }

    fn subty_ty_neg(&mut self, lhs: &Ty, rhs: &Ty) -> Result<(), Diagnostic> {
        match (lhs, rhs) {
            (Ty::Name(lid), Ty::Name(rid)) => match lid == rid {
                false => Ok(()),
                true => {
                    let diagnostic = Diagnostic::error("invalid::negation")
                        .message(format!("constraint `{} <: ~{}` is invalid", lhs, rhs));

                    Err(diagnostic)
                }
            },

            (Ty::Record(lfields), Ty::Record(rfields)) => {
                for (lid, lty) in lfields {
                    if let Some(rty) = rfields.get(lid) {
                        let rty = Ty::Neg(Box::new(rty.clone()));

                        self.subty(lty, &rty)?;
                    }
                }

                Ok(())
            }

            (lhs, rhs) => {
                let lhs = Ty::Neg(Box::new(lhs.clone()));
                self.subty(&lhs, rhs)
            }
        }
    }
}
