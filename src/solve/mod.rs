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

    let mut context = Context::new();

    while !tokens.is(Token::Eof) {
        let lhs = parse::ty(&mut tokens)?;

        tokens.expect(Token::Lt)?;
        tokens.expect(Token::Colon)?;

        let rhs = parse::ty(&mut tokens)?;

        tokens.expect(Token::Newline)?;

        context.subty(&lhs, &rhs);
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

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    /* algebraic data types */
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    Neg(Box<Ty>),

    /* type constructors */
    Name(&'static str),
    Record(HashMap<&'static str, Ty>),
    Func(Box<Ty>, Box<Ty>),
    Always,
    Never,

    /* type variables */
    Var(&'static str),
}

impl Ty {
    pub fn add(&mut self, other: Ty) {
        let inter = Ty::Inter(Box::new(self.clone()), Box::new(other));
        *self = inter.normalize();
    }

    fn normalize(&self) -> Self {
        match self {
            Ty::Union(lhs, rhs) => {
                let lhs = lhs.normalize();
                let rhs = rhs.normalize();

                if lhs == rhs {
                    return lhs;
                }

                match (lhs, rhs) {
                    (Ty::Record(lfields), Ty::Record(rfields)) => {
                        let mut fields = HashMap::new();

                        for (lid, lty) in lfields {
                            if let Some(rty) = rfields.get(lid) {
                                let union = Ty::Union(Box::new(lty.clone()), Box::new(rty.clone()))
                                    .normalize();

                                fields.insert(lid, union);
                            }
                        }

                        Ty::Record(fields)
                    }

                    (Ty::Func(i1, o1), Ty::Func(i2, o2)) => {
                        let input = Ty::Union(i1, i2);
                        let output = Ty::Union(o1, o2);

                        Ty::Func(Box::new(input), Box::new(output))
                    }

                    (lhs, rhs) => Ty::Union(Box::new(lhs), Box::new(rhs)),
                }
            }

            Ty::Inter(lhs, rhs) => {
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
                                let inter = Ty::Inter(Box::new(lty.clone()), Box::new(rty.clone()))
                                    .normalize();

                                fields.insert(lid, inter);
                            }
                        }

                        Ty::Record(fields)
                    }

                    (Ty::Func(i1, o1), Ty::Func(i2, o2)) => {
                        let input = Ty::Inter(i1, i2);
                        let output = Ty::Inter(o1, o2);

                        Ty::Func(Box::new(input), Box::new(output))
                    }

                    (lhs, rhs) => Ty::Inter(Box::new(lhs), Box::new(rhs)),
                }
            }

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

                Ty::Record(fields)
            }

            Ty::Func(lhs, rhs) => Ty::Func(Box::new(lhs.normalize()), Box::new(rhs.normalize())),
            Ty::Always => Ty::Always,
            Ty::Never => Ty::Never,
            Ty::Var(ident) => Ty::Var(ident),
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
pub struct Context {
    pub variables: HashMap<&'static str, Variable>,
}

/// Constraints for a type variable.
#[derive(Clone, Debug)]
pub struct Variable {
    pub lbs: Vec<Ty>,
    pub ubs: Vec<Ty>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn var(&mut self, ident: &'static str) -> &mut Variable {
        self.variables.entry(ident).or_insert_with(|| Variable {
            lbs: Vec::new(),
            ubs: Vec::new(),
        })
    }

    pub fn subty(&mut self, lhs: &Ty, rhs: &Ty) {
        println!("denm: {} <: {}", lhs, rhs);

        let lhs = lhs.normalize();
        let rhs = rhs.normalize();

        println!("norm: {} <: {}", lhs, rhs);

        if lhs == rhs {
            return;
        }

        match (&lhs, &rhs) {
            (Ty::Never, _) => {}
            (_, Ty::Always) => {}

            (Ty::Record(lfields), Ty::Record(rfields)) => {
                for (lid, lty) in lfields {
                    match rfields.get(lid) {
                        Some(rty) => self.subty(lty, rty),
                        None => todo!("missing field: {}", lid),
                    }
                }
            }

            // a <: b & c => a <: b, a <: c
            (lhs, Ty::Inter(r1, r2)) => {
                self.subty(lhs, r1);
                self.subty(lhs, r2);
            }

            // a | b <: c => a <: c, b <: c
            (Ty::Union(l1, l2), rhs) => {
                self.subty(l1, rhs);
                self.subty(l2, rhs);
            }

            // ~lhs <: ~rhs => rhs <: lhs
            (Ty::Neg(lhs), Ty::Neg(rhs)) => self.subty(rhs, lhs),

            (Ty::Var(lhs), rhs) => {
                let var = self.var(lhs);
                for ub in var.ubs.clone() {
                    self.subty(rhs, &ub);
                }

                let var = self.var(lhs);
                var.ubs.push(rhs.clone());

                for lb in var.lbs.clone() {
                    self.subty(&lb, rhs);
                }
            }

            (lhs, Ty::Var(rhs)) => {
                let var = self.var(rhs);
                for lb in var.lbs.clone() {
                    self.subty(lhs, &lb);
                }

                let var = self.var(rhs);
                var.lbs.push(lhs.clone());

                for rhs in var.ubs.clone() {
                    self.subty(lhs, &rhs);
                }
            }

            (Ty::Inter(l1, l2), rhs) => {
                if l1.as_ref() == rhs || l2.as_ref() == rhs {
                    return;
                }

                match (l1.as_ref(), l2.as_ref(), rhs) {
                    (rhs @ Ty::Var(_), neg, lhs)
                    | (neg, rhs @ Ty::Var(_), lhs)
                    | (rhs, neg, lhs) => {
                        let lhs = Ty::Inter(
                            Box::new(Ty::Neg(Box::new(neg.clone()))),
                            Box::new(lhs.clone()),
                        );

                        self.subty(&lhs, rhs);
                    }
                }
            }

            (lhs, Ty::Union(r1, r2)) => {
                if r1.as_ref() == lhs || r2.as_ref() == lhs {
                    return;
                }

                match (lhs, r1.as_ref(), r2.as_ref()) {
                    // a <: b | c => a & ~b <: c
                    (rhs, lhs @ Ty::Var(_), neg)
                    | (rhs, neg, lhs @ Ty::Var(_))
                    | (rhs, lhs, neg) => {
                        let rhs = Ty::Inter(
                            Box::new(rhs.clone()),
                            Box::new(Ty::Neg(Box::new(neg.clone()))),
                        );

                        self.subty(&rhs, lhs);
                    }
                }
            }

            (lhs, Ty::Neg(rhs)) => match (lhs, rhs.as_ref()) {
                (Ty::Name(lid), Ty::Name(rid)) => {
                    if lid == rid {
                        todo!("{} <: ~{}", lhs, rhs);
                    }
                }

                (Ty::Record(lfields), Ty::Record(rfields)) => {
                    for (lid, lty) in lfields {
                        if let Some(rty) = rfields.get(lid) {
                            let rty = Ty::Neg(Box::new(rty.clone()));

                            self.subty(lty, &rty);
                        }
                    }
                }

                (lhs, rhs) => {
                    let lhs = Ty::Neg(Box::new(lhs.clone()));
                    self.subty(&lhs, rhs);
                }
            },

            (Ty::Func(l1, l2), Ty::Func(r1, r2)) => {
                self.subty(r1, l1);
                self.subty(l2, r2);
            }

            (lhs, rhs) => todo!("{} <: {}", lhs, rhs),
        }
    }
}
