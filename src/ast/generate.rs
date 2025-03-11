use crate::ast;
use crate::diagnostic::{SourceId, Span};
use crate::parse::Interner;
use rand::prelude::IndexedRandom;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn random_name(rng: &mut rand::rngs::ThreadRng) -> String {
    let len = rng.random_range(1..=10);
    let mut name = String::with_capacity(len);
    for _ in 0..len {
        name.push(rng.random_range(b'a'..=b'z') as char);
    }
    name
}

#[allow(non_upper_case_globals)]
const span: Span = Span::new(SourceId(0), 0, 0);

pub struct Generator {
    bodies: HashMap<&'static str, Rc<RefCell<BodyGenerator>>>,
    types: Vec<ast::Ty>,
    interner: Rc<RefCell<Interner>>,
}

/// Randomize a single ast module no cross-module references.
///
/// Start by generating a list of types
/// Then generate functions using these types.
/// Then generate function bodies using the available other functions, and arguments.
impl Generator {
    pub fn new() -> Rc<RefCell<Self>> {
        let interner = Rc::new(RefCell::new(Interner::new()));
        Rc::new(RefCell::new(Self {
            bodies: HashMap::new(),
            types: vec![],
            interner,
        }))
    }

    pub fn generate(self_rc: Rc<RefCell<Self>>) -> ast::Module {
        self_rc.borrow_mut().populate_types();
        Self::populate_bodies(self_rc.clone(), 2);

        // Ensure self_rc is dropped before calling generate on the bodies
        let bodies = self_rc.borrow().bodies.clone();

        ast::Module {
            decls: bodies
                .values()
                .map(|b| ast::Decl::Func(b.borrow().generate()))
                .collect::<Vec<_>>(),
        }
    }

    fn populate_types(&mut self) {
        // Testing with only ints
        self.types.clear();
        self.types.push(ast::Ty::Int(span));
        self.types.push(ast::Ty::Float(span));
    }

    fn populate_bodies(self_rc: Rc<RefCell<Self>>, n: usize) {
        let mut rng = rand::rng();
        let interner = self_rc.borrow().interner.clone();

        for i in 0..(n + 1) {
            let (name, args) = if i != n {
                (
                    random_name(&mut rng),
                    vec![self_rc.borrow().ty(&mut rng), self_rc.borrow().ty(&mut rng)],
                )
            } else {
                ("main".to_string(), vec![])
            };

            let ret_ty = self_rc.borrow().ty(&mut rng);

            let body = Rc::new(RefCell::new(BodyGenerator::new(
                self_rc.clone(),
                interner.clone(),
                name.clone(),
                ret_ty,
                args,
            )));

            self_rc
                .borrow_mut()
                .bodies
                .insert(interner.borrow_mut().intern(name.as_str()), body);
        }
    }

    fn ty(&self, rng: &mut rand::rngs::ThreadRng) -> ast::Ty {
        // Select a random type
        self.types.choose(rng).unwrap().clone()
    }

    /// Select random function (excluding main)
    fn func(&self, rng: &mut rand::rngs::ThreadRng) -> Rc<RefCell<BodyGenerator>> {
        let values = self
            .bodies
            .iter()
            .filter(|(name, _)| **name != "main")
            .map(|(_, v)| v);
        let values = values.collect::<Vec<_>>();
        let x = values.choose(rng).unwrap();
        Rc::clone(x)
    }
}

pub struct BodyGenerator {
    generator: Rc<RefCell<Generator>>,
    interner: Rc<RefCell<Interner>>,
    name: &'static str,
    args: Vec<ast::Argument>,
    output: ast::Ty,
    locals: Vec<&'static str>,
}

pub struct BodyGeneratorCtx {
    rng: rand::rngs::ThreadRng,
    depth: usize,
}

impl BodyGeneratorCtx {
    pub fn new(rng: rand::rngs::ThreadRng) -> Self {
        Self { rng, depth: 0 }
    }
}

impl BodyGenerator {
    pub fn new(
        generator: Rc<RefCell<Generator>>,
        interner: Rc<RefCell<Interner>>,
        name: String,
        output: ast::Ty,
        args: Vec<ast::Ty>,
    ) -> Self {
        let mut rng = rand::rng();

        let args = args
            .iter()
            .map(|ty| ast::Argument {
                binding: ast::Binding::Bind {
                    mutable: false,
                    name: interner.borrow_mut().intern(random_name(&mut rng).as_str()),
                    span,
                },
                ty: Some(ty.clone()),
                span,
            })
            .collect();

        let name = interner.borrow_mut().intern(name.as_str());

        Self {
            generator,
            interner,
            name,
            args,
            output,
            locals: vec![],
        }
    }

    fn as_path(&self) -> ast::Path {
        ast::Path {
            segments: vec![ast::PathSegment {
                name: self.name,
                span,
            }],
            spec: None,
            span,
        }
    }

    fn generate(&self) -> ast::Func {
        let mut ctx = BodyGeneratorCtx::new(rand::rng());

        let exprs = (0..ctx.rng.random_range(1..=10))
            .map(|_| self.expr(&mut ctx))
            .collect::<Vec<_>>();

        ast::Func {
            attrs: Default::default(),
            is_extern: false,
            name: self.name,
            generics: None,
            args: self.args.clone(),
            output: Some(self.output.clone()),
            body: Some(ast::Expr::Block(ast::BlockExpr { exprs, span })),
            span,
        }
    }

    fn expr(&self, ctx: &mut BodyGeneratorCtx) -> ast::Expr {
        let ty = self.generator.borrow().ty(&mut ctx.rng);

        if ctx.depth > 3 {
            return self.expr_literal(ctx, &ty);
        }

        ctx.depth += 1;

        let e = match [0, 1, 2, 3].choose(&mut ctx.rng).unwrap() {
            0 => self.expr_call_function(ctx, &ty),
            1 => self.expr_binary_op(ctx, &ty, &ty),
            2 => self.expr_unary_op(ctx, &ty),
            3 => self.expr_literal(ctx, &ty),
            _ => unreachable!(),
        };

        ctx.depth -= 1;

        e
    }

    fn expr_call_function(&self, ctx: &mut BodyGeneratorCtx, _ty: &ast::Ty) -> ast::Expr {
        let generator = self.generator.borrow();
        let func = generator.func(&mut ctx.rng);
        let borrow = func.borrow();
        let func_path = borrow.as_path();

        ast::Expr::Call(ast::CallExpr {
            target: Box::new(ast::Expr::Call(ast::CallExpr {
                target: Box::new(ast::Expr::Path(func_path)),
                input: Box::new(self.expr(ctx)),
                span,
            })),
            input: Box::new(self.expr(ctx)),
            span,
        })
    }

    fn expr_binary_op(
        &self,
        ctx: &mut BodyGeneratorCtx,
        _lhs: &ast::Ty,
        _rhs: &ast::Ty,
    ) -> ast::Expr {
        ast::Expr::Binary(ast::BinaryExpr {
            lhs: Box::new(self.expr(ctx)),
            rhs: Box::new(self.expr(ctx)),
            op: ast::BinaryOp::Add,
            span,
        })
    }

    fn expr_unary_op(&self, ctx: &mut BodyGeneratorCtx, _ty: &ast::Ty) -> ast::Expr {
        ast::Expr::Unary(ast::UnaryExpr {
            op: ast::UnaryOp::Neg,
            expr: Box::new(self.expr(ctx)),
            span,
        })
    }

    fn expr_literal(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> ast::Expr {
        match ty {
            ast::Ty::Int(_) => ast::Expr::Literal(ast::Literal::Int {
                value: ctx.rng.random_range(0..=100),
                span,
            }),
            ast::Ty::Float(_) => ast::Expr::Literal(ast::Literal::Float {
                value: ctx.rng.random_range(0.0..=100.0),
                span,
            }),
            _ => todo!(),
        }
    }
}
