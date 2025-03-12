use crate::ast;
use crate::diagnostic::{SourceId, Span};
use crate::parse::Interner;
use rand::prelude::IndexedRandom;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::discriminant;
use std::rc::Rc;

const MAX_DEPTH: usize = 20;
const MAX_DEPTH_FUNCTION: usize = 2;
const MAX_UNION_TYS: usize = 3;
const MAX_RECORD_FIELDS: usize = 3;
const MAX_TUPLE_TYS: usize = 3;
const MAX_CUSTOM_TYS: usize = 3;
const MAX_EXPRS: usize = 4;

const FUNCTIONS: usize = 3;
const MAX_FUNCTION_ARGS: usize = 3;

fn random_name(rng: &mut rand::rngs::ThreadRng) -> String {
    let len = rng.random_range(1..=10);
    let mut name = String::with_capacity(len);
    for _ in 0..len {
        name.push(rng.random_range(b'a'..=b'z') as char);
    }
    name
}

fn is_subtype_of(a: &ast::Ty, b: &ast::Ty) -> bool {
    if discriminant(a) != discriminant(b) {
        return false;
    }

    match (a, b) {
        (ast::Ty::Int(_), ast::Ty::Int(_))
        | (ast::Ty::Float(_), ast::Ty::Float(_))
        | (ast::Ty::Str(_), ast::Ty::Str(_))
        | (ast::Ty::None(_), ast::Ty::None(_))
        | (ast::Ty::True(_), ast::Ty::True(_))
        | (ast::Ty::False(_), ast::Ty::False(_)) => true,
        (ast::Ty::Union { tys: a, .. }, ast::Ty::Union { tys: b, .. }) => {
            a.iter().zip(b.iter()).all(|(a, b)| is_subtype_of(a, b))
        }
        (ast::Ty::Tuple { tys: a, .. }, ast::Ty::Tuple { tys: b, .. }) => {
            a.iter().zip(b.iter()).all(|(a, b)| is_subtype_of(a, b))
        }
        (ast::Ty::List { ty: a, .. }, ast::Ty::List { ty: b, .. }) => is_subtype_of(a, b),
        (ast::Ty::Record { fields: a, .. }, ast::Ty::Record { fields: b, .. }) => a
            .iter()
            .zip(b.iter())
            .all(|(a, b)| is_subtype_of(&a.ty, &b.ty)),
        _ => false,
    }
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
        // Controls function count
        Self::populate_bodies(self_rc.clone(), FUNCTIONS);

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
        let mut rng = rand::rng();
        let mut interner = self.interner.borrow_mut();

        // Testing with only ints
        self.types.clear();
        self.types.push(ast::Ty::Int(span));
        self.types.push(ast::Ty::Float(span));
        self.types.push(ast::Ty::Str(span));
        self.types.push(ast::Ty::None(span));
        self.types.push(ast::Ty::Union {
            tys: vec![ast::Ty::True(span), ast::Ty::False(span)],
            span,
        });

        // Generate a couple of random unions, tuples, lists and records
        for _ in 0..MAX_CUSTOM_TYS {
            let tys = (0..MAX_UNION_TYS)
                .map(|_| self.ty(&mut rng))
                .collect::<Vec<_>>();

            self.types.push(ast::Ty::Union { tys, span });

            let tys = (0..MAX_TUPLE_TYS)
                .map(|_| self.ty(&mut rng))
                .collect::<Vec<_>>();

            self.types.push(ast::Ty::Tuple { tys, span });

            let ty = self.ty(&mut rng);
            self.types.push(ast::Ty::List {
                ty: Box::new(ty),
                span,
            });

            let fields = (0..MAX_RECORD_FIELDS)
                .map(|_| ast::Field {
                    name: interner.intern(random_name(&mut rng).as_str()),
                    ty: self.ty(&mut rng),
                    span,
                })
                .collect::<Vec<_>>();

            self.types.push(ast::Ty::Record { fields, span });
        }
    }

    fn populate_bodies(self_rc: Rc<RefCell<Self>>, n: usize) {
        let mut rng = rand::rng();
        let interner = self_rc.borrow().interner.clone();

        for i in 0..(n + 1) {
            let (name, args) = if i != n {
                let mut args = vec![];
                // Controls function argument count
                let argc = rng.random_range(1..=MAX_FUNCTION_ARGS);

                for _ in 0..argc {
                    args.push(self_rc.borrow().ty(&mut rng));
                }

                (random_name(&mut rng), args)
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
}

pub struct BodyGenerator {
    generator: Rc<RefCell<Generator>>,
    interner: Rc<RefCell<Interner>>,
    name: &'static str,
    args: Vec<ast::Argument>,
    output: ast::Ty,
    locals: HashMap<&'static str, ast::Ty>,
}

pub struct BodyGeneratorCtx {
    rng: rand::rngs::ThreadRng,
    depth: usize,
    in_loop: bool,
}

impl BodyGeneratorCtx {
    pub fn new(rng: rand::rngs::ThreadRng) -> Self {
        Self {
            rng,
            depth: 0,
            in_loop: false,
        }
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
            locals: HashMap::new(),
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

        ast::Func {
            attrs: Default::default(),
            is_extern: false,
            name: self.name,
            generics: None,
            args: self.args.clone(),
            output: Some(self.output.clone()),
            body: Some(self.expr_block(&mut ctx, &self.output).unwrap()),
            span,
        }
    }

    fn expr(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        // Give up if we are too deep.
        if ctx.depth > MAX_DEPTH {
            return None;
        }

        let mut choices = vec![0, 1, 2];

        // Prevent deeply nested blocks and functions chaining.
        if ctx.depth < MAX_DEPTH_FUNCTION {
            choices.push(3);
            choices.push(4);
            choices.push(6);
        };

        if ctx.in_loop {
            choices.push(5);
        }

        ctx.depth += 1;

        let mut e: Option<ast::Expr> = None;

        while e.is_none() {
            if let Some(choice) = &mut choices.choose(&mut ctx.rng) {
                e = match choice {
                    0 => self.expr_binary_op(ctx, ty, ty),
                    1 => self.expr_unary_op(ctx, ty),
                    2 => Some(self.expr_value(ctx, ty)),
                    3 => self.expr_call_function(ctx, ty),
                    4 => self.expr_block(ctx, ty),
                    5 => self.expr_break(ctx, ty),
                    6 => self.expr_loop(ctx, ty),
                    _ => unreachable!(),
                };

                choices = choices.iter().filter(|c| *c != *choice).copied().collect();
            } else {
                return None;
            }
        }

        ctx.depth -= 1;

        e
    }

    fn expr_call_function(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let generator = self.generator.borrow();

        // Find fitting function.
        let values = generator
            .bodies
            .iter()
            .filter(|(name, v)| {
                **name != "main" && **name != self.name && is_subtype_of(&v.borrow().output, ty)
            })
            .map(|(_, v)| v)
            .collect::<Vec<_>>();

        let func = values.choose(&mut ctx.rng)?;

        let borrow = func.borrow();
        let func_args = borrow.args.clone();

        let mut call_expr = ast::Expr::Path(borrow.as_path());

        for arg in func_args {
            let ty = match &arg.ty {
                Some(ty) => ty,
                None => &generator.ty(&mut ctx.rng),
            };

            if let Some(input_expr) = self.expr(ctx, ty) {
                call_expr = ast::Expr::Call(ast::CallExpr {
                    target: Box::new(call_expr),
                    input: Box::new(input_expr),
                    span,
                });
            } else {
                return None;
            }
        }

        Some(call_expr)
    }

    fn expr_binary_op(
        &self,
        ctx: &mut BodyGeneratorCtx,
        lhs: &ast::Ty,
        rhs: &ast::Ty,
    ) -> Option<ast::Expr> {
        let choices = match (lhs, rhs) {
            (ast::Ty::Int(_), ast::Ty::Int(_)) => {
                vec![
                    ast::BinaryOp::Add,
                    ast::BinaryOp::Sub,
                    ast::BinaryOp::Mul,
                    ast::BinaryOp::Div,
                    ast::BinaryOp::Mod,
                    ast::BinaryOp::BitAnd,
                    ast::BinaryOp::BitOr,
                    ast::BinaryOp::BitXor,
                    ast::BinaryOp::Shl,
                    ast::BinaryOp::Shr,
                ]
            }
            (ast::Ty::Float(_), ast::Ty::Float(_)) => {
                vec![
                    ast::BinaryOp::Add,
                    ast::BinaryOp::Sub,
                    ast::BinaryOp::Mul,
                    ast::BinaryOp::Div,
                    ast::BinaryOp::Mod,
                ]
            }
            _ => {
                return None;
            }
        };

        let lhs = self.expr(ctx, lhs)?;

        // We can return just the lhs if we got that at least.
        let Some(rhs) = self.expr(ctx, rhs) else {
            return Some(lhs);
        };

        Some(ast::Expr::Binary(ast::BinaryExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: *choices.choose(&mut ctx.rng).unwrap(),
            span,
        }))
    }

    fn expr_unary_op(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let choices = match ty {
            ast::Ty::Int(_) => vec![ast::UnaryOp::Neg, ast::UnaryOp::BitNot],
            _ => {
                return None;
            }
        };

        if let Some(expr) = self.expr(ctx, ty) {
            Some(ast::Expr::Unary(ast::UnaryExpr {
                op: *choices.choose(&mut ctx.rng).unwrap(),
                expr: Box::new(expr),
                span,
            }))
        } else {
            None
        }
    }

    /// Either a local or a literal. Must be infallible to be used with literal, as it falls back to
    /// a literal if no locals are available.
    fn expr_value(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> ast::Expr {
        let valid_locals = self
            .locals
            .iter()
            .filter(|(_, local_ty)| is_subtype_of(local_ty, ty))
            .map(|(name, _)| *name)
            .collect::<Vec<_>>();

        if !valid_locals.is_empty() && ctx.rng.random_bool(0.5) {
            return ast::Expr::Path(ast::Path {
                segments: vec![ast::PathSegment {
                    name: valid_locals.choose(&mut ctx.rng).unwrap(),
                    span,
                }],
                spec: None,
                span,
            });
        }

        self.expr_literal(ctx, ty)
    }

    /// Literal is by definition infallible (and must stay so).
    fn expr_literal(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> ast::Expr {
        let l = match ty {
            ast::Ty::Int(_) => ast::Expr::Literal(ast::Literal::Int {
                value: ctx.rng.random_range(0..=100),
                span,
            }),
            ast::Ty::Float(_) => ast::Expr::Literal(ast::Literal::Float {
                value: ctx.rng.random_range(0.0..=100.0),
                span,
            }),

            ast::Ty::Str(_) => ast::Expr::Literal(ast::Literal::String {
                value: self
                    .interner
                    .borrow_mut()
                    .intern(random_name(&mut ctx.rng).as_str()),
                span,
            }),
            ast::Ty::None(_) => ast::Expr::Literal(ast::Literal::None { span }),
            ast::Ty::True(_) => ast::Expr::Literal(ast::Literal::True { span }),
            ast::Ty::False(_) => ast::Expr::Literal(ast::Literal::False { span }),
            ast::Ty::List { ty, .. } => {
                let n = ctx.rng.random_range(1..=MAX_EXPRS);
                let mut items = vec![];
                for _ in 0..n {
                    items.push(self.expr_value(ctx, ty));
                }
                ast::Expr::List(ast::ListExpr { items, span })
            }
            ast::Ty::Tuple { tys, .. } => {
                let mut items = vec![];
                for ty in tys {
                    items.push(self.expr_value(ctx, ty));
                }
                ast::Expr::Tuple(ast::TupleExpr { items, span })
            }

            ast::Ty::Record { fields, .. } => {
                let mut field_inits = vec![];
                for field in fields {
                    field_inits.push(ast::FieldInit {
                        name: field.name,
                        value: self.expr_value(ctx, &field.ty),
                        span,
                    });
                }
                ast::Expr::Record(ast::RecordExpr {
                    fields: field_inits,
                    span,
                })
            }

            ast::Ty::Union { tys, .. } => {
                let ty = tys.choose(&mut ctx.rng).unwrap();
                self.expr_value(ctx, ty)
            }

            _ => {
                panic!("Unsupported type: {:?}", ty);
            }
        };

        l
    }

    fn expr_loop(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let n = ctx.rng.random_range(1..=MAX_EXPRS);

        ctx.in_loop = true;

        let expr = self.expr_block(ctx, ty)?;

        ctx.in_loop = false;

        Some(ast::Expr::Loop(ast::LoopExpr {
            body: Box::new(expr),
            span,
        }))
    }

    fn expr_break(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.in_loop && matches!(ty, ast::Ty::None(_)) {
            Some(ast::Expr::Break(ast::BreakExpr { value: None, span }))
        } else {
            None
        }
    }

    fn expr_block(&self, ctx: &mut BodyGeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let n = ctx.rng.random_range(1..=MAX_EXPRS);

        let mut exprs = vec![];

        for i in 0..n {
            let ty = if i == n - 1 {
                ty
            } else {
                &self.generator.borrow().ty(&mut ctx.rng)
            };

            if let Some(expr) = self.expr(ctx, ty) {
                exprs.push(expr);
            }
        }

        Some(ast::Expr::Block(ast::BlockExpr { exprs, span }))
    }

    //
}
