use crate::ast;
use crate::diagnostic::{SourceId, Span};
use crate::parse::{Interner, Token};
use clap::Args;
use rand::distr::weighted::WeightedIndex;
use rand::distr::Distribution;
use rand::prelude::IndexedRandom;
use rand::Rng;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::mem::discriminant;
use std::rc::Rc;
use std::sync::LazyLock;

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
            let mut a = a.clone();
            let mut b = b.clone();

            a.sort();
            b.sort();

            a.iter().zip(b.iter()).all(|(a, b)| is_subtype_of(a, b))
        }
        (ast::Ty::Tuple { tys: a, .. }, ast::Ty::Tuple { tys: b, .. }) => {
            a.iter().zip(b.iter()).all(|(a, b)| is_subtype_of(a, b))
        }
        (ast::Ty::List { ty: a, .. }, ast::Ty::List { ty: b, .. }) => is_subtype_of(a, b),
        (ast::Ty::Record { fields: a, .. }, ast::Ty::Record { fields: b, .. }) => {
            let mut a = a.clone();
            let mut b = b.clone();

            a.sort();
            b.sort();

            a.iter()
                .zip(b.iter())
                .all(|(a, b)| is_subtype_of(&a.ty, &b.ty))
        }
        (ast::Ty::Ref { ty: a, .. }, ast::Ty::Ref { ty: b, .. }) => is_subtype_of(a, b),
        (
            ast::Ty::Func {
                input: a,
                output: b,
                ..
            },
            ast::Ty::Func {
                input: c,
                output: d,
                ..
            },
        ) => is_subtype_of(a, c) && is_subtype_of(b, d),
        (any, ast::Ty::Union { tys, .. }) => tys.iter().any(|ty| is_subtype_of(any, ty)),
        _ => false,
    }
}

#[allow(non_upper_case_globals)]
const span: Span = Span::new(SourceId(0), 0, 0);

static AST_BOOL: LazyLock<ast::Ty, fn() -> ast::Ty> = LazyLock::new(|| ast::Ty::Union {
    tys: vec![ast::Ty::True(span), ast::Ty::False(span)],
    span,
});

/// Only allocate the collection once.
/// (name, weight, generator)
type ExprGeneratorCollection = Vec<(
    &'static str,
    usize,
    Box<dyn Fn(&BodyGenerator, &mut GeneratorCtx, &ast::Ty) -> Option<ast::Expr> + Send + Sync>,
)>;

static EXPR_MULTI_COLLECTION: LazyLock<ExprGeneratorCollection, fn() -> ExprGeneratorCollection> =
    LazyLock::new(|| {
        vec![
            ("expr", 5, Box::new(|s, ctx, ty| s.expr(ctx, ty))),
            (
                "block",
                1,
                Box::new(|s, ctx, ty| ctx.with_complexity(5, |ctx| s.expr_block(ctx, ty))),
            ),
            (
                "loop",
                1,
                Box::new(|s, ctx, ty| ctx.with_complexity(5, |ctx| s.expr_loop(ctx, ty))),
            ),
            (
                "match",
                2,
                Box::new(|s, ctx, ty| ctx.with_complexity(5, |ctx| s.expr_match(ctx, ty))),
            ),
            (
                "let",
                3,
                Box::new(|s, ctx, _| ctx.with_complexity(2, |ctx| s.expr_let(ctx))),
            ),
            (
                "break",
                2,
                Box::new(|s, ctx, ty| ctx.with_complexity(10, |ctx| s.expr_break(ctx, ty))),
            ),
        ]
    });

static EXPR_SINGLE_COLLECTION: LazyLock<ExprGeneratorCollection, fn() -> ExprGeneratorCollection> =
    LazyLock::new(|| {
        vec![
            (
                "binary",
                3,
                Box::new(|s, ctx, ty| ctx.with_complexity(4, |ctx| s.expr_binary_op(ctx, ty, ty))),
            ),
            (
                "unary",
                1,
                Box::new(|s, ctx, ty| ctx.with_complexity(10, |ctx| s.expr_unary_op(ctx, ty))),
            ),
            (
                "value",
                1,
                Box::new(|s, ctx, ty| ctx.with_complexity(2, |ctx| Some(s.expr_value(ctx, ty)))),
            ),
            (
                "call",
                5,
                Box::new(|s, ctx, ty| ctx.with_complexity(10, |ctx| s.expr_call_function(ctx, ty))),
            ),
        ]
    });

#[derive(Args, Debug, Clone)]
pub struct GeneratorOptions {
    #[clap(long, default_value = "100")]
    pub functions: usize,
    #[clap(long, default_value = "false")]
    pub enable_match: bool,
    #[clap(long, default_value = "false")]
    pub enable_loop: bool,
    #[clap(long, default_value = "false")]
    pub enable_ref_ty: bool,
    #[clap(long, default_value = "false")]
    pub enable_list_ty: bool,
    #[clap(long, default_value = "false")]
    pub enable_unary: bool,
    #[clap(long, default_value = "false")]
    pub enable_floats: bool,
    #[clap(long, default_value = "3")]
    pub max_function_args: usize,
    #[clap(long, default_value = "0")]
    pub min_exprs: usize,
    #[clap(long, default_value = "50")]
    pub max_exprs: usize,
    #[clap(long, default_value = "2")]
    pub max_match_arms: usize,
    #[clap(long, default_value = "3")]
    pub max_union_tys: usize,
    #[clap(long, default_value = "3")]
    pub max_record_fields: usize,
    #[clap(long, default_value = "3")]
    pub max_tuple_tys: usize,
    #[clap(long, default_value = "3")]
    pub max_custom_tys: usize,
    #[clap(long, default_value = "1")]
    pub max_depth: usize,
    #[clap(long, default_value = "30")]
    pub max_allowed_complexity: usize,
    #[clap(long, default_value = "100")]
    pub max_bruteforce_attempts: usize,
}

impl Default for GeneratorOptions {
    fn default() -> Self {
        Self {
            functions: 100,
            enable_match: true,
            enable_loop: true,
            enable_ref_ty: true,
            enable_list_ty: false,
            enable_unary: true,
            enable_floats: true,
            max_function_args: 3,
            min_exprs: 0,
            max_exprs: 50,
            max_match_arms: 2,
            max_union_tys: 3,
            max_record_fields: 3,
            max_tuple_tys: 3,
            max_custom_tys: 3,
            max_depth: 1,
            max_allowed_complexity: 30,
            max_bruteforce_attempts: 100,
        }
    }
}

pub struct GeneratorCtx {
    rng: rand::rngs::ThreadRng,
    depth: usize,
    complexity: usize,
    in_loop: bool,
    locals_history: BTreeMap<&'static str, Vec<(usize, ast::Ty)>>,
    locals: BTreeMap<&'static str, ast::Ty>,
    options: GeneratorOptions,
}

impl GeneratorCtx {
    pub fn new(rng: rand::rngs::ThreadRng, options: GeneratorOptions) -> Self {
        Self {
            rng,
            depth: 0,
            complexity: 0,
            in_loop: false,
            locals_history: BTreeMap::new(),
            locals: BTreeMap::new(),
            options,
        }
    }

    pub fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.depth += 1;
        let ret: T = f(self);
        self.depth -= 1;
        // Recompute the locals and remove history from depth that is no longer valid.
        self.locals.clear();
        for (name, history) in self.locals_history.iter_mut() {
            history.retain(|(d, _)| *d <= self.depth);
            if let Some((_, ty)) = history.last() {
                self.locals.insert(*name, ty.clone());
            }
        }
        ret
    }

    pub fn with_loop<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_in_loop = self.in_loop;
        self.in_loop = true;
        let ret: T = f(self);
        self.in_loop = old_in_loop;
        ret
    }

    pub fn with_complexity<T>(&mut self, complexity: usize, f: impl FnOnce(&mut Self) -> T) -> T {
        self.complexity += complexity;
        let ret: T = f(self);
        self.complexity -= complexity;
        ret
    }

    pub fn extend_locals(&mut self, binding: &ast::Binding, ty: &ast::Ty) {
        match binding {
            ast::Binding::Wild { .. } => {}
            ast::Binding::Bind { name, .. } => {
                let local_history = self.locals_history.entry(*name).or_default();
                local_history.push((self.depth, ty.clone()));
                self.locals.insert(*name, ty.clone());
            }
            ast::Binding::Tuple { bindings, .. } => {
                for binding in bindings {
                    self.extend_locals(binding, ty);
                }
            }
        }
    }

    pub fn random_name(&mut self) -> String {
        let len = self.rng.random_range(1..=10);
        let mut name = String::with_capacity(len);

        for _ in 0..self.options.max_bruteforce_attempts {
            name.clear();

            for _ in 0..len {
                name.push(self.rng.random_range(b'a'..=b'z') as char);
            }

            // Is a keyword no bueno.
            if Token::from_keyword(name.as_str()).is_some() {
                continue;
            }

            // Is a local, no bueno.
            if self.locals.contains_key(name.as_str()) {
                continue;
            }

            return name;
        }

        unreachable!()
    }
}

impl Default for GeneratorCtx {
    fn default() -> Self {
        Self::new(rand::rng(), GeneratorOptions::default())
    }
}

pub struct Generator {
    bodies: Vec<(&'static str, Rc<RefCell<BodyGenerator>>)>,
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
            bodies: Vec::new(),
            types: Vec::new(),
            interner,
        }))
    }

    pub fn generate(self_rc: Rc<RefCell<Self>>, options: GeneratorOptions) -> ast::Module {
        let ctx = &mut GeneratorCtx::new(rand::rng(), options.clone());

        self_rc.borrow_mut().populate_types(ctx);
        Self::populate_bodies(self_rc.clone(), ctx);

        // Ensure self_rc is dropped before calling generate on the bodies
        let bodies = self_rc.borrow().bodies.clone();

        ast::Module {
            decls: bodies
                .iter()
                .map(|(_, b)| ast::Decl::Func(b.borrow().generate(options.clone())))
                .collect::<Vec<_>>(),
        }
    }

    fn populate_types(&mut self, ctx: &mut GeneratorCtx) {
        let mut interner = self.interner.borrow_mut();

        self.types.clear();
        self.types.push(ast::Ty::Int(span));
        self.types.push(ast::Ty::Str(span));
        self.types.push(ast::Ty::None(span));
        self.types.push(AST_BOOL.clone());

        if ctx.options.enable_floats {
            self.types.push(ast::Ty::Float(span));
        }

        // Generate a couple of random unions, tuples, lists and records
        for _ in 0..ctx.options.max_custom_tys {
            let mut tys = (2..ctx.options.max_union_tys)
                .map(|_| self.ty(ctx))
                .collect::<Vec<_>>();

            // Deduplicate the types
            tys.sort();
            tys.dedup();

            self.types.push(ast::Ty::Union { tys, span });

            let tys = (2..ctx.options.max_tuple_tys)
                .map(|_| self.ty(ctx))
                .collect::<Vec<_>>();

            self.types.push(ast::Ty::Tuple { tys, span });

            if ctx.options.enable_list_ty {
                let ty = self.ty(ctx);
                self.types.push(ast::Ty::List {
                    ty: Box::new(ty),
                    span,
                });
            }

            let fields = (0..ctx.options.max_record_fields)
                .map(|_| ast::Field {
                    name: interner.intern(ctx.random_name().as_str()),
                    ty: self.ty(ctx),
                    span,
                })
                .collect::<Vec<_>>();

            self.types.push(ast::Ty::Record { fields, span });
        }
    }

    fn populate_bodies(self_rc: Rc<RefCell<Self>>, ctx: &mut GeneratorCtx) {
        let interner = self_rc.borrow().interner.clone();

        for i in 0..(ctx.options.functions + 1) {
            let (name, args) = if i != ctx.options.functions {
                let mut args = vec![];
                // Controls function argument count
                let argc = ctx.rng.random_range(1..=ctx.options.max_function_args);

                for _ in 0..argc {
                    args.push(self_rc.borrow().ty(ctx));
                }

                (ctx.random_name(), args)
            } else {
                ("main".to_string(), vec![])
            };

            if self_rc.borrow().bodies.iter().any(|(n, _)| *n == name) {
                continue;
            }

            let ret_ty = self_rc.borrow().ty(ctx);

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
                .push((interner.borrow_mut().intern(name.as_str()), body));
        }
    }

    fn ty(&self, ctx: &mut GeneratorCtx) -> ast::Ty {
        // Select a random type
        self.types.choose(&mut ctx.rng).unwrap().clone()
    }

    fn argument(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Argument {
        ast::Argument {
            binding: self.binding(ctx, ty),
            ty: Some(ty.clone()),
            span,
        }
    }

    fn binding(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Binding {
        // Special case for tuple bindings. In the future records as well :)
        if let ast::Ty::Tuple { tys, .. } = ty {
            return ast::Binding::Tuple {
                bindings: tys.iter().map(|ty| self.binding(ctx, ty)).collect(),
                span,
            };
        }

        ast::Binding::Bind {
            mutable: ctx.rng.random_bool(0.3),
            name: self
                .interner
                .borrow_mut()
                .intern(ctx.random_name().as_str()),
            span,
        }
    }
}

pub struct BodyGenerator {
    generator: Rc<RefCell<Generator>>,
    interner: Rc<RefCell<Interner>>,
    name: &'static str,
    args: Vec<ast::Argument>,
    output: ast::Ty,
}

impl BodyGenerator {
    pub fn new(
        generator: Rc<RefCell<Generator>>,
        interner: Rc<RefCell<Interner>>,
        name: String,
        output: ast::Ty,
        args: Vec<ast::Ty>,
    ) -> Self {
        let args = args
            .iter()
            .map(|ty| {
                generator
                    .borrow()
                    .argument(&mut GeneratorCtx::default(), ty)
            })
            .collect();

        let name = interner.borrow_mut().intern(name.as_str());

        Self {
            generator,
            interner,
            name,
            args,
            output,
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

    fn generate(&self, options: GeneratorOptions) -> ast::Func {
        let mut ctx = GeneratorCtx::new(rand::rng(), options);

        self.args
            .iter()
            .for_each(|arg| ctx.extend_locals(&arg.binding, &arg.ty.clone().unwrap()));

        ast::Func {
            attrs: Default::default(),
            is_extern: false,
            name: ast::Name {
                segments: vec![self.name],
                span,
            },
            generics: None,
            args: self.args.clone(),
            output: Some(self.output.clone()),
            body: Some(self.expr_block(&mut ctx, &self.output).unwrap()),
            span,
        }
    }

    // Potentially multi-line block.
    fn expr_multi(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.complexity >= ctx.options.max_allowed_complexity {
            return None;
        }

        let mut choices = EXPR_MULTI_COLLECTION.iter().collect::<Vec<_>>();

        if !ctx.options.enable_loop {
            choices.retain(|(name, _, _)| *name != "loop");
        }

        if !ctx.in_loop {
            choices.retain(|(name, _, _)| *name != "break");
        }

        if !ctx.options.enable_match {
            choices.retain(|(name, _, _)| *name != "match");
        }

        if ctx.depth >= ctx.options.max_depth {
            choices.retain(|(name, _, _)| *name != "block");
        }

        // Let bindings can only appear when the required return type is none.
        if !matches!(ty, ast::Ty::None(_)) {
            choices.retain(|(name, _, _)| *name != "let" && *name != "loop");
        }

        while !choices.is_empty() {
            let index = WeightedIndex::new(choices.iter().map(|(_, w, _)| *w))
                .unwrap()
                .sample(&mut ctx.rng);

            if let Some(expr) = choices[index].2(self, ctx, ty) {
                return Some(expr);
            }

            let _ = choices.remove(index);
        }

        None
    }

    // Simple, non-block expression.
    fn expr(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.complexity >= ctx.options.max_allowed_complexity {
            return None;
        }

        let mut choices = EXPR_SINGLE_COLLECTION.iter().collect::<Vec<_>>();

        if !ctx.options.enable_ref_ty {
            choices.retain(|(name, _, _)| *name != "ref");
        }

        if !ctx.options.enable_unary {
            choices.retain(|(name, _, _)| *name != "unary");
        }

        while !choices.is_empty() {
            let index = WeightedIndex::new(choices.iter().map(|(_, w, _)| *w))
                .unwrap()
                .sample(&mut ctx.rng);

            if let Some(expr) = choices[index].2(self, ctx, ty) {
                return Some(expr);
            }

            let _ = choices.remove(index);
        }

        None
    }

    fn expr_call_function(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let generator = self.generator.borrow();

        // Find fitting function.
        let values = generator
            .bodies
            .iter()
            .filter(|(name, v)| {
                *name != "main" && *name != self.name && is_subtype_of(&v.borrow().output, ty)
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
                None => &generator.ty(ctx),
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
        ctx: &mut GeneratorCtx,
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

    fn expr_unary_op(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let choices = match ty {
            ast::Ty::Int(_) => vec![ast::UnaryOp::Neg, ast::UnaryOp::BitNot],
            _ => {
                return None;
            }
        };

        if let Some(expr) = self.expr(ctx, ty) {
            Some(ast::Expr::Unary(ast::UnaryExpr {
                op: *choices.choose(&mut ctx.rng).unwrap(),
                target: Box::new(expr),
                span,
            }))
        } else {
            None
        }
    }

    /// Either a local or a literal. Must be infallible to be used with literal, as it falls back to
    /// a literal if no locals are available.
    fn expr_value(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Expr {
        let valid_locals = ctx
            .locals
            .iter()
            .filter(|(_, local_ty)| is_subtype_of(local_ty, ty))
            .map(|(name, _)| *name)
            .collect::<Vec<_>>();

        if !valid_locals.is_empty() {
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
    fn expr_literal(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Expr {
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
                    .intern(ctx.random_name().as_str()),
                span,
            }),
            ast::Ty::None(_) => ast::Expr::Literal(ast::Literal::None { span }),
            ast::Ty::True(_) => ast::Expr::Literal(ast::Literal::True { span }),
            ast::Ty::False(_) => ast::Expr::Literal(ast::Literal::False { span }),
            ast::Ty::List { ty, .. } => {
                let count = ctx
                    .rng
                    .random_range(ctx.options.min_exprs..=ctx.options.max_exprs);
                let mut items = Vec::with_capacity(count);
                for _ in 0..count {
                    items.push(self.expr_value(ctx, ty));
                }
                ast::Expr::List(ast::ListExpr { items, span })
            }
            ast::Ty::Tuple { tys, .. } => {
                let mut items = Vec::with_capacity(tys.len());
                for ty in tys {
                    items.push(self.expr_value(ctx, ty));
                }
                ast::Expr::Tuple(ast::TupleExpr { items, span })
            }

            ast::Ty::Record { fields, .. } => {
                let mut field_inits = Vec::with_capacity(fields.len());
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

    fn expr_loop(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        ctx.with_loop(|ctx| {
            let expr = self.expr_block(ctx, ty)?;

            Some(ast::Expr::Loop(ast::LoopExpr {
                body: Box::new(expr),
                span,
            }))
        })
    }

    fn expr_break(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.in_loop && matches!(ty, ast::Ty::None(_)) {
            Some(ast::Expr::Break(ast::BreakExpr { value: None, span }))
        } else {
            None
        }
    }

    fn expr_block(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        ctx.with_scope(move |ctx| {
            let count = ctx
                .rng
                .random_range(ctx.options.min_exprs..=ctx.options.max_exprs);
            let mut exprs = Vec::with_capacity(count + 1);

            // Generate count - 1 exprs
            for _ in 0..count {
                let ty = &self.generator.borrow().ty(ctx);
                if let Some(expr) = self.expr_multi(ctx, ty) {
                    exprs.push(expr);
                }
            }

            // Generate last expression (same as the block)
            exprs.push('out: {
                for _ in 0..ctx.options.max_bruteforce_attempts {
                    if let Some(expr) = self.expr_multi(ctx, ty) {
                        break 'out expr;
                    }
                }

                self.expr_value(ctx, ty)
            });

            Some(ast::Expr::Block(ast::BlockExpr { exprs, span }))
        })
    }

    /// A let binding returns none.
    fn expr_let(&self, ctx: &mut GeneratorCtx) -> Option<ast::Expr> {
        let generator = self.generator.borrow();
        let ty = generator.ty(ctx);
        let binding = generator.binding(ctx, &ty);
        let value = self.expr(ctx, &ty)?;

        ctx.extend_locals(&binding, &ty);

        Some(ast::Expr::Let(ast::LetExpr {
            binding,
            ty: Some(ty),
            value: Box::new(value),
            span,
        }))
    }

    fn expr_match(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let match_ty = self.generator.borrow().ty(ctx);
        let target = self.expr(ctx, &match_ty)?;
        let mut arms = Vec::with_capacity(ctx.options.max_match_arms);

        for _ in 0..ctx.options.max_match_arms {
            // Should return a type that is a subtype of the context type.
            let (pattern, arm_expr) =
                ctx.with_scope(|ctx| (self.pattern(ctx, &match_ty), self.expr(ctx, ty)));

            if arm_expr.is_none() {
                continue;
            }

            let arm = ast::MatchArm {
                pattern,
                body: arm_expr.unwrap(),
                span,
            };

            arms.push(arm);
        }

        if !arms.is_empty() {
            Some(ast::Expr::Match(ast::MatchExpr {
                target: Box::new(target),
                arms,
                span,
            }))
        } else {
            None
        }
    }

    fn pattern(&self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Pattern {
        let generator = self.generator.borrow();
        let binding = generator.binding(ctx, ty);
        ctx.extend_locals(&binding, ty);

        ast::Pattern::Ty {
            ty: ty.clone(),
            binding: Some(binding),
            span,
        }
    }
}
