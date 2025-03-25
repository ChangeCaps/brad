use crate::ast;
use crate::diagnostic::{SourceId, Span};
use crate::parse::{Interner, Token};
use clap::Args;
use rand::distr::weighted::WeightedIndex;
use rand::distr::Distribution;
use rand::prelude::IndexedRandom;
use rand::Rng;
use std::collections::BTreeMap;
use std::mem::discriminant;
use std::sync::LazyLock;

#[allow(non_upper_case_globals)]
const span: Span = Span::new(SourceId(0), 0, 0);

/// Only allocate the collection once.
/// (name, weight, generator)
type ExprGeneratorCollection = Vec<(
    &'static str,
    usize,
    Box<dyn Fn(&mut Generator, &mut GeneratorCtx, &ast::Ty) -> Option<ast::Expr> + Send + Sync>,
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
    #[clap(long, default_value = "10")]
    pub max_functions: usize,
    #[clap(long, default_value = "3")]
    pub max_function_args: usize,
    #[clap(long, default_value = "0")]
    pub min_exprs: usize,
    #[clap(long, default_value = "50")]
    pub max_exprs: usize,
    #[clap(long, default_value = "2")]
    pub max_match_arms: usize,
    #[clap(long, default_value = "3")]
    pub max_union_size: usize,
    #[clap(long, default_value = "3")]
    pub max_record_size: usize,
    #[clap(long, default_value = "3")]
    pub max_tuple_size: usize,
    #[clap(long, default_value = "3")]
    pub max_tys_rounds: usize,
    #[clap(long, default_value = "1")]
    pub max_depth: usize,
    #[clap(long, default_value = "30")]
    pub max_complexity: usize,
    #[clap(long, default_value = "100")]
    pub max_bruteforce_attempts: usize,
}

impl Default for GeneratorOptions {
    fn default() -> Self {
        Self {
            enable_match: true,
            enable_loop: true,
            enable_ref_ty: true,
            enable_list_ty: false,
            enable_unary: true,
            enable_floats: true,
            max_functions: 1,
            max_function_args: 3,
            min_exprs: 0,
            max_exprs: 50,
            max_match_arms: 2,
            max_union_size: 3,
            max_record_size: 3,
            max_tuple_size: 3,
            max_tys_rounds: 3,
            max_depth: 1,
            max_complexity: 30,
            max_bruteforce_attempts: 100,
        }
    }
}

pub struct Generator {
    opts: GeneratorOptions,
    rng: rand::rngs::ThreadRng,
    interner: Interner,

    /// Types (Basic types, ADT's, aliases to ADT's, new types)
    tys: Types,
    bodies: Vec<(&'static str, ast::Func)>,
}

struct GeneratorCtx {
    name: &'static str,
    depth: usize,
    complexity: usize,
    in_loop: bool,
    locals: BTreeMap<&'static str, Vec<(usize, ast::Ty)>>,
    computed_locals: BTreeMap<&'static str, ast::Ty>,
}

struct Types {
    tys: Vec<ast::Ty>,
    adts: Vec<ast::Ty>,
    alias_tys: Vec<(&'static str, ast::Ty)>,
    named_tys: Vec<(&'static str, ast::Ty)>,
}

impl Generator {
    pub fn new(opts: GeneratorOptions) -> Self {
        Self {
            opts,
            rng: rand::rng(),
            interner: Interner::new(),
            tys: Types::new(),
            bodies: Vec::new(),
        }
    }

    /// Generate a random module with a main function with somewhat correct types.
    /// - Generate a set of random types
    /// - Generate a set of random function declarations
    /// - Populate the function bodies with random expressions
    pub fn generate(&mut self) -> ast::Module {
        let mut decls = Vec::new();

        // Generate a set of base types.
        self.tys.reset();

        self.tys.tys.push(ast::Ty::Int(span));
        self.tys.tys.push(ast::Ty::Str(span));
        self.tys.tys.push(ast::Ty::None(span));
        self.tys.tys.push(ast::Ty::True(span));
        self.tys.tys.push(ast::Ty::False(span));

        // Optional base types (tags)
        if self.opts.enable_floats {
            self.tys.tys.push(ast::Ty::Float(span));
        }

        // Generate a set of custom types.
        for _ in 0..self.rng.random_range(0..=self.opts.max_tys_rounds) {
            // Generate a random union type.
            let mut tys: Vec<_> = (2..=self.opts.max_union_size).map(|_| self.ty()).collect();
            tys.sort();
            tys.dedup();

            // Can be 1
            if tys.len() >= 2 {
                self.tys.adts.push(ast::Ty::Union { tys, span });
            }

            // Generate a random tuple
            let tys: Vec<_> = (2..=self.opts.max_tuple_size).map(|_| self.ty()).collect();

            assert!(tys.len() >= 2);

            self.tys.adts.push(ast::Ty::Tuple { tys, span });

            // Generate a random list
            if self.opts.enable_list_ty {
                let ty = self.ty();
                self.tys.adts.push(ast::Ty::List {
                    ty: Box::new(ty),
                    span,
                });
            }

            // Generate a random record
            let fields = (0..self.opts.max_record_size)
                .map(|_| ast::Field {
                    name: self.name(None),
                    ty: self.ty(),
                    span,
                })
                .collect::<Vec<_>>();

            self.tys.adts.push(ast::Ty::Record { fields, span });

            // Generate a random alias
            let ty = self.ty();
            let name = self.name(None);
            self.tys.alias_tys.push((name, ty.clone()));
            decls.push(ast::Decl::Alias(ast::Alias {
                attrs: Default::default(),
                name: ast::Name {
                    segments: vec![name],
                    span,
                },
                generics: None,
                ty: ty.clone(),
                span,
            }));

            // Generate a random named type
            let ty = self.ty();
            let name = self.name(None);
            self.tys.named_tys.push((name, ty.clone()));
            decls.push(ast::Decl::Type(ast::Type {
                attrs: Default::default(),
                is_extern: false,
                name: ast::Name {
                    segments: vec![name],
                    span,
                },
                generics: None,
                ty: Some(ty),
                span,
            }));
        }

        // Generate a set of function bodies
        for i in 0..self.rng.random_range(1..=self.opts.max_functions) {
            let name = if i != 0 {
                self.name(None)
            } else {
                self.interner.intern("main")
            };

            // Do not create duplicate function names.
            if self.bodies.iter().any(|(n, _)| *n == name) {
                continue;
            }

            let ret_ty = self.ty();

            let args_count = self.rng.random_range(0..=self.opts.max_function_args);
            let args = (0..args_count)
                .map(|_| self.argument(None, &ret_ty))
                .collect();

            let body = ast::Func {
                attrs: Default::default(),
                is_extern: false,
                name: ast::Name {
                    segments: vec![name],
                    span,
                },
                generics: None,
                args,
                output: Some(ret_ty),
                body: None,
                span,
            };

            self.bodies.push((name, body));
        }

        // Populate the function bodies with random expressions
        for (name, body) in self.bodies.clone().iter() {
            let mut ctx = GeneratorCtx::new(name);
            let ty = body.output.as_ref().unwrap();
            let expr = self.expr_block(&mut ctx, ty).unwrap();

            // Find original body and set expr
            if let Some((_, body)) = self.bodies.iter_mut().find(|(n, _)| n == name) {
                body.body = Some(expr);
            }
        }

        self.bodies
            .iter()
            .for_each(|(_, body)| decls.push(ast::Decl::Func(body.clone())));

        ast::Module {
            attrs: Default::default(),
            decls,
        }
    }

    /// Generate a random (interned) name.
    fn name(&mut self, ctx: Option<&GeneratorCtx>) -> &'static str {
        let len = self.rng.random_range(1..=10);
        let mut name = String::with_capacity(len);

        let charset_first = b"abcdefghijklmnopqrstuvwxyz";
        let charset_full = b"abcdefghijklmnopqrstuvwxyz1234567890_--''";

        for _ in 0..self.opts.max_bruteforce_attempts {
            name.push(*charset_first.choose(&mut self.rng).unwrap() as char);

            for _ in 0..len - 1 {
                name.push(*charset_full.choose(&mut self.rng).unwrap() as char);
            }

            // Is a keyword no bueno.
            if Token::from_keyword(name.as_str()).is_some() {
                continue;
            }

            // Is a local, no bueno.
            if ctx.is_some() && ctx.unwrap().locals().contains_key(name.as_str()) {
                continue;
            }

            // If it's a function name, no bueno.
            if self.bodies.iter().any(|(n, _)| *n == name) {
                continue;
            }

            // If it's a type name, no bueno.
            if self.tys.named_tys.iter().any(|(n, _)| *n == name) {
                continue;
            }

            if self.tys.alias_tys.iter().any(|(n, _)| *n == name) {
                continue;
            }

            return self.interner.intern(name.as_str());
        }

        unreachable!()
    }

    fn ty(&mut self) -> ast::Ty {
        self.tys.random_any(&mut self.rng)
    }

    fn argument(&mut self, ctx: Option<&GeneratorCtx>, ty: &ast::Ty) -> ast::Argument {
        ast::Argument {
            binding: self.binding(ctx, ty),
            ty: Some(ty.clone()),
            span,
        }
    }

    fn binding(&mut self, ctx: Option<&GeneratorCtx>, ty: &ast::Ty) -> ast::Binding {
        // Special case for tuple bindings. In the future records as well :)
        if let ast::Ty::Tuple { tys, .. } = ty {
            return ast::Binding::Tuple {
                bindings: tys.iter().map(|ty| self.binding(ctx, ty)).collect(),
                span,
            };
        }

        ast::Binding::Bind {
            mutable: self.rng.random_bool(0.3),
            name: self.name(ctx),
            span,
        }
    }

    fn pattern(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Pattern {
        let binding = self.binding(Some(ctx), ty);
        ctx.extend_locals(&binding, ty);

        ast::Pattern::Ty {
            ty: ty.clone(),
            binding: Some(binding),
            span,
        }
    }

    // Potentially multi-line block.
    fn expr_multi(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.complexity >= self.opts.max_complexity {
            return None;
        }

        let mut choices = EXPR_MULTI_COLLECTION.iter().collect::<Vec<_>>();

        if !self.opts.enable_loop {
            choices.retain(|(name, _, _)| *name != "loop");
        }

        if !ctx.in_loop {
            choices.retain(|(name, _, _)| *name != "break");
        }

        if !self.opts.enable_match {
            choices.retain(|(name, _, _)| *name != "match");
        }

        if ctx.depth >= self.opts.max_depth {
            choices.retain(|(name, _, _)| *name != "block");
        }

        // Let bindings can only appear when the required return type is none.
        if !matches!(ty, ast::Ty::None(_)) {
            choices.retain(|(name, _, _)| *name != "let" && *name != "loop");
        }

        while !choices.is_empty() {
            let index = WeightedIndex::new(choices.iter().map(|(_, w, _)| *w))
                .unwrap()
                .sample(&mut self.rng);

            if let Some(expr) = choices[index].2(self, ctx, ty) {
                return Some(expr);
            }

            let _ = choices.remove(index);
        }

        None
    }

    // Simple, non-block expression.
    fn expr(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.complexity >= self.opts.max_complexity {
            return None;
        }

        let mut choices = EXPR_SINGLE_COLLECTION.iter().collect::<Vec<_>>();

        if !self.opts.enable_ref_ty {
            choices.retain(|(name, _, _)| *name != "ref");
        }

        if !self.opts.enable_unary {
            choices.retain(|(name, _, _)| *name != "unary");
        }

        while !choices.is_empty() {
            let index = WeightedIndex::new(choices.iter().map(|(_, w, _)| *w))
                .unwrap()
                .sample(&mut self.rng);

            if let Some(expr) = choices[index].2(self, ctx, ty) {
                return Some(expr);
            }

            let _ = choices.remove(index);
        }

        None
    }

    fn expr_call_function(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        // Find fitting function.
        let values = self
            .bodies
            .iter()
            .filter(|(name, v)| {
                *name != "main"
                    && *name != ctx.name
                    && self.tys.is_subtype_of(&v.output.clone().unwrap(), ty)
            })
            .map(|(_, v)| v)
            .collect::<Vec<_>>();

        let func = values.choose(&mut self.rng)?;

        let func_args = func.args.clone();

        let mut call_expr = ast::Expr::Path(ast::Path {
            segments: func
                .name
                .segments
                .iter()
                .cloned()
                .map(|name| ast::PathSegment { name, span })
                .collect(),
            spec: None,
            span,
        });

        for arg in func_args {
            let ty = match &arg.ty {
                Some(ty) => ty,
                None => &self.ty(),
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
        &mut self,
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
            op: *choices.choose(&mut self.rng).unwrap(),
            span,
        }))
    }

    fn expr_unary_op(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let choices = match ty {
            ast::Ty::Int(_) => vec![ast::UnaryOp::Neg, ast::UnaryOp::BitNot],
            _ => {
                return None;
            }
        };

        if let Some(expr) = self.expr(ctx, ty) {
            Some(ast::Expr::Unary(ast::UnaryExpr {
                op: *choices.choose(&mut self.rng).unwrap(),
                target: Box::new(expr),
                span,
            }))
        } else {
            None
        }
    }

    /// Either a local or a literal. Must be infallible to be used with literal, as it falls back to
    /// a literal if no locals are available.
    fn expr_value(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Expr {
        let valid_locals = ctx
            .computed_locals
            .iter()
            .filter(|(_, local_ty)| self.tys.is_subtype_of(local_ty, ty))
            .map(|(name, _)| *name)
            .collect::<Vec<_>>();

        if !valid_locals.is_empty() {
            return ast::Expr::Path(ast::Path {
                segments: vec![ast::PathSegment {
                    name: valid_locals.choose(&mut self.rng).unwrap(),
                    span,
                }],
                spec: None,
                span,
            });
        }

        self.expr_literal(ctx, ty)
    }

    /// Literal is by definition infallible (and must stay so).
    fn expr_literal(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> ast::Expr {
        let l = match ty {
            ast::Ty::Int(_) => ast::Expr::Literal(ast::Literal::Int {
                value: self.rng.random_range(0..=100),
                span,
            }),
            ast::Ty::Float(_) => ast::Expr::Literal(ast::Literal::Float {
                value: self.rng.random_range(0.0..=100.0),
                span,
            }),

            ast::Ty::Str(_) => ast::Expr::Literal(ast::Literal::String {
                value: self.name(Some(ctx)),
                span,
            }),
            ast::Ty::None(_) => ast::Expr::Literal(ast::Literal::None { span }),
            ast::Ty::True(_) => ast::Expr::Literal(ast::Literal::True { span }),
            ast::Ty::False(_) => ast::Expr::Literal(ast::Literal::False { span }),
            ast::Ty::List { ty, .. } => {
                let count = self
                    .rng
                    .random_range(self.opts.min_exprs..=self.opts.max_exprs);
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
                let ty = tys.choose(&mut self.rng).unwrap();
                self.expr_value(ctx, ty)
            }

            ast::Ty::Path(p) => {
                let (named, ty) = self.tys.resolve_path(p).unwrap();

                let value = self.expr_value(ctx, &ty);

                if named {
                    ast::Expr::Call(ast::CallExpr {
                        target: Box::new(ast::Expr::Path(p.clone())),
                        input: Box::new(value),
                        span,
                    })
                } else {
                    value
                }
            }

            _ => {
                panic!("Unsupported type: {:?}", ty);
            }
        };

        l
    }

    fn expr_loop(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        ctx.with_loop(|ctx| {
            let expr = self.expr_block(ctx, ty)?;

            Some(ast::Expr::Loop(ast::LoopExpr {
                body: Box::new(expr),
                span,
            }))
        })
    }

    fn expr_break(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        if ctx.in_loop && matches!(ty, ast::Ty::None(_)) {
            Some(ast::Expr::Break(ast::BreakExpr { value: None, span }))
        } else {
            None
        }
    }

    fn expr_block(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        ctx.with_scope(move |ctx| {
            let count = self
                .rng
                .random_range(self.opts.min_exprs..=self.opts.max_exprs);
            let mut exprs = Vec::with_capacity(count + 1);

            // Generate count - 1 exprs
            for _ in 0..count {
                let ty = &self.ty();
                if let Some(expr) = self.expr_multi(ctx, ty) {
                    exprs.push(expr);
                }
            }

            // Generate last expression (same as the block)
            exprs.push('out: {
                for _ in 0..self.opts.max_bruteforce_attempts {
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
    fn expr_let(&mut self, ctx: &mut GeneratorCtx) -> Option<ast::Expr> {
        let ty = self.ty();
        let binding = self.binding(Some(ctx), &ty);
        let value = self.expr(ctx, &ty)?;

        ctx.extend_locals(&binding, &ty);

        Some(ast::Expr::Let(ast::LetExpr {
            binding,
            ty: Some(ty),
            value: Box::new(value),
            span,
        }))
    }

    fn expr_match(&mut self, ctx: &mut GeneratorCtx, ty: &ast::Ty) -> Option<ast::Expr> {
        let match_ty = self.tys.random_tagged(&mut self.rng);
        let target = self.expr(ctx, &match_ty)?;
        let mut arms = Vec::with_capacity(self.opts.max_match_arms);

        for _ in 0..self.opts.max_match_arms {
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

            // Exact same type is already covered.
            if arms.iter().any(|arm: &ast::MatchArm| {
                let ast::Pattern::Ty { ty, .. } = &arm.pattern;
                ty == &match_ty || self.tys.is_subtype_of(&match_ty, ty)
            }) {
                continue;
            }

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
}

impl Types {
    fn new() -> Self {
        Self {
            tys: Vec::new(),
            adts: Vec::new(),
            alias_tys: Vec::new(),
            named_tys: Vec::new(),
        }
    }

    fn reset(&mut self) {
        self.tys.clear();
        self.adts.clear();
        self.alias_tys.clear();
        self.named_tys.clear();
    }

    /// Return any random type
    fn random_any(&self, rng: &mut rand::rngs::ThreadRng) -> ast::Ty {
        let weights = [
            self.tys.len(),
            self.adts.len(),
            self.alias_tys.len(),
            self.named_tys.len(),
        ];

        let index = WeightedIndex::new(weights).unwrap().sample(rng);

        match index {
            0 => self.tys.choose(rng).unwrap().clone(),
            1 => self.adts.choose(rng).unwrap().clone(),
            2 => ast::Ty::Path(ast::Path {
                segments: vec![ast::PathSegment {
                    name: self.alias_tys.choose(rng).unwrap().0,
                    span,
                }],
                spec: None,
                span,
            }),
            3 => ast::Ty::Path(ast::Path {
                segments: vec![ast::PathSegment {
                    name: self.named_tys.choose(rng).unwrap().0,
                    span,
                }],
                spec: None,
                span,
            }),
            _ => unreachable!(),
        }
    }

    /// Return a random *tagged* type.
    fn random_tagged(&self, rng: &mut rand::rngs::ThreadRng) -> ast::Ty {
        let weights = [self.tys.len(), self.named_tys.len()];

        let index = WeightedIndex::new(weights).unwrap().sample(rng);

        match index {
            0 => self.tys.choose(rng).unwrap().clone(),
            1 => ast::Ty::Path(ast::Path {
                segments: vec![ast::PathSegment {
                    name: self.named_tys.choose(rng).unwrap().0,
                    span,
                }],
                spec: None,
                span,
            }),
            _ => unreachable!(),
        }
    }

    fn resolve_path(&self, path: &ast::Path) -> Option<(bool, ast::Ty)> {
        match path.segments.as_slice() {
            [segment] => {
                if let Some(ty) = self
                    .named_tys
                    .iter()
                    .find(|(name, _)| *name == segment.name)
                {
                    return Some((true, ty.1.clone()));
                }

                if let Some(ty) = self
                    .alias_tys
                    .iter()
                    .find(|(name, _)| *name == segment.name)
                {
                    return Some((false, ty.1.clone()));
                }

                None
            }
            _ => None,
        }
    }

    fn is_subtype_of(&self, a: &ast::Ty, b: &ast::Ty) -> bool {
        if discriminant(a) != discriminant(b) {
            return false;
        }

        match (a, b) {
            (ast::Ty::Path(p), _) => {
                if let Some((_, ty)) = self.resolve_path(p) {
                    self.is_subtype_of(&ty, b)
                } else {
                    false
                }
            }
            (_, ast::Ty::Path(p)) => {
                if let Some((_, ty)) = self.resolve_path(p) {
                    self.is_subtype_of(a, &ty)
                } else {
                    false
                }
            }
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

                a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| self.is_subtype_of(a, b))
            }
            (ast::Ty::Tuple { tys: a, .. }, ast::Ty::Tuple { tys: b, .. }) => a
                .iter()
                .zip(b.iter())
                .all(|(a, b)| self.is_subtype_of(a, b)),
            (ast::Ty::List { ty: a, .. }, ast::Ty::List { ty: b, .. }) => self.is_subtype_of(a, b),
            (ast::Ty::Record { fields: a, .. }, ast::Ty::Record { fields: b, .. }) => {
                let mut a = a.clone();
                let mut b = b.clone();

                a.sort();
                b.sort();

                a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| self.is_subtype_of(&a.ty, &b.ty))
            }
            (ast::Ty::Ref { ty: a, .. }, ast::Ty::Ref { ty: b, .. }) => self.is_subtype_of(a, b),
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
            ) => self.is_subtype_of(a, c) && self.is_subtype_of(b, d),
            (any, ast::Ty::Union { tys, .. }) => tys.iter().any(|ty| self.is_subtype_of(any, ty)),
            _ => false,
        }
    }
}

impl GeneratorCtx {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            depth: 0,
            complexity: 0,
            in_loop: false,
            locals: BTreeMap::new(),
            computed_locals: BTreeMap::new(),
        }
    }

    pub fn locals(&self) -> &BTreeMap<&'static str, ast::Ty> {
        &self.computed_locals
    }

    pub fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.depth += 1;
        let ret: T = f(self);
        self.depth -= 1;
        // Recompute the locals and remove history from depth that is no longer valid.
        self.computed_locals.clear();
        for (name, history) in self.locals.iter_mut() {
            history.retain(|(d, _)| *d <= self.depth);
            if let Some((_, ty)) = history.last() {
                self.computed_locals.insert(*name, ty.clone());
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
                let local_history = self.locals.entry(*name).or_default();
                local_history.push((self.depth, ty.clone()));
                self.computed_locals.insert(*name, ty.clone());
            }
            ast::Binding::Tuple { bindings, .. } => {
                for binding in bindings {
                    self.extend_locals(binding, ty);
                }
            }
        }
    }
}
