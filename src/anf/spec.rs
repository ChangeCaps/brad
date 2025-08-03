use std::collections::HashMap;

use crate::{anf, hir2};

pub fn specialize(program: &hir2::Program) -> hir2::SpecializedProgram {
    let mut spec = Specializer {
        hir: program.clone(),
        spec: hir2::SpecializedProgram::default(),
        cache: HashMap::default(),
    };

    for (bid, body) in program.bodies.iter() {
        let map = Default::default();

        let ty = spec.spec_type(&map, &body.ty(), true);

        spec.spec_body(&map, bid, ty);
    }

    spec.spec
}

struct Specializer {
    hir: hir2::Program,
    spec: hir2::SpecializedProgram,
    cache: HashMap<(hir2::BodyId, anf::Type), hir2::SpecializedBodyId>,
}

impl Specializer {
    fn spec_body(
        &mut self,
        map: &HashMap<solve::Var, anf::Type>,
        bid: hir2::BodyId,
        ty: anf::Type,
    ) -> hir2::SpecializedBodyId {
        if let Some(id) = self.cache.get(&(bid, ty.clone())) {
            return *id;
        }

        let spec = hir2::SpecializedBody {
            attrs: Default::default(),
            is_extern: Default::default(),
            name: Default::default(),
            locals: Default::default(),
            input: Default::default(),
            output: Default::default(),
            expr: Default::default(),
            span: Default::default(),
        };

        let specid = self.spec.bodies.insert(spec);

        self.cache.insert((bid, ty), specid);

        let body = self.hir.bodies[bid].clone();

        let expr = body.expr.map(|expr| self.spec_expr(map, &expr));

        let mut locals = hir2::Locals::new();

        for local in body.locals {
            locals.insert(hir2::Local {
                is_mutable: local.is_mutable,
                name: local.name,
                ty: self.spec_type(map, &local.ty, true),
                span: local.span,
            });
        }

        let mut input = Vec::new();

        for arg in body.input {
            input.push(hir2::Argument {
                binding: arg.binding,
                ty: self.spec_type(map, &arg.ty, false),
            });
        }

        let spec = hir2::SpecializedBody {
            attrs: body.attrs.clone(),
            is_extern: body.is_extern,
            name: body.name.clone(),
            locals,
            input,
            output: self.spec_type(map, &body.output, true),
            expr,
            span: body.span,
        };

        self.spec.bodies[specid] = spec;

        specid
    }

    fn spec_expr(
        &mut self,
        map: &HashMap<solve::Var, anf::Type>,
        expr: &hir2::Expr,
    ) -> hir2::Expr<anf::Type> {
        let kind = match &expr.kind {
            hir2::ExprKind::Int(v) => hir2::ExprKind::Int(*v),
            hir2::ExprKind::Float(v) => hir2::ExprKind::Float(*v),
            hir2::ExprKind::String(v) => hir2::ExprKind::String(v),
            hir2::ExprKind::ZeroSize(tag) => hir2::ExprKind::ZeroSize(*tag),
            hir2::ExprKind::Local(lid) => hir2::ExprKind::Local(*lid),

            hir2::ExprKind::Tag(tag, expr) => {
                let expr = self.spec_expr(map, expr);
                hir2::ExprKind::Tag(*tag, Box::new(expr))
            }

            hir2::ExprKind::Func(bid) => {
                let ty = self.spec_type(map, &expr.ty, true);

                let mut new_map = Default::default();
                self.unify_type(&mut new_map, &self.hir.bodies[*bid].ty(), ty.clone());

                let bid = self.spec_body(&new_map, *bid, ty);
                hir2::ExprKind::Func(hir2::BodyId(bid.0))
            }

            hir2::ExprKind::Array(_exprs) => todo!(),

            hir2::ExprKind::Tuple(exprs) => {
                let exprs = exprs.iter().map(|expr| self.spec_expr(map, expr)).collect();
                hir2::ExprKind::Tuple(exprs)
            }

            hir2::ExprKind::Record(inits) => {
                let inits = inits
                    .iter()
                    .map(|init| hir2::Init {
                        name: init.name,
                        value: self.spec_expr(map, &init.value),
                        span: init.span,
                    })
                    .collect();

                hir2::ExprKind::Record(inits)
            }

            hir2::ExprKind::Index(expr, expr1) => todo!(),

            hir2::ExprKind::Field(expr, name) => {
                let expr = self.spec_expr(map, expr);
                hir2::ExprKind::Field(Box::new(expr), name)
            }

            hir2::ExprKind::Unary(op, expr) => {
                let expr = self.spec_expr(map, expr);
                hir2::ExprKind::Unary(*op, Box::new(expr))
            }

            hir2::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.spec_expr(map, lhs);
                let rhs = self.spec_expr(map, rhs);

                hir2::ExprKind::Binary(*op, Box::new(lhs), Box::new(rhs))
            }

            hir2::ExprKind::Call(function, input) => {
                let function = self.spec_expr(map, function);
                let input = self.spec_expr(map, input);

                hir2::ExprKind::Call(Box::new(function), Box::new(input))
            }

            hir2::ExprKind::Lambda {
                captures,
                args,
                locals,
                body,
            } => todo!(),

            hir2::ExprKind::Assign(lhs, rhs) => {
                let lhs = self.spec_expr(map, lhs);
                let rhs = self.spec_expr(map, rhs);

                hir2::ExprKind::Assign(Box::new(lhs), Box::new(rhs))
            }

            hir2::ExprKind::Ref(_expr) => todo!(),

            hir2::ExprKind::Match(expr, body) => {
                let expr = self.spec_expr(map, expr);

                let mut arms = Vec::new();

                for arm in &body.arms {
                    arms.push(hir2::Arm {
                        pattern: arm.pattern.clone(),
                        body: self.spec_expr(map, &arm.body),
                        span: arm.span,
                    });
                }

                let default = body
                    .default
                    .as_deref()
                    .map(|(binding, expr)| Box::new((binding.clone(), self.spec_expr(map, expr))));

                let body = hir2::MatchBody { arms, default };

                hir2::ExprKind::Match(Box::new(expr), body)
            }

            hir2::ExprKind::Loop(expr) => {
                let expr = self.spec_expr(map, expr);
                hir2::ExprKind::Loop(Box::new(expr))
            }

            hir2::ExprKind::Break(expr) => {
                let expr = expr
                    .as_deref()
                    .map(|expr| self.spec_expr(map, expr))
                    .map(Box::new);

                hir2::ExprKind::Break(expr)
            }

            hir2::ExprKind::Let(binding, expr) => {
                let expr = self.spec_expr(map, expr);

                hir2::ExprKind::Let(binding.clone(), Box::new(expr))
            }

            hir2::ExprKind::Block(exprs) => {
                let exprs = exprs.iter().map(|e| self.spec_expr(map, e)).collect();

                hir2::ExprKind::Block(exprs)
            }
        };

        let ty = self.spec_type(map, &expr.ty, true);
        let span = expr.span;

        hir2::Expr { kind, ty, span }
    }

    fn spec_type(
        &mut self,
        map: &HashMap<solve::Var, anf::Type>,
        ty: &solve::Type,
        pol: bool,
    ) -> anf::Type {
        self.spec_type_impl(map, ty, pol, &mut Default::default())
    }

    fn spec_type_impl(
        &mut self,
        map: &HashMap<solve::Var, anf::Type>,
        ty: &solve::Type,
        pol: bool,
        cache: &mut HashMap<Result<(solve::Var, bool), solve::App>, ()>,
    ) -> anf::Type {
        let mut anf = anf::Type { terms: Vec::new() };

        for term in ty.conjuncts() {
            let mut term_ty = anf::Type {
                terms: vec![anf::Term {
                    tags: term.positive.tags.clone(),
                    base: match term.positive.base {
                        solve::Base::None => anf::Base::Any,

                        solve::Base::Record(ref record) => {
                            let mut fields = Vec::new();

                            for (name, item) in &record.fields {
                                let item = self.spec_type_impl(map, item, pol, cache);
                                let item = self.spec.types.insert(item);

                                fields.push((*name, item));
                            }

                            anf::Base::Record(fields)
                        }

                        solve::Base::Tuple(ref tuple) => {
                            let mut items = Vec::new();

                            for item in &tuple.fields {
                                let item = self.spec_type_impl(map, item, pol, cache);
                                let item = self.spec.types.insert(item);

                                items.push(item);
                            }

                            anf::Base::Tuple(items)
                        }

                        solve::Base::Array(ref array) => {
                            let item = self.spec_type_impl(map, &array.element, pol, cache);
                            let item = self.spec.types.insert(item);

                            anf::Base::Array(item)
                        }

                        solve::Base::Function(ref function) => {
                            let input = self.spec_type_impl(map, &function.input, !pol, cache);
                            let output = self.spec_type_impl(map, &function.output, pol, cache);

                            let input = self.spec.types.insert(input);
                            let output = self.spec.types.insert(output);

                            anf::Base::Function(input, output)
                        }
                    },
                }],
            };

            for app in &term.positive.apps {
                if cache.get(&Err(app.clone())).is_some() {
                    continue;
                }

                cache.insert(Err(app.clone()), ());

                let app = self.hir.tcx.expand(app.clone());
                let app = self.spec_type_impl(map, &app, pol, cache);

                self.spec.types.intersect(&mut term_ty, &app);
            }

            for &var in &term.positive.vars {
                if let Some(ty) = map.get(&var) {
                    self.spec.types.intersect(&mut term_ty, ty);
                    continue;
                }

                if cache.get(&Ok((var, pol))).is_some() {
                    continue;
                }

                cache.insert(Ok((var, pol)), ());

                let bounds = self.hir.tcx.bounds(var);

                let bound = match pol {
                    true => bounds.upper,
                    false => bounds.lower,
                };

                let bound = self.spec_type_impl(map, &bound, pol, cache);
                self.spec.types.intersect(&mut term_ty, &bound);
            }

            anf.terms.extend(term_ty.terms);
        }

        anf
    }

    fn unify_type(
        &mut self,
        map: &mut HashMap<solve::Var, anf::Type>,
        ty: &solve::Type,
        ex: anf::Type,
    ) {
        for ex_term in &ex.terms {
            let ex_ty = self.anf_to_solve(&anf::Type {
                terms: vec![ex_term.clone()],
            });

            for conjunct in ty.conjuncts() {
                let ty = solve::Type::from(conjunct.clone());

                if self.hir.tcx.is_subtype(ty, ex_ty.clone()) {
                    self.unify_term(map, &conjunct.positive, ex_term.clone());
                }
            }
        }
    }

    fn unify_term(
        &mut self,
        map: &mut HashMap<solve::Var, anf::Type>,
        tm: &solve::Term,
        ex: anf::Term,
    ) {
        for var in &tm.vars {
            let ex = anf::Type {
                terms: vec![ex.clone()],
            };

            map.insert(*var, ex);
        }

        for app in &tm.apps {
            let ex = anf::Type {
                terms: vec![ex.clone()],
            };

            let app = self.hir.tcx.expand(app.clone());
            self.unify_type(map, &app, ex);
        }

        match (&tm.base, ex.base) {
            (solve::Base::None, anf::Base::Any) => {}

            (solve::Base::Function(function), anf::Base::Function(input, output)) => {
                self.unify_type(map, &function.input, self.spec.types[input].clone());
                self.unify_type(map, &function.output, self.spec.types[output].clone());
            }

            (_, _) => unreachable!(),
        }
    }

    fn anf_to_solve(&mut self, ty: &anf::Type) -> solve::Type {
        let mut conjuncts = Vec::new();

        for term in &ty.terms {
            let term = solve::Term {
                vars: Vec::new(),
                apps: Vec::new(),
                tags: term.tags.clone(),
                base: match term.base {
                    anf::Base::Any => solve::Base::None,

                    anf::Base::Record(ref items) => {
                        let mut fields = Vec::new();

                        for (name, item) in items {
                            let item = self.tid_to_solve(*item);
                            fields.push((*name, item));
                        }

                        solve::Base::Record(solve::Record { fields })
                    }

                    anf::Base::Tuple(ref items) => {
                        let mut fields = Vec::new();

                        for item in items {
                            let item = self.tid_to_solve(*item);
                            fields.push(item);
                        }

                        solve::Base::Tuple(solve::Tuple { fields })
                    }

                    anf::Base::Array(item) => {
                        let item = self.tid_to_solve(item);

                        solve::Base::Array(solve::Array { element: item })
                    }

                    anf::Base::Function(input, output) => {
                        let input = self.tid_to_solve(input);
                        let output = self.tid_to_solve(output);

                        solve::Base::Function(solve::Function { input, output })
                    }
                },
            };

            conjuncts.push(solve::Conjunct {
                positive: term,
                negative: solve::Term::extreme(),
            });
        }

        From::from(conjuncts)
    }

    fn tid_to_solve(&mut self, tid: anf::Tid) -> solve::Type {
        let ty = self.anf_to_solve(&self.spec.types[tid].clone());
        let bounds = solve::Bounds {
            lower: ty.clone(),
            upper: ty.clone(),
        };

        let var = solve::Var::fresh();
        self.hir.tcx.set_bounds(var, bounds);

        solve::Type::var(var)
    }
}
