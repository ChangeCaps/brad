use std::collections::HashMap;

use crate::{hir2 as hir, ir};

pub fn build(program: &hir::Program) -> ir::Program {
    let mut ir = ir::Program::default();

    for (bid, body) in program.bodies.iter() {
        println!("Building body: {}", body.name);

        let mut locals = ir::Locals::default();

        let mut ctx = BuildContext {
            hir: program,
            ir: &mut ir,
            locals: &mut locals,
            local_map: &mut HashMap::new(),
            ty_subst: &mut HashMap::new(),
            ty_map: &mut HashMap::new(),
        };

        let ty = ctx.build_type(&body.ty(), true);
        println!("Type: {}", ctx.ir.types.format_type(ty));
    }

    todo!()
}

struct BuildContext<'a> {
    hir: &'a hir::Program,
    ir: &'a mut ir::Program,

    locals: &'a mut ir::Locals,
    local_map: &'a mut HashMap<hir::LocalId, ir::Local>,

    ty_subst: &'a mut HashMap<solve::Var, ir::Type>,
    ty_map: &'a mut HashMap<(solve::Type, bool), ir::Type>,
}

impl BuildContext<'_> {
    fn build_tid(&mut self, ty: &solve::Type, pol: bool) -> ir::Tid {
        let ir = self.build_type(ty, pol);
        self.ir.types.insert(ir)
    }

    fn build_type(&mut self, ty: &solve::Type, pol: bool) -> ir::Type {
        if let Some(ty) = self.ty_map.get(&(ty.clone(), pol)) {
            return ty.clone();
        }

        let mut terms = Vec::new();

        for conjunct in ty.conjuncts() {
            let ir_term = self.build_type_term(&conjunct.positive, pol);
            terms.extend(ir_term.terms);
        }

        let ir = ir::Type { terms };
        self.ty_map.insert((ty.clone(), pol), ir.clone());
        ir
    }

    fn build_type_term(&mut self, term: &solve::Term, pol: bool) -> ir::Type {
        let mut ty = ir::Type {
            terms: vec![ir::Term {
                tags: term.tags.clone(),
                base: self.build_type_base(&term.base, pol),
            }],
        };

        for &var in &term.vars {
            if let Some(subst) = self.ty_subst.get(&var) {
                self.ir.types.intersect(&mut ty, subst);
                continue;
            }

            let bounds = self.hir.tcx.bounds(var);
            let bound = match pol {
                true => self.build_type(&bounds.lower, pol),
                false => self.build_type(&bounds.upper, pol),
            };
            self.ir.types.intersect(&mut ty, &bound);
        }

        for app in term.apps.iter().cloned() {
            let expanded = self.hir.tcx.expand(app);
            let expanded = self.build_type(&expanded, pol);
            self.ir.types.intersect(&mut ty, &expanded);
        }

        ty
    }

    fn build_type_base(&mut self, base: &solve::Base, pol: bool) -> ir::Base {
        match base {
            solve::Base::None => ir::Base::Any,

            solve::Base::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|(name, ty)| (*name, self.build_tid(ty, pol)))
                    .collect();

                ir::Base::Record(fields)
            }

            solve::Base::Tuple(tuple) => {
                let types = tuple
                    .fields
                    .iter()
                    .map(|ty| self.build_tid(ty, pol))
                    .collect();

                ir::Base::Tuple(types)
            }

            solve::Base::Array(array) => {
                let ty = self.build_tid(&array.element, pol);
                ir::Base::Array(ty)
            }

            solve::Base::Function(function) => {
                let input = self.build_tid(&function.input, !pol);
                let output = self.build_tid(&function.output, pol);

                ir::Base::Function(input, output)
            }
        }
    }

    /*
    fn build_expr(&mut self, expr: &hir::Expr) -> ir::Expr {
        match &expr.kind {
            hir::ExprKind::Int(value) => ir::Expr {
                kind: ir::ExprKind::Int(*value),
                ty: expr.ty,
            },

            hir::ExprKind::Float(value) => ir::Expr::Float(*value),

            hir::ExprKind::String(value) => ir::Expr::String(value),

            hir::ExprKind::ZeroSize(tag) => {
                ir::Expr::Tag(Tags::from(*tag), Box::new(ir::Expr::Unit))
            }

            hir::ExprKind::Local(id) => {
                let local = self.local_map[id];
                ir::Expr::Local(local)
            }

            hir::ExprKind::Tag(tag, expr) => todo!(),
            hir::ExprKind::Func(body_id) => todo!(),
            hir::ExprKind::Array(exprs) => todo!(),
            hir::ExprKind::Tuple(exprs) => todo!(),
            hir::ExprKind::Record(inits) => todo!(),
            hir::ExprKind::Index(expr, expr1) => todo!(),
            hir::ExprKind::Field(expr, _) => todo!(),
            hir::ExprKind::Unary(unary_op, expr) => todo!(),
            hir::ExprKind::Binary(binary_op, expr, expr1) => todo!(),
            hir::ExprKind::Call(expr, expr1) => todo!(),
            hir::ExprKind::Lambda {
                captures,
                args,
                locals,
                body,
            } => todo!(),
            hir::ExprKind::Assign(expr, expr1) => todo!(),
            hir::ExprKind::Ref(expr) => todo!(),
            hir::ExprKind::Match(expr, match_body) => todo!(),
            hir::ExprKind::Loop(expr) => todo!(),
            hir::ExprKind::Break(expr) => todo!(),
            hir::ExprKind::Let(binding, expr) => todo!(),
            hir::ExprKind::Block(exprs) => todo!(),
        }
    }
    */
}
