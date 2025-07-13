//! Simple specialization of a HIR2 program. XXX: Delete this.
use crate::anf::{Type, Types};
use crate::hir2;

pub fn simple_spec(program: &hir2::Program) -> hir2::SpecializedProgram {
    let mut specialized_program = hir2::SpecializedProgram::default();
    let mut type_converter = TypeConverter::new();

    // Transform each body from the original program
    for (_body_id, body) in program.bodies.iter() {
        // Crash if we encounter generics (as requested)
        if !body.generics.is_empty() {
            panic!(
                "Generics not supported in simple specializer! Found {} generics in body '{}'",
                body.generics.len(),
                body.name
            );
        }

        // Convert Body to SpecializedBody
        let specialized_body = hir2::SpecializedBody {
            attrs: body.attrs.clone(),
            is_extern: body.is_extern,
            name: body.name.clone(),
            locals: convert_locals(&body.locals, &mut type_converter),
            input: body
                .input
                .iter()
                .map(|arg| hir2::Argument {
                    binding: arg.binding.clone(),
                    ty: type_converter.convert_type(arg.ty.clone()),
                })
                .collect(),
            output: type_converter.convert_type(body.output.clone()),
            expr: body
                .expr
                .as_ref()
                .map(|expr| convert_expr(expr, &mut type_converter)),
            span: body.span,
        };

        specialized_program.bodies.insert(specialized_body);
    }

    specialized_program.types = type_converter.types;

    // Success! The simple specializer completed without crashing on generics
    eprintln!(
        "✅ Simple specialization completed successfully for {} bodies",
        specialized_program.bodies.bodies.len()
    );

    specialized_program
}

struct TypeConverter {
    types: Types,
}

impl TypeConverter {
    fn new() -> Self {
        Self {
            types: Types::new(),
        }
    }

    fn convert_type(&mut self, ty: solve::Type) -> Type {
        // Handle empty conjuncts (bottom type)
        if ty.conjuncts().is_empty() {
            return Type::any(); // Treat bottom as any for simplicity
        }

        // Convert each conjunct and collect the resulting terms
        let mut all_terms = Vec::new();

        for conjunct in ty.conjuncts() {
            // Skip conjuncts that contain type variables
            if !conjunct.positive.vars.is_empty() || !conjunct.negative.vars.is_empty() {
                // If any conjunct has variables, fall back to any type
                return Type::any();
            }

            // Skip conjuncts with apps for now
            if !conjunct.positive.apps.is_empty() || !conjunct.negative.apps.is_empty() {
                return Type::any();
            }

            // Convert the conjunct to terms
            let terms = self.convert_conjunct(conjunct);
            all_terms.extend(terms);
        }

        // If we couldn't convert any terms, fall back to any
        if all_terms.is_empty() {
            return Type::any();
        }

        Type { terms: all_terms }
    }

    fn convert_conjunct(&mut self, conjunct: &solve::Conjunct) -> Vec<crate::anf::Term> {
        use crate::anf::{Base, Term};
        use solve::Base as SolveBase;

        let mut terms = Vec::new();

        // Handle positive tags
        if !conjunct.positive.tags.is_empty() {
            for tag in conjunct.positive.tags.iter() {
                let mut tags = solve::Tags::default();
                tags.insert(tag);

                terms.push(Term {
                    tags,
                    base: Base::Any,
                });
            }
        }

        // Handle positive base types
        match &conjunct.positive.base {
            SolveBase::None => {
                // If no specific base, but we have tags, that's handled above
                if conjunct.positive.tags.is_empty() {
                    // Pure structural constraint - convert to any
                    terms.push(Term {
                        tags: solve::Tags::default(),
                        base: Base::Any,
                    });
                }
            }
            SolveBase::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|(name, field_ty)| {
                        let converted_ty = self.convert_type(field_ty.clone());
                        let tid = self.types.insert(converted_ty);
                        (*name, tid)
                    })
                    .collect();

                terms.push(Term {
                    tags: conjunct.positive.tags.clone(),
                    base: Base::Record(fields),
                });
            }
            SolveBase::Tuple(tuple) => {
                let field_tids = tuple
                    .fields
                    .iter()
                    .map(|field_ty| {
                        let converted_ty = self.convert_type(field_ty.clone());
                        self.types.insert(converted_ty)
                    })
                    .collect();

                terms.push(Term {
                    tags: conjunct.positive.tags.clone(),
                    base: Base::Tuple(field_tids),
                });
            }
            SolveBase::Array(array) => {
                let element_ty = self.convert_type(array.element.clone());
                let element_tid = self.types.insert(element_ty);

                terms.push(Term {
                    tags: conjunct.positive.tags.clone(),
                    base: Base::Array(element_tid),
                });
            }
            SolveBase::Function(function) => {
                let input_ty = self.convert_type(function.input.clone());
                let output_ty = self.convert_type(function.output.clone());
                let input_tid = self.types.insert(input_ty);
                let output_tid = self.types.insert(output_ty);

                terms.push(Term {
                    tags: conjunct.positive.tags.clone(),
                    base: Base::Function(input_tid, output_tid),
                });
            }
        }

        // If we have negative constraints, we need to handle them too
        // For now, we'll ignore them in the simple specializer
        // TODO: Handle negative constraints properly

        terms
    }
}

fn convert_locals(
    locals: &hir2::Locals<solve::Type>,
    type_converter: &mut TypeConverter,
) -> hir2::Locals<Type> {
    let mut new_locals = hir2::Locals::new();

    for local_id in locals.ids() {
        let local = &locals[local_id];
        let new_local = hir2::Local {
            is_mutable: local.is_mutable,
            name: local.name,
            ty: type_converter.convert_type(local.ty.clone()),
            span: local.span,
        };
        new_locals.insert(new_local);
    }

    new_locals
}

fn convert_expr(expr: &hir2::Expr, type_converter: &mut TypeConverter) -> hir2::Expr<Type> {
    hir2::Expr {
        kind: convert_expr_kind(&expr.kind, type_converter),
        ty: type_converter.convert_type(expr.ty.clone()),
        span: expr.span,
    }
}

fn convert_expr_kind(
    kind: &hir2::ExprKind,
    type_converter: &mut TypeConverter,
) -> hir2::ExprKind<Type> {
    match kind {
        hir2::ExprKind::Int(i) => hir2::ExprKind::Int(*i),
        hir2::ExprKind::Float(f) => hir2::ExprKind::Float(*f),
        hir2::ExprKind::ZeroSize(tag) => hir2::ExprKind::ZeroSize(*tag),
        hir2::ExprKind::String(s) => hir2::ExprKind::String(*s),
        hir2::ExprKind::Local(local_id) => hir2::ExprKind::Local(*local_id),
        hir2::ExprKind::Block(exprs) => hir2::ExprKind::Block(
            exprs
                .iter()
                .map(|e| convert_expr(e, type_converter))
                .collect(),
        ),
        hir2::ExprKind::Func(body_id) => hir2::ExprKind::Func(*body_id),
        hir2::ExprKind::Call(func, arg) => hir2::ExprKind::Call(
            Box::new(convert_expr(func, type_converter)),
            Box::new(convert_expr(arg, type_converter)),
        ),
        hir2::ExprKind::Binary(op, left, right) => hir2::ExprKind::Binary(
            *op,
            Box::new(convert_expr(left, type_converter)),
            Box::new(convert_expr(right, type_converter)),
        ),
        hir2::ExprKind::Match(expr, match_body) => hir2::ExprKind::Match(
            Box::new(convert_expr(expr, type_converter)),
            convert_match_body(match_body, type_converter),
        ),
        hir2::ExprKind::Let(binding, expr) => hir2::ExprKind::Let(
            binding.clone(),
            Box::new(convert_expr(expr, type_converter)),
        ),
        _ => panic!(
            "Expression kind conversion not implemented in simple specializer: {:?}",
            kind
        ),
    }
}

fn convert_match_body(
    match_body: &hir2::MatchBody,
    type_converter: &mut TypeConverter,
) -> hir2::MatchBody<Type> {
    hir2::MatchBody {
        arms: match_body
            .arms
            .iter()
            .map(|arm| convert_arm(arm, type_converter))
            .collect(),
        default: match_body
            .default
            .as_ref()
            .map(|default| Box::new((default.0.clone(), convert_expr(&default.1, type_converter)))),
    }
}

fn convert_arm(arm: &hir2::Arm, type_converter: &mut TypeConverter) -> hir2::Arm<Type> {
    hir2::Arm {
        pattern: arm.pattern.clone(),
        body: convert_expr(&arm.body, type_converter),
        span: arm.span,
    }
}
