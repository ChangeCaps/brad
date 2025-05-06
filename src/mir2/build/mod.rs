use crate::{
    hir2 as hir,
    mir2::ty::{Ty, TyKind},
    solve,
};

use super::ty::Tcx;

pub fn build(hir: &hir::Program) {
    let mut tcx = Tcx::new();

    for (_id, body) in hir.bodies.iter() {
        let ty = build_ty(&mut tcx, &hir.solver, body.ty());
    }
}

fn build_ty(tcx: &mut Tcx, solver: &solve::Solver, ty: solve::Ty) -> Ty {
    let mut variants = Vec::new();

    for conj in ty {
        let term = conj.pos;

        let mut ty = Ty {
            tags: term.tags,
            kind: TyKind::Any,
        };

        for var in term.vars {
            let bounds = solver.bounds(var);
            let lower = build_ty(tcx, solver, bounds.lower.clone());
            ty = unify_tys(tcx, ty, lower);
        }

        let base = match term.base {
            Some(solve::Base::Ref(refee)) => {
                let refee = build_ty(tcx, solver, refee);
                let refee = tcx.push(refee);
                Ty::from(TyKind::Ref { refee })
            }

            Some(solve::Base::Array(item)) => {
                let item = build_ty(tcx, solver, item);
                let item = tcx.push(item);
                Ty::from(TyKind::Array { item })
            }

            Some(solve::Base::Func(input, output)) => {
                let input = build_ty(tcx, solver, input);
                let input = tcx.push(input);

                let output = build_ty(tcx, solver, output);
                let output = tcx.push(output);

                Ty::from(TyKind::Func { input, output })
            }

            Some(solve::Base::Tuple(items)) => {
                let items = items
                    .into_iter()
                    .map(|item| {
                        let item = build_ty(tcx, solver, item);
                        tcx.push(item)
                    })
                    .collect();

                Ty::from(TyKind::Tuple { items })
            }

            Some(solve::Base::Record(fields)) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, item)| {
                        let item = build_ty(tcx, solver, item);
                        let item = tcx.push(item);
                        (name, item)
                    })
                    .collect();

                Ty::from(TyKind::Record { fields })
            }

            None => Ty::from(TyKind::Any),
        };

        ty = unify_tys(tcx, ty, base);
        variants.push(ty);
    }

    match variants.len() {
        0 => Ty::from(TyKind::Any),
        1 => variants.pop().unwrap(),
        _ => {
            let variants = variants.into_iter().map(|ty| tcx.push(ty)).collect();
            Ty::from(TyKind::Union { variants })
        }
    }
}

fn unify_tys(tcx: &mut Tcx, a: Ty, b: Ty) -> Ty {
    let tags = a.tags.union(&b.tags).copied().collect();

    let kind = match (a.kind, b.kind) {
        (TyKind::Any, other) | (other, TyKind::Any) => other,
        (TyKind::Never, TyKind::Never) => TyKind::Never,

        (TyKind::Ref { refee: a }, TyKind::Ref { refee: b }) => {
            let a = tcx[a].clone();
            let b = tcx[b].clone();

            let refee = unify_tys(tcx, a, b);
            let refee = tcx.push(refee);

            TyKind::Ref { refee }
        }

        (_, _) => TyKind::Never,
    };

    Ty { tags, kind }
}
