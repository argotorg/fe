use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::ty_def::TyId;
use hir::projection::{IndexSource, Projection};
use rustc_hash::FxHashSet;

use crate::ir::{MirBody, MirInst, Terminator, ValueData, ValueId, ValueOrigin};

struct StabilizeCtx<'db, 'a, 'b> {
    db: &'db dyn HirAnalysisDb,
    values: &'a [ValueData<'db>],
    value_use_counts: &'a [usize],
    bound_in_block: &'b mut FxHashSet<ValueId>,
    rewritten: &'b mut Vec<MirInst<'db>>,
}

impl<'db> StabilizeCtx<'db, '_, '_> {
    fn stabilize_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Return(Some(value)) => self.stabilize_value(*value, true, false),
            Terminator::ReturnData { offset, size } | Terminator::Revert { offset, size } => {
                self.stabilize_value(*offset, true, false);
                self.stabilize_value(*size, true, false);
            }
            Terminator::Branch { cond, .. } => self.stabilize_value(*cond, true, false),
            Terminator::Switch { discr, .. } => self.stabilize_value(*discr, true, false),
            Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
        }
    }

    fn stabilize_path(&mut self, path: &crate::ir::MirProjectionPath<'db>) {
        for proj in path.iter() {
            if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                self.stabilize_value(*value, true, false);
            }
        }
    }

    fn stabilize_value(&mut self, value: ValueId, bind_root: bool, force_root_bind: bool) {
        let mut visiting: FxHashSet<ValueId> = FxHashSet::default();
        self.stabilize_value_inner(value, bind_root, force_root_bind, &mut visiting);
    }

    fn stabilize_value_inner(
        &mut self,
        value: ValueId,
        bind_root: bool,
        force_root_bind: bool,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }

        let origin = &self.values[value.index()].origin;
        for dep in value_deps_in_eval_order(origin) {
            self.stabilize_value_inner(dep, true, false, visiting);
        }

        if bind_root
            && value_should_bind(
                self.db,
                value,
                &self.values[value.index()],
                origin,
                self.value_use_counts,
                force_root_bind,
            )
            && self.bound_in_block.insert(value)
        {
            self.rewritten.push(MirInst::BindValue { value });
        }
    }
}

pub(crate) fn insert_temp_binds<'db>(db: &'db dyn HirAnalysisDb, body: &mut MirBody<'db>) {
    let value_use_counts = compute_value_use_counts(body);
    let (blocks, values) = (&mut body.blocks, &body.values);
    for block in blocks {
        let mut bound_in_block: FxHashSet<ValueId> = FxHashSet::default();
        let mut rewritten: Vec<MirInst<'db>> = Vec::with_capacity(block.insts.len());
        {
            let mut ctx = StabilizeCtx {
                db,
                values,
                value_use_counts: &value_use_counts,
                bound_in_block: &mut bound_in_block,
                rewritten: &mut rewritten,
            };

            for inst in std::mem::take(&mut block.insts) {
                match inst {
                    MirInst::BindValue { value } => {
                        ctx.stabilize_value(value, true, true);
                    }
                    MirInst::Eval { stmt, value } => {
                        ctx.stabilize_value(value, false, false);
                        ctx.rewritten.push(MirInst::Eval { stmt, value });
                    }
                    MirInst::EvalValue { value } => {
                        ctx.stabilize_value(value, false, false);
                        ctx.rewritten.push(MirInst::EvalValue { value });
                    }
                    MirInst::LocalInit {
                        stmt,
                        local,
                        ty,
                        value,
                    } => {
                        if let Some(value) = value {
                            ctx.stabilize_value(value, true, false);
                        }
                        ctx.rewritten.push(MirInst::LocalInit {
                            stmt,
                            local,
                            ty,
                            value,
                        });
                    }
                    MirInst::LocalSet { stmt, local, value } => {
                        ctx.stabilize_value(value, true, false);
                        ctx.rewritten.push(MirInst::LocalSet { stmt, local, value });
                    }
                    MirInst::LocalAugAssign {
                        stmt,
                        local,
                        value,
                        op,
                    } => {
                        ctx.stabilize_value(value, true, false);
                        ctx.rewritten.push(MirInst::LocalAugAssign {
                            stmt,
                            local,
                            value,
                            op,
                        });
                    }
                    MirInst::Store { place, value } => {
                        ctx.stabilize_value(place.base, true, false);
                        ctx.stabilize_path(&place.projection);
                        ctx.stabilize_value(value, true, false);
                        ctx.rewritten.push(MirInst::Store { place, value });
                    }
                    MirInst::InitAggregate { place, inits } => {
                        ctx.stabilize_value(place.base, true, false);
                        ctx.stabilize_path(&place.projection);
                        for (path, value) in &inits {
                            ctx.stabilize_path(path);
                            ctx.stabilize_value(*value, true, false);
                        }
                        ctx.rewritten.push(MirInst::InitAggregate { place, inits });
                    }
                    MirInst::SetDiscriminant { place, variant } => {
                        ctx.stabilize_value(place.base, true, false);
                        ctx.stabilize_path(&place.projection);
                        ctx.rewritten
                            .push(MirInst::SetDiscriminant { place, variant });
                    }
                    MirInst::IntrinsicStmt { op, args } => {
                        for arg in &args {
                            ctx.stabilize_value(*arg, true, false);
                        }
                        ctx.rewritten.push(MirInst::IntrinsicStmt { op, args });
                    }
                }
            }

            ctx.stabilize_terminator(&block.terminator);
        }
        block.insts = rewritten;
    }
}

fn value_should_bind(
    db: &dyn HirAnalysisDb,
    value_id: ValueId,
    value: &ValueData<'_>,
    origin: &ValueOrigin<'_>,
    value_use_counts: &[usize],
    force_root_bind: bool,
) -> bool {
    if matches!(origin, ValueOrigin::Alloc { .. }) {
        return true;
    }
    if is_zero_sized_ty(db, value.ty) {
        return false;
    }
    if force_root_bind {
        return true;
    }
    value_use_counts.get(value_id.index()).copied().unwrap_or(0) > 1
        && value_requires_single_eval(origin)
}

fn value_requires_single_eval(origin: &ValueOrigin<'_>) -> bool {
    matches!(
        origin,
        ValueOrigin::Call(..) | ValueOrigin::Intrinsic(..) | ValueOrigin::PlaceLoad(..)
    )
}

fn is_zero_sized_ty(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
    if (ty.is_tuple(db) && ty.field_count(db) == 0) || ty.is_never(db) {
        return true;
    }
    crate::layout::ty_size_bytes(db, ty).is_some_and(|size| size == 0)
}

fn value_deps_in_eval_order(origin: &ValueOrigin<'_>) -> Vec<ValueId> {
    match origin {
        ValueOrigin::Unary { inner, .. } => vec![*inner],
        ValueOrigin::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
        ValueOrigin::Call(call) => call
            .args
            .iter()
            .chain(call.effect_args.iter())
            .copied()
            .collect(),
        ValueOrigin::Intrinsic(intr) => intr.args.clone(),
        ValueOrigin::FieldPtr(field_ptr) => vec![field_ptr.base],
        ValueOrigin::PlaceLoad(place) | ValueOrigin::PlaceRef(place) => {
            let mut deps = vec![place.base];
            for proj in place.projection.iter() {
                if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                    deps.push(*value);
                }
            }
            deps
        }
        ValueOrigin::Expr(..)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Synthetic(..)
        | ValueOrigin::Local(..)
        | ValueOrigin::FuncItem(..)
        | ValueOrigin::Alloc { .. } => Vec::new(),
    }
}

fn compute_value_use_counts<'db>(body: &MirBody<'db>) -> Vec<usize> {
    let mut counts = vec![0usize; body.values.len()];

    let mut bump = |value: ValueId| {
        if let Some(slot) = counts.get_mut(value.index()) {
            *slot += 1;
        }
    };

    for value in &body.values {
        for dep in value_deps_in_eval_order(&value.origin) {
            bump(dep);
        }
    }

    for block in &body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::BindValue { value } | MirInst::EvalValue { value } => bump(*value),
                MirInst::Eval { value, .. } => bump(*value),
                MirInst::LocalInit { value, .. } => {
                    if let Some(value) = value {
                        bump(*value);
                    }
                }
                MirInst::LocalSet { value, .. } | MirInst::LocalAugAssign { value, .. } => {
                    bump(*value);
                }
                MirInst::Store { place, value } => {
                    bump(place.base);
                    bump_place_path(&mut bump, &place.projection);
                    bump(*value);
                }
                MirInst::InitAggregate { place, inits } => {
                    bump(place.base);
                    bump_place_path(&mut bump, &place.projection);
                    for (path, value) in inits {
                        bump_place_path(&mut bump, path);
                        bump(*value);
                    }
                }
                MirInst::SetDiscriminant { place, .. } => {
                    bump(place.base);
                    bump_place_path(&mut bump, &place.projection);
                }
                MirInst::IntrinsicStmt { args, .. } => {
                    for arg in args {
                        bump(*arg);
                    }
                }
            }
        }

        match &block.terminator {
            Terminator::Return(Some(value)) => bump(*value),
            Terminator::ReturnData { offset, size } | Terminator::Revert { offset, size } => {
                bump(*offset);
                bump(*size);
            }
            Terminator::Branch { cond, .. } => bump(*cond),
            Terminator::Switch { discr, .. } => bump(*discr),
            Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
        }
    }

    counts
}

fn bump_place_path<'db>(bump: &mut impl FnMut(ValueId), path: &crate::ir::MirProjectionPath<'db>) {
    for proj in path.iter() {
        if let Projection::Index(IndexSource::Dynamic(value)) = proj {
            bump(*value);
        }
    }
}
