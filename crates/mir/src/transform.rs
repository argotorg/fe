use hir::analysis::HirAnalysisDb;
use hir::projection::{IndexSource, Projection};
use rustc_hash::FxHashSet;

use crate::ir::{
    MirBody, MirInst, Rvalue, TerminatingCall, Terminator, ValueData, ValueId, ValueOrigin,
};
use crate::layout;

struct StabilizeCtx<'db, 'a, 'b> {
    db: &'db dyn HirAnalysisDb,
    values: &'a [ValueData<'db>],
    value_use_counts: &'a [usize],
    bound_in_block: &'b mut FxHashSet<ValueId>,
    rewritten: &'b mut Vec<MirInst<'db>>,
}

impl<'db> StabilizeCtx<'db, '_, '_> {
    fn stabilize_terminator(&mut self, term: &Terminator<'db>) {
        match term {
            Terminator::Return(Some(value)) => self.stabilize_value(*value, true, false),
            Terminator::TerminatingCall(call) => match call {
                TerminatingCall::Call(call) => {
                    for arg in call.args.iter().chain(call.effect_args.iter()) {
                        self.stabilize_value(*arg, true, false);
                    }
                }
                TerminatingCall::Intrinsic { args, .. } => {
                    for arg in args {
                        self.stabilize_value(*arg, true, false);
                    }
                }
            },
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
                    MirInst::Assign { stmt, dest, rvalue } => {
                        match &rvalue {
                            Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
                            Rvalue::Value(value) => {
                                ctx.stabilize_value(*value, dest.is_some(), false);
                            }
                            Rvalue::Call(call) => {
                                for arg in call.args.iter().chain(call.effect_args.iter()) {
                                    ctx.stabilize_value(*arg, true, false);
                                }
                            }
                            Rvalue::Intrinsic { args, .. } => {
                                for arg in args {
                                    ctx.stabilize_value(*arg, true, false);
                                }
                            }
                            Rvalue::Load { place } => {
                                ctx.stabilize_value(place.base, true, false);
                                ctx.stabilize_path(&place.projection);
                            }
                        }
                        ctx.rewritten.push(MirInst::Assign { stmt, dest, rvalue });
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
                }
            }

            ctx.stabilize_terminator(&block.terminator);
        }
        block.insts = rewritten;
    }
}

/// Canonicalize zero-sized types (ZSTs) in MIR.
///
/// After this pass:
/// - ZST-returning ops do not produce runtime values; their values are rewritten to `Unit`.
/// - `Eval`/`EvalValue` instructions for ZST values are removed (their effects
///   are preserved by explicit effectful MIR instructions (`Call`/`Intrinsic`/`Load`/`Alloc`).
/// - `Store`/`InitAggregate` instructions of ZST values are removed, but any
///   dynamic index computations and RHS evaluations are preserved via inserted
///   `EvalValue`s to maintain evaluation order.
/// - `Return(Some(v))` where `v` is ZST is rewritten to `Return(None)`.
pub(crate) fn canonicalize_zero_sized<'db>(db: &'db dyn HirAnalysisDb, body: &mut MirBody<'db>) {
    fn is_zst(db: &dyn HirAnalysisDb, ty: hir::analysis::ty::ty_def::TyId<'_>) -> bool {
        layout::is_zero_sized_ty(db, ty)
    }

    fn push_eval_value<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut [ValueData<'db>],
        out: &mut Vec<MirInst<'db>>,
        value: ValueId,
    ) {
        let value_ty = values[value.index()].ty;
        if !is_zst(db, value_ty) {
            out.push(MirInst::Assign {
                stmt: None,
                dest: None,
                rvalue: Rvalue::Value(value),
            });
            return;
        }

        // ZST: the value has no runtime representation.
        values[value.index()].origin = ValueOrigin::Unit;
    }

    fn push_place_eval<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut [ValueData<'db>],
        out: &mut Vec<MirInst<'db>>,
        place: &crate::ir::Place<'db>,
    ) {
        push_eval_value(db, values, out, place.base);
        for proj in place.projection.iter() {
            if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                push_eval_value(db, values, out, *value);
            }
        }
    }

    fn push_path_eval<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut [ValueData<'db>],
        out: &mut Vec<MirInst<'db>>,
        path: &crate::ir::MirProjectionPath<'db>,
    ) {
        for proj in path.iter() {
            if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                push_eval_value(db, values, out, *value);
            }
        }
    }

    let zst_locals: Vec<bool> = body
        .locals
        .iter()
        .map(|local| is_zst(db, local.ty))
        .collect();
    let (blocks, values) = (&mut body.blocks, &mut body.values);
    for block in blocks {
        let mut rewritten: Vec<MirInst<'db>> = Vec::with_capacity(block.insts.len());
        for inst in std::mem::take(&mut block.insts) {
            match inst {
                MirInst::Assign { stmt, dest, rvalue } => match dest {
                    Some(dest) if zst_locals.get(dest.index()).copied().unwrap_or(false) => {
                        // Dest is ZST: keep side effects, drop runtime write.
                        match rvalue {
                            Rvalue::Call(call) => rewritten.push(MirInst::Assign {
                                stmt,
                                dest: None,
                                rvalue: Rvalue::Call(call),
                            }),
                            Rvalue::Intrinsic { op, args } => rewritten.push(MirInst::Assign {
                                stmt,
                                dest: None,
                                rvalue: Rvalue::Intrinsic { op, args },
                            }),
                            Rvalue::Value(value) => {
                                // Pure value, no runtime representation needed.
                                if is_zst(db, values[value.index()].ty) {
                                    values[value.index()].origin = ValueOrigin::Unit;
                                }
                            }
                            Rvalue::Load { place } => {
                                // Even though the loaded value is ZST (so the write can be
                                // dropped), we must still evaluate the load's place to preserve
                                // any side effects in the base/index expressions.
                                push_place_eval(db, values, &mut rewritten, &place);
                            }
                            Rvalue::Alloc { .. } | Rvalue::ZeroInit => {}
                        }
                    }
                    _ => {
                        // Dest is non-ZST (or none): canonicalize ZST-valued evals.
                        if dest.is_none()
                            && let Rvalue::Value(value) = &rvalue
                        {
                            let value_ty = values[value.index()].ty;
                            if is_zst(db, value_ty) {
                                values[value.index()].origin = ValueOrigin::Unit;
                                continue;
                            }
                        }
                        rewritten.push(MirInst::Assign { stmt, dest, rvalue });
                    }
                },
                MirInst::BindValue { value } => {
                    let value_ty = values[value.index()].ty;
                    if is_zst(db, value_ty) {
                        push_eval_value(db, values, &mut rewritten, value);
                    } else {
                        rewritten.push(MirInst::BindValue { value });
                    }
                }
                MirInst::Store { place, value } => {
                    let value_ty = values[value.index()].ty;
                    if is_zst(db, value_ty) {
                        push_place_eval(db, values, &mut rewritten, &place);
                        push_eval_value(db, values, &mut rewritten, value);
                    } else {
                        rewritten.push(MirInst::Store { place, value });
                    }
                }
                MirInst::InitAggregate { place, inits } => {
                    let base_ty = values[place.base.index()].ty;
                    if is_zst(db, base_ty) {
                        push_place_eval(db, values, &mut rewritten, &place);
                        for (path, value) in &inits {
                            push_path_eval(db, values, &mut rewritten, path);
                            push_eval_value(db, values, &mut rewritten, *value);
                        }
                    } else {
                        let mut kept: Vec<(crate::ir::MirProjectionPath<'db>, ValueId)> =
                            Vec::with_capacity(inits.len());
                        let mut removed_any = false;
                        for (path, value) in inits {
                            let value_ty = values[value.index()].ty;
                            if is_zst(db, value_ty) {
                                if !removed_any {
                                    push_place_eval(db, values, &mut rewritten, &place);
                                    removed_any = true;
                                }
                                push_path_eval(db, values, &mut rewritten, &path);
                                push_eval_value(db, values, &mut rewritten, value);
                            } else {
                                kept.push((path, value));
                            }
                        }
                        if kept.is_empty() {
                            if !removed_any {
                                // No inits to keep, but still preserve evaluation of the base.
                                push_place_eval(db, values, &mut rewritten, &place);
                            }
                        } else {
                            rewritten.push(MirInst::InitAggregate { place, inits: kept });
                        }
                    }
                }
                other => rewritten.push(other),
            }
        }

        if let Terminator::Return(Some(value)) = &mut block.terminator {
            let ty = values[value.index()].ty;
            if is_zst(db, ty) {
                // Ensure any side effects are emitted before the return.
                push_eval_value(db, values, &mut rewritten, *value);
                block.terminator = Terminator::Return(None);
            }
        }

        block.insts = rewritten;
    }

    // Ensure no runtime value references a zero-sized local.
    //
    // The instruction forms that "define" locals (e.g. `Alloc`, `Load`, `Call` dests) can be
    // removed for ZSTs above. Any MIR `ValueId` that still points at such locals must be
    // canonicalized to `Unit` so codegen never needs to bind/resolve a runtime representation.
    for value in values.iter_mut() {
        if let ValueOrigin::Local(local) = &value.origin
            && zst_locals.get(local.index()).copied().unwrap_or(false)
        {
            value.origin = ValueOrigin::Unit;
        }
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
    if force_root_bind {
        return true;
    }
    if layout::is_zero_sized_ty(db, value.ty) {
        return false;
    }
    value_use_counts.get(value_id.index()).copied().unwrap_or(0) > 1
        && !matches!(
            origin,
            ValueOrigin::Unit
                | ValueOrigin::Synthetic(..)
                | ValueOrigin::Local(..)
                | ValueOrigin::FuncItem(..)
        )
}

fn value_deps_in_eval_order(origin: &ValueOrigin<'_>) -> Vec<ValueId> {
    match origin {
        ValueOrigin::Unary { inner, .. } => vec![*inner],
        ValueOrigin::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
        ValueOrigin::FieldPtr(field_ptr) => vec![field_ptr.base],
        ValueOrigin::PlaceRef(place) => {
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
        | ValueOrigin::FuncItem(..) => Vec::new(),
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
                MirInst::BindValue { value } => bump(*value),
                MirInst::Assign { rvalue, .. } => match rvalue {
                    Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
                    Rvalue::Value(value) => bump(*value),
                    Rvalue::Call(call) => {
                        for arg in call.args.iter().chain(call.effect_args.iter()) {
                            bump(*arg);
                        }
                    }
                    Rvalue::Intrinsic { args, .. } => {
                        for arg in args {
                            bump(*arg);
                        }
                    }
                    Rvalue::Load { place } => {
                        bump(place.base);
                        bump_place_path(&mut bump, &place.projection);
                    }
                },
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
            }
        }

        match &block.terminator {
            Terminator::Return(Some(value)) => bump(*value),
            Terminator::TerminatingCall(call) => match call {
                TerminatingCall::Call(call) => {
                    for arg in call.args.iter().chain(call.effect_args.iter()) {
                        bump(*arg);
                    }
                }
                TerminatingCall::Intrinsic { args, .. } => {
                    for arg in args {
                        bump(*arg);
                    }
                }
            },
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
