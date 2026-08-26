use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        LayoutBackingPlace, SBlock, SBlockId, SConst, SExpr, SStmt, SStmtKind, STerminatorKind,
        SemConstId, SemConstValue, SemanticBody, SemanticCalleeRef, array_const,
        consts::demand_concrete_const_ty, enum_const, instance::SemanticInstance,
        reify_runtime_const_for_ty, sem_const_from_ty, struct_const, tuple_const,
    },
    ty::ty_def::{BorrowKind, CapabilityKind, TyId},
};
use crate::projection::{IndexSource, Projection};

use super::{CtfeError, eval_const_ref, machine::try_eval_expr_to_const};

type LocalConstMap<'db> = Vec<Option<SemConstId<'db>>>;
type LocalDefs<'db> = Vec<Vec<SExpr<'db>>>;

#[derive(Clone, Copy)]
enum ConstCanonicalizationMode {
    Full,
    Admission,
}

#[derive(Clone, Copy)]
struct ConstCanonicalizationCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a SemanticBody<'db>,
    local_defs: &'a LocalDefs<'db>,
    layout_index_locals: &'a [bool],
    mode: ConstCanonicalizationMode,
}

#[salsa::tracked(return_ref)]
fn canonicalize_semantic_consts_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<SemanticBody<'db>, CtfeError<'db>> {
    let original = instance
        .admitted_body(db)
        .map_err(|_| CtfeError::InvalidBody {
            origin: crate::analysis::semantic::SemOrigin::Body(instance.key(db).owner(db)),
        })?;
    Ok(canonicalize_semantic_consts_from_body(
        db, instance, original,
    ))
}

pub fn canonicalize_semantic_consts<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<&'db SemanticBody<'db>, CtfeError<'db>> {
    canonicalize_semantic_consts_query(db, instance)
        .as_ref()
        .map_err(Clone::clone)
}

pub(crate) fn canonicalize_semantic_consts_from_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    original: &SemanticBody<'db>,
) -> SemanticBody<'db> {
    canonicalize_semantic_consts_from_body_with_mode(
        db,
        instance,
        original,
        ConstCanonicalizationMode::Full,
    )
}

pub(crate) fn canonicalize_semantic_consts_for_admission<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    original: &SemanticBody<'db>,
) -> SemanticBody<'db> {
    canonicalize_semantic_consts_from_body_with_mode(
        db,
        instance,
        original,
        ConstCanonicalizationMode::Admission,
    )
}

fn canonicalize_semantic_consts_from_body_with_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    original: &SemanticBody<'db>,
    mode: ConstCanonicalizationMode,
) -> SemanticBody<'db> {
    let mut body = original.clone();
    if body.blocks.is_empty() {
        return body;
    }
    let local_defs = collect_local_defs(original);
    let layout_index_locals = collect_layout_index_locals(original);
    let cx = ConstCanonicalizationCx {
        db,
        instance,
        body: original,
        local_defs: &local_defs,
        layout_index_locals: &layout_index_locals,
        mode,
    };

    let mut incoming = vec![None; body.blocks.len()];
    incoming[0] = Some(vec![None; body.locals.len()]);
    let mut pending = VecDeque::from([SBlockId::from_u32(0)]);

    while let Some(bb) = pending.pop_front() {
        let Some(mut locals) = incoming[bb.index()].clone() else {
            continue;
        };
        body.blocks[bb.index()] = canonicalize_block(cx, &original.blocks[bb.index()], &mut locals);
        for succ in block_successors(&original.blocks[bb.index()].terminator.kind) {
            if merge_local_consts(&mut incoming[succ.index()], &locals) {
                pending.push_back(succ);
            }
        }
    }

    let mut unknown_locals = vec![None; body.locals.len()];
    for (idx, state) in incoming.iter().enumerate() {
        if state.is_none() {
            body.blocks[idx] = canonicalize_block(cx, &original.blocks[idx], &mut unknown_locals);
            unknown_locals.fill(None);
        }
    }

    body
}

fn canonicalize_block<'db>(
    cx: ConstCanonicalizationCx<'_, 'db>,
    block: &SBlock<'db>,
    locals: &mut LocalConstMap<'db>,
) -> SBlock<'db> {
    SBlock {
        stmts: block
            .stmts
            .iter()
            .map(|stmt| canonicalize_stmt(cx, stmt, locals))
            .collect(),
        terminator: block.terminator.clone(),
    }
}

fn canonicalize_stmt<'db>(
    cx: ConstCanonicalizationCx<'_, 'db>,
    stmt: &SStmt<'db>,
    locals: &mut LocalConstMap<'db>,
) -> SStmt<'db> {
    let kind = match &stmt.kind {
        SStmtKind::Assign { dst, expr } => {
            let (expr, value) = canonicalize_expr(
                cx,
                expr,
                cx.body.locals[dst.index()].ty,
                locals,
                cx.layout_index_locals[dst.index()],
            );
            locals[dst.index()] = value;
            invalidate_mutated_call_locals(cx.db, &expr, locals, cx.body, cx.local_defs);
            SStmtKind::Assign { dst: *dst, expr }
        }
        SStmtKind::Store { dst, src } => {
            locals[dst.local.index()] = None;
            // A store through a mut-borrow carrier also mutates the borrowed
            // locals, so their cached constants are stale.
            let mut memo = vec![None; cx.body.locals.len()];
            let mut visiting = FxHashSet::default();
            for root in writable_local_roots(dst.local, cx.local_defs, &mut memo, &mut visiting) {
                locals[root.index()] = None;
            }
            SStmtKind::Store {
                dst: dst.clone(),
                src: *src,
            }
        }
    };
    SStmt {
        id: stmt.id,
        origin: stmt.origin,
        kind,
    }
}

fn collect_local_defs<'db>(body: &SemanticBody<'db>) -> LocalDefs<'db> {
    let mut defs = vec![Vec::new(); body.locals.len()];
    for stmt in body.blocks.iter().flat_map(|block| &block.stmts) {
        if let SStmtKind::Assign { dst, expr } = &stmt.kind {
            defs[dst.index()].push(expr.clone());
        }
    }
    defs
}

fn collect_layout_index_locals(body: &SemanticBody<'_>) -> Vec<bool> {
    let mut locals = vec![false; body.locals.len()];
    for local in &body.locals {
        for backing in &local.layout_backing_sources {
            let path = match &backing.source {
                LayoutBackingPlace::Local(place) => &place.path,
                LayoutBackingPlace::RootProvider { path, .. } => path,
            };
            for projection in path.iter() {
                if let Projection::Index(IndexSource::Dynamic(local)) = projection {
                    locals[local.index()] = true;
                }
            }
        }
    }
    locals
}

fn invalidate_mutated_call_locals<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: &SExpr<'db>,
    locals: &mut LocalConstMap<'db>,
    body: &SemanticBody<'db>,
    local_defs: &LocalDefs<'db>,
) {
    let SExpr::Call { callee, args, .. } = expr else {
        return;
    };
    let mut memo = vec![None; body.locals.len()];
    let mut visiting = FxHashSet::default();
    for (idx, arg) in args.iter().enumerate() {
        if !callee_arg_is_mutable(db, *callee, idx) {
            continue;
        }
        for root in writable_local_roots(arg.value, local_defs, &mut memo, &mut visiting) {
            locals[root.index()] = None;
        }
    }
}

fn callee_arg_is_mutable<'db>(
    db: &'db dyn HirAnalysisDb,
    callee: SemanticCalleeRef<'db>,
    idx: usize,
) -> bool {
    let callee = SemanticInstance::new(db, callee.key);
    let typed_body = callee.key(db).typed_body(db);
    typed_body.param_binding(idx).is_some_and(|binding| {
        matches!(
            typed_body.binding_ty(db, binding).as_capability(db),
            Some((CapabilityKind::Mut, _))
        )
    })
}

fn writable_local_roots<'db>(
    local: crate::analysis::semantic::SLocalId,
    local_defs: &LocalDefs<'db>,
    memo: &mut [Option<Vec<crate::analysis::semantic::SLocalId>>],
    visiting: &mut FxHashSet<crate::analysis::semantic::SLocalId>,
) -> Vec<crate::analysis::semantic::SLocalId> {
    if let Some(cached) = &memo[local.index()] {
        return cached.clone();
    }
    if !visiting.insert(local) {
        return Vec::new();
    }

    let mut roots = FxHashSet::default();
    for expr in &local_defs[local.index()] {
        match expr {
            SExpr::Borrow {
                place,
                kind: BorrowKind::Mut,
                ..
            } => {
                roots.insert(place.local);
            }
            SExpr::Forward(src) | SExpr::UseValue(src) => {
                roots.extend(writable_local_roots(src.value, local_defs, memo, visiting));
            }
            SExpr::ReadPlace { .. } => {}
            SExpr::CodeRegionRef { .. }
            | SExpr::Const(_)
            | SExpr::Unary { .. }
            | SExpr::Binary { .. }
            | SExpr::Cast { .. }
            | SExpr::ArrayRepeat { .. }
            | SExpr::AggregateMake { .. }
            | SExpr::EnumMake { .. }
            | SExpr::Field { .. }
            | SExpr::Index { .. }
            | SExpr::Borrow { .. }
            | SExpr::GetEnumTag { .. }
            | SExpr::IsEnumVariant { .. }
            | SExpr::ExtractEnumField { .. }
            | SExpr::CodeRegionOffset { .. }
            | SExpr::CodeRegionLen { .. }
            | SExpr::Call { .. } => {}
        }
    }

    visiting.remove(&local);
    let roots = roots.into_iter().collect::<Vec<_>>();
    memo[local.index()] = Some(roots.clone());
    roots
}

fn canonicalize_expr<'db>(
    cx: ConstCanonicalizationCx<'_, 'db>,
    expr: &SExpr<'db>,
    result_ty: TyId<'db>,
    locals: &LocalConstMap<'db>,
    preserves_layout_index: bool,
) -> (SExpr<'db>, Option<SemConstId<'db>>) {
    if let SExpr::Const(SConst::Ref(cref)) = expr {
        let Ok(value) = eval_const_ref(cx.db, *cref) else {
            return (SExpr::Const(SConst::Ref(*cref)), None);
        };
        let value = canonicalize_const_value(cx.db, cx.instance, value);
        let runtime = reify_runtime_const_for_ty(cx.db, cx.instance, result_ty, value);
        return (
            SExpr::Const(runtime.map_or(SConst::Value(value), |_| SConst::Ref(*cref))),
            runtime,
        );
    }

    if matches!(cx.mode, ConstCanonicalizationMode::Full)
        || matches!(cx.mode, ConstCanonicalizationMode::Admission)
            && (matches!(expr, SExpr::Call { .. }) || !preserves_layout_index)
    {
        let has_runtime_evidence = match expr {
            SExpr::Call { callee, .. } => callee
                .key
                .layout_bundle_signature(cx.db)
                .has_runtime_evidence(),
            _ => false,
        };
        if !has_runtime_evidence
            && let Some(value) =
                try_eval_expr_to_const(cx.db, cx.body, result_ty, expr, locals, synthetic())
            && !matches!(value.value(cx.db), SemConstValue::TypeLevel { .. })
        {
            let value = canonicalize_const_value(cx.db, cx.instance, value);
            if let Some(value) = reify_runtime_const_for_ty(cx.db, cx.instance, result_ty, value) {
                return (SExpr::Const(SConst::Value(value)), Some(value));
            }
        }
    }

    match expr {
        SExpr::Const(SConst::Value(value)) => {
            let value = canonicalize_const_value(cx.db, cx.instance, *value);
            let runtime = reify_runtime_const_for_ty(cx.db, cx.instance, result_ty, value);
            let value = runtime.unwrap_or(value);
            (SExpr::Const(SConst::Value(value)), runtime)
        }
        _ => (expr.clone(), None),
    }
}

fn merge_local_consts<'db>(
    current: &mut Option<LocalConstMap<'db>>,
    incoming: &LocalConstMap<'db>,
) -> bool {
    match current {
        None => {
            *current = Some(incoming.clone());
            true
        }
        Some(current) => {
            let mut changed = false;
            for (slot, incoming) in current.iter_mut().zip(incoming.iter().copied()) {
                let merged = if *slot == incoming { incoming } else { None };
                if *slot != merged {
                    *slot = merged;
                    changed = true;
                }
            }
            changed
        }
    }
}

fn block_successors<'db>(term: &STerminatorKind<'db>) -> Vec<SBlockId> {
    match term {
        STerminatorKind::Goto(bb) => vec![*bb],
        STerminatorKind::Branch {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        STerminatorKind::MatchEnum { cases, default, .. } => {
            let mut succs = cases.iter().map(|(_, bb)| *bb).collect::<Vec<_>>();
            if let Some(default) = default {
                succs.push(*default);
            }
            succs
        }
        STerminatorKind::Assert { .. } | STerminatorKind::Return(None | Some(_)) => Vec::new(),
    }
}

fn synthetic<'db>() -> crate::analysis::semantic::SemOrigin<'db> {
    crate::analysis::semantic::SemOrigin::Synthetic
}

fn canonicalize_const_value<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    value: SemConstId<'db>,
) -> SemConstId<'db> {
    match value.value(db) {
        SemConstValue::Unit | SemConstValue::Scalar { .. } => value,
        SemConstValue::TypeLevel { ty, const_ty } => {
            let Some(evaluated) = demand_concrete_const_ty(
                db,
                const_ty,
                ty,
                instance.key(db).subst(db).generic_args(db),
            ) else {
                return value;
            };
            let Some(evaluated) = sem_const_from_ty(db, TyId::const_ty(db, evaluated)) else {
                return value;
            };
            if matches!(evaluated.value(db), SemConstValue::TypeLevel { .. }) {
                // Canonicalization did not produce a runtime value. Preserve
                // the original symbolic reference so a formal const-parameter
                // use keeps its exact parameter identity; replacing it with
                // an instantiated hole would make runtime ABI selection rely
                // on structural root matching.
                value
            } else {
                evaluated
            }
        }
        SemConstValue::Tuple { ty, elems } => tuple_const(
            db,
            ty,
            elems
                .iter()
                .copied()
                .map(|elem| canonicalize_const_value(db, instance, elem))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        SemConstValue::Struct { ty, fields } => struct_const(
            db,
            ty,
            fields
                .iter()
                .copied()
                .map(|field| canonicalize_const_value(db, instance, field))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        SemConstValue::Array { ty, elems } => array_const(
            db,
            ty,
            elems
                .iter()
                .copied()
                .map(|elem| canonicalize_const_value(db, instance, elem))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        SemConstValue::Enum {
            ty,
            variant,
            fields,
        } => enum_const(
            db,
            ty,
            variant,
            fields
                .iter()
                .copied()
                .map(|field| canonicalize_const_value(db, instance, field))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
    }
}
