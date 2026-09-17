use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        LayoutBackingPlace, SBlock, SBlockId, SConst, SEffectArgValue, SExpr, SLocalId, SStmt,
        SStmtKind, STerminatorKind, SemConstId, SemConstValue, SemanticBody, array_const,
        consts::demand_concrete_const_ty, enum_const, instance::SemanticInstance,
        reify_runtime_const_for_ty, sem_const_from_ty, struct_const, tuple_const,
    },
    ty::ty_def::{BorrowKind, TyId},
};
use crate::projection::{IndexSource, Projection};

use super::{CtfeError, eval_const_ref, machine::try_eval_expr_to_const};

type LocalConstMap<'db> = Vec<Option<SemConstId<'db>>>;
type LocalRoots = Vec<FxHashSet<SLocalId>>;

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
    local_roots: &'a LocalRoots,
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
    let local_roots = collect_local_roots(original);
    let layout_index_locals = collect_layout_index_locals(original);
    let cx = ConstCanonicalizationCx {
        db,
        instance,
        body: original,
        local_roots: &local_roots,
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
            let (canonical, value) = canonicalize_expr(
                cx,
                expr,
                cx.body.locals[dst.index()].ty,
                locals,
                cx.layout_index_locals[dst.index()],
            );
            locals[dst.index()] = value;
            if let SExpr::Call {
                args, effect_args, ..
            } = expr
            {
                for arg in args {
                    for root in &cx.local_roots[arg.value.index()] {
                        locals[root.index()] = None;
                    }
                }
                for arg in effect_args {
                    let local = match &arg.arg {
                        SEffectArgValue::Value(value) => value.value,
                        SEffectArgValue::Place(place) => {
                            if arg.required_mut {
                                locals[place.local.index()] = None;
                            }
                            place.local
                        }
                    };
                    for root in &cx.local_roots[local.index()] {
                        locals[root.index()] = None;
                    }
                }
            }
            SStmtKind::Assign {
                dst: *dst,
                expr: canonical,
            }
        }
        SStmtKind::Store { dst, src } => {
            locals[dst.local.index()] = None;
            for root in &cx.local_roots[dst.local.index()] {
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

/// A flow- and field-insensitive upper bound on writable local storage reachable through
/// each value. This runs before normalized borrow facts are available. Calls may
/// return or exchange any reachable capability, and stores may install a new
/// capability in any reachable destination. Solve those edges together: a DFS
/// cache can publish incomplete roots when loops or stored handles form cycles.
fn collect_local_roots(body: &SemanticBody<'_>) -> LocalRoots {
    let mut roots = vec![FxHashSet::default(); body.locals.len()];
    loop {
        let mut changed = false;
        for statement in body.blocks.iter().flat_map(|block| &block.stmts) {
            let (dst, sources, address, writes) = match &statement.kind {
                SStmtKind::Assign { dst, expr } => {
                    let mut address = None;
                    let mut writes = Vec::new();
                    let sources = match expr {
                        SExpr::Borrow { place, kind, .. } => {
                            address = (*kind == BorrowKind::Mut).then_some(place.local);
                            vec![place.local]
                        }
                        SExpr::Forward(value)
                        | SExpr::UseValue(value)
                        | SExpr::Cast { value, .. }
                        | SExpr::ArrayRepeat { value, .. }
                        | SExpr::ExtractEnumField { value, .. } => vec![value.value],
                        SExpr::ReadPlace { place } => vec![place.local],
                        SExpr::Field { base, .. } | SExpr::Index { base, .. } => vec![base.value],
                        SExpr::AggregateMake { fields, .. } | SExpr::EnumMake { fields, .. } => {
                            fields.iter().map(|field| field.value).collect()
                        }
                        SExpr::Call {
                            args, effect_args, ..
                        } => {
                            let mut sources = args.iter().map(|arg| arg.value).collect::<Vec<_>>();
                            for arg in effect_args {
                                sources.push(match &arg.arg {
                                    SEffectArgValue::Value(value) => value.value,
                                    SEffectArgValue::Place(place) => {
                                        if arg.required_mut {
                                            writes.push(place.local);
                                        }
                                        place.local
                                    }
                                });
                            }
                            for source in &sources {
                                writes.extend(roots[source.index()].iter().copied());
                            }
                            sources
                        }
                        SExpr::CodeRegionRef { .. }
                        | SExpr::Const(_)
                        | SExpr::Unary { .. }
                        | SExpr::Binary { .. }
                        | SExpr::GetEnumTag { .. }
                        | SExpr::IsEnumVariant { .. }
                        | SExpr::CodeRegionOffset { .. }
                        | SExpr::CodeRegionLen { .. } => Vec::new(),
                    };
                    (*dst, sources, address, writes)
                }
                SStmtKind::Store { dst, src } => (
                    dst.local,
                    vec![src.value],
                    None,
                    roots[dst.local.index()].iter().copied().collect(),
                ),
            };
            let mut reachable = sources
                .iter()
                .flat_map(|source| roots[source.index()].iter().copied())
                .collect::<FxHashSet<_>>();
            reachable.extend(address);
            // Effect places expose their own storage as well as capabilities
            // already held there, including when a call returns that address.
            if let SStmtKind::Assign {
                expr: SExpr::Call { effect_args, .. },
                ..
            } = &statement.kind
            {
                reachable.extend(effect_args.iter().filter_map(|arg| match &arg.arg {
                    SEffectArgValue::Place(place) if arg.required_mut => Some(place.local),
                    SEffectArgValue::Place(_) | SEffectArgValue::Value(_) => None,
                }));
            }
            for target in writes.into_iter().chain([dst]) {
                let target = &mut roots[target.index()];
                let before = target.len();
                target.extend(reachable.iter().copied());
                changed |= target.len() != before;
            }
        }
        if !changed {
            return roots;
        }
    }
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
