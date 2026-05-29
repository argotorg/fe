use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SBlock, SBlockId, SConst, SExpr, SStmt, SStmtKind, STerminatorKind, SemConstId,
        SemConstValue, SemanticBody, SemanticCalleeRef, array_const, enum_const,
        instance::SemanticInstance, instantiate_with_generic_args, reify_runtime_const_for_ty,
        sem_const_from_ty, struct_const, tuple_const,
    },
    ty::{
        const_ty::{ConstTyData, evaluate_type_level_int_const_expr},
        ty_def::{BorrowKind, CapabilityKind, TyData, TyId},
    },
};

use super::{eval_const_ref, machine::try_eval_expr_to_const};

type LocalConstMap<'db> = Vec<Option<SemConstId<'db>>>;
type LocalDefs<'db> = Vec<Vec<SExpr<'db>>>;

#[derive(Clone, Copy)]
enum ConstCanonicalizationMode {
    Final,
    Provisional,
}

#[salsa::tracked]
pub fn canonicalize_semantic_consts<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBody<'db> {
    let original = instance.body(db).clone();
    canonicalize_semantic_consts_from_body(db, instance, original)
}

pub(crate) fn canonicalize_semantic_consts_from_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    original: SemanticBody<'db>,
) -> SemanticBody<'db> {
    canonicalize_semantic_consts_from_body_with_mode(
        db,
        instance,
        original,
        ConstCanonicalizationMode::Final,
    )
}

pub(crate) fn canonicalize_provisional_semantic_consts_from_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    original: SemanticBody<'db>,
) -> SemanticBody<'db> {
    canonicalize_semantic_consts_from_body_with_mode(
        db,
        instance,
        original,
        ConstCanonicalizationMode::Provisional,
    )
}

fn canonicalize_semantic_consts_from_body_with_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    original: SemanticBody<'db>,
    mode: ConstCanonicalizationMode,
) -> SemanticBody<'db> {
    let mut body = original.clone();
    if body.blocks.is_empty() {
        return body;
    }
    let local_defs = collect_local_defs(&original);

    let mut incoming = vec![None; body.blocks.len()];
    incoming[0] = Some(vec![None; body.locals.len()]);
    let mut pending = VecDeque::from([SBlockId::from_u32(0)]);

    while let Some(bb) = pending.pop_front() {
        let Some(mut locals) = incoming[bb.index()].clone() else {
            continue;
        };
        body.blocks[bb.index()] = canonicalize_block(
            db,
            instance,
            &original.blocks[bb.index()],
            &mut locals,
            &original,
            &local_defs,
            mode,
        );
        for succ in block_successors(&original.blocks[bb.index()].terminator.kind) {
            if merge_local_consts(&mut incoming[succ.index()], &locals) {
                pending.push_back(succ);
            }
        }
    }

    let mut unknown_locals = vec![None; body.locals.len()];
    for (idx, state) in incoming.iter().enumerate() {
        if state.is_none() {
            body.blocks[idx] = canonicalize_block(
                db,
                instance,
                &original.blocks[idx],
                &mut unknown_locals,
                &original,
                &local_defs,
                mode,
            );
            unknown_locals.fill(None);
        }
    }

    body
}

fn canonicalize_block<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    block: &SBlock<'db>,
    locals: &mut LocalConstMap<'db>,
    body: &SemanticBody<'db>,
    local_defs: &LocalDefs<'db>,
    mode: ConstCanonicalizationMode,
) -> SBlock<'db> {
    SBlock {
        stmts: block
            .stmts
            .iter()
            .map(|stmt| canonicalize_stmt(db, instance, stmt, locals, body, local_defs, mode))
            .collect(),
        terminator: block.terminator.clone(),
    }
}

fn canonicalize_stmt<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    stmt: &SStmt<'db>,
    locals: &mut LocalConstMap<'db>,
    body: &SemanticBody<'db>,
    local_defs: &LocalDefs<'db>,
    mode: ConstCanonicalizationMode,
) -> SStmt<'db> {
    let kind = match &stmt.kind {
        SStmtKind::Assign { dst, expr } => {
            let (expr, value) = canonicalize_expr(
                db,
                instance,
                expr,
                body.locals[dst.index()].ty,
                locals,
                mode,
            );
            locals[dst.index()] = value;
            invalidate_mutated_call_locals(db, &expr, locals, body, local_defs);
            SStmtKind::Assign { dst: *dst, expr }
        }
        SStmtKind::Store { dst, src } => {
            locals[dst.local.index()] = None;
            SStmtKind::Store {
                dst: dst.clone(),
                src: *src,
            }
        }
    };
    SStmt {
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
            | SExpr::DynField { .. }
            | SExpr::Call { .. } => {}
        }
    }

    visiting.remove(&local);
    let roots = roots.into_iter().collect::<Vec<_>>();
    memo[local.index()] = Some(roots.clone());
    roots
}

fn canonicalize_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    expr: &SExpr<'db>,
    result_ty: TyId<'db>,
    locals: &LocalConstMap<'db>,
    mode: ConstCanonicalizationMode,
) -> (SExpr<'db>, Option<SemConstId<'db>>) {
    if let SExpr::Const(SConst::Ref(cref)) = expr {
        let value = eval_const_ref(db, *cref)
            .unwrap_or_else(|err| panic!("CTFE failed for {cref:?}: {err:?}"));
        let value = canonicalize_const_value(db, instance, value);
        let runtime = reify_runtime_const_for_ty(db, instance, result_ty, value);
        return (
            SExpr::Const(runtime.map_or(SConst::Value(value), |_| SConst::Ref(*cref))),
            runtime,
        );
    }

    if matches!(mode, ConstCanonicalizationMode::Final)
        && let Some(value) =
            try_eval_expr_to_const(db, instance, result_ty, expr, locals, synthetic())
        && !matches!(value.value(db), SemConstValue::TypeLevel { .. })
    {
        let value = canonicalize_const_value(db, instance, value);
        if let Some(value) = reify_runtime_const_for_ty(db, instance, result_ty, value) {
            return (SExpr::Const(SConst::Value(value)), Some(value));
        }
    }

    match expr {
        SExpr::Const(SConst::Value(value)) => {
            let value = canonicalize_const_value(db, instance, *value);
            let runtime = reify_runtime_const_for_ty(db, instance, result_ty, value);
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
            let instantiated = instantiate_with_generic_args(
                db,
                const_ty,
                instance.key(db).subst(db).generic_args(db),
            );
            let TyData::ConstTy(const_ty) = instantiated.data(db) else {
                return value;
            };
            let mut evaluated = const_ty.evaluate(db, Some(ty));
            if let ConstTyData::Abstract(expr, expected_ty) = evaluated.data(db)
                && let Some(concrete) = evaluate_type_level_int_const_expr(db, *expr, *expected_ty)
            {
                evaluated = concrete;
            }
            if matches!(evaluated.data(db), ConstTyData::Abstract(..)) {
                let instantiated = instantiate_with_generic_args(
                    db,
                    TyId::const_ty(db, evaluated),
                    instance.key(db).subst(db).generic_args(db),
                );
                let TyData::ConstTy(instantiated) = instantiated.data(db) else {
                    unreachable!("instantiating a const ty must yield a const ty");
                };
                evaluated = instantiated.evaluate(db, Some(ty));
                if let ConstTyData::Abstract(expr, expected_ty) = evaluated.data(db)
                    && let Some(concrete) =
                        evaluate_type_level_int_const_expr(db, *expr, *expected_ty)
                {
                    evaluated = concrete;
                }
            }
            sem_const_from_ty(db, TyId::const_ty(db, evaluated)).unwrap_or(value)
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
