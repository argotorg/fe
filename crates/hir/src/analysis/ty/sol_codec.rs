//! The compiler's implementation of `core::ops::LaneCodec<T>` for std's
//! Solidity codec, `std::evm::SolLanes`, for every `T` without a `SolPacked`
//! encoding. std codes a `SolPacked` element itself; any other element takes
//! whole slots, laid out as the layout pass lays out a field of its type, so
//! the codec's `BITS` is 256 times their number, a layout fact the library
//! cannot compute. A codec that wide packs nothing, so the compiler never
//! calls its `get` or `set`.

use common::indexmap::IndexMap;
use num_bigint::BigInt;

use super::{
    const_ty::{ConstTyId, const_ty_from_sem_const},
    corelib::{resolve_core_trait, resolve_lib_trait_path, resolve_lib_type_path},
    trait_def::{ImplementorId, ImplementorOrigin, ResolvedImplInstance, TraitInstId},
    trait_resolution::{GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable},
    ty_def::{TyFlags, TyId},
    visitor::collect_flags,
};
use crate::{
    analysis::{HirAnalysisDb, semantic::int_const},
    core::semantic::storage_slot_span,
    hir_def::scope_graph::ScopeId,
};

/// A generic whole-slot fallback is provisional layout information, not a
/// proof that survives substitution: its element can later be `SolPacked`.
/// Keep that obligation deferred until instance finalization. A declared
/// bound or an actual implementation proof is independent of this fallback.
pub(crate) fn sol_codec_proof_is_deferred<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
) -> bool {
    sol_codec_elem(db, goal).is_some_and(|(_, elem)| {
        collect_flags(db, elem).intersects(TyFlags::HAS_PARAM | TyFlags::HAS_PROJECTION)
    })
}

/// The implementation proving `goal` when it is `SolLanes: LaneCodec<T>`
/// for a `T` that is not `SolPacked` under `assumptions`. A `T` still to be
/// inferred, or one whose encoding is undecided, has none.
pub(crate) fn sol_codec_implementor<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<ImplementorId<'db>> {
    let (std_scope, elem) = sol_codec_elem(db, goal)?;
    if elem.has_var(db) || elem.has_invalid(db) {
        return None;
    }
    let sol_packed = resolve_lib_trait_path(db, std_scope, "std::evm::SolPacked")?;
    let packed = TraitInstId::new_simple(db, sol_packed, vec![elem]);
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    matches!(
        is_goal_satisfiable(db, solve_cx, packed),
        GoalSatisfiability::UnSat(_)
    )
    .then(|| {
        ImplementorId::new(
            db,
            goal,
            Vec::new(),
            IndexMap::new(),
            ImplementorOrigin::Structural,
        )
    })
}

/// `BITS` of the compiler's `SolLanes: LaneCodec<T>` that `resolved` selects:
/// 256 times the slots `T` takes. `None` for any other implementation, and
/// while `T` is generic.
pub(crate) fn sol_codec_bits<'db>(
    db: &'db dyn HirAnalysisDb,
    resolved: ResolvedImplInstance<'db>,
) -> Option<ConstTyId<'db>> {
    if !matches!(
        resolved.selected().origin(db),
        ImplementorOrigin::Structural
    ) {
        return None;
    }
    let (_, elem) = sol_codec_elem(db, resolved.trait_inst())?;
    if elem.has_param(db) || elem.has_var(db) {
        return None;
    }
    let slots = storage_slot_span(db, elem).ok()?;
    let u256 = TyId::u256(db);
    Some(const_ty_from_sem_const(
        db,
        int_const(db, u256, BigInt::from(slots) * 256),
    ))
}

/// std's scope and `T` when `inst` is `SolLanes: LaneCodec<T>`.
fn sol_codec_elem<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
) -> Option<(ScopeId<'db>, TyId<'db>)> {
    let self_ty = inst.self_ty(db);
    let adt = self_ty.adt_def(db)?;
    if adt.name(db)?.data(db) != "SolLanes" {
        return None;
    }
    let scope = adt.scope(db);
    let lane_codec = resolve_core_trait(db, scope, &["ops", "LaneCodec"])?;
    if inst.def(db) != lane_codec
        || resolve_lib_type_path(db, scope, "std::evm::SolLanes") != Some(self_ty)
    {
        return None;
    }
    Some((scope, *inst.args(db).get(1)?))
}
