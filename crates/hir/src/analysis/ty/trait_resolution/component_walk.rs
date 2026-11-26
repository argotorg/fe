//! The component walk: the one place that reads implementations while bounds
//! are derived.
//!
//! The plain walk (`PredicateListId::extend_all_bounds`) builds assumptions.
//! Headers, signatures, method selection and path resolution read those, and
//! the set of implementations is itself found by reading headers, so the plain
//! walk is a function of the predicates and the trait declarations alone: it
//! reads no implementation. The walk that keeps concrete bounds (the
//! components of a body, `concrete_components`) and the readers of component
//! paths run when a body is checked or an instance is built, after every
//! header is known, and may normalize through ordinary implementations. The
//! walk loop is shared; what lets it read an implementation is a `Components`
//! value, and only this module can make one.

use super::{
    BoundStep, BoundWalk, PredicateListId, TraitSolveCx, bound_walk_cycle_initial,
    is_concrete_bound, normalize_trait_inst_preserving_validity, step_bound,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{trait_def::TraitInstId, ty_def::TyFlags, visitor::collect_flags},
};

/// The permission to derive a bound through ordinary implementations. No value
/// can be made outside this module, so the plain walk cannot be given one.
#[derive(Debug, Clone, Copy)]
pub(super) struct Components(());

impl Components {
    /// The bound `step` derives from `parent`. When `parent` is a member of the
    /// walk, a bound the walk added below an equality (not one of the input
    /// slots), and is concrete, a projection left in the child is fixed by the
    /// implementation that defines the associated type: `<U<Foo> as Tr>::A:
    /// Show` below `U<Foo>: Tr` is `Foo: Show`. A concrete slot is stepped as
    /// the plain walk steps it, so that the two walks agree on the slots.
    pub(super) fn child<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        parent: TraitInstId<'db>,
        parent_is_member: bool,
        step: BoundStep<'db>,
    ) -> Option<TraitInstId<'db>> {
        let child = step_bound(db, parent, step)?;
        Some(if parent_is_member {
            normalize_below_concrete(db, parent, child)
        } else {
            child
        })
    }
}

/// A bound derived from a concrete member `pred` has no equality of its own to
/// fix a projection in it. The implementation that defines the associated type
/// fixes it, as ordinary normalization reads it; a choice never changes this
/// normalization.
fn normalize_below_concrete<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: TraitInstId<'db>,
    bound: TraitInstId<'db>,
) -> TraitInstId<'db> {
    if !is_concrete_bound(db, pred) || !collect_flags(db, bound).contains(TyFlags::HAS_PROJECTION) {
        return bound;
    }
    let scope = TraitSolveCx::normalization_scope_for_trait_inst_with_origin(
        db,
        pred.def(db).scope().top_mod(db).ingot(db),
        bound,
    );
    normalize_trait_inst_preserving_validity(db, bound, scope, PredicateListId::empty_list(db))
}

/// The goal at the end of a component path that starts at the slot goal
/// `root`: the first step is taken from a slot, each later step from a member.
/// Every reader of a component path derives its goals here, so that they agree
/// with the walk. Header lowering never calls it.
pub(crate) fn component_path_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    root: TraitInstId<'db>,
    steps: &[BoundStep<'db>],
) -> Option<TraitInstId<'db>> {
    steps
        .iter()
        .enumerate()
        .try_fold(root, |goal, (position, &step)| {
            Components(()).child(db, goal, position > 0, step)
        })
}

#[salsa::tracked(returns(ref), cycle_initial = bound_walk_cycle_initial)]
pub(super) fn bound_walk_keeping_concrete_query<'db>(
    db: &'db dyn HirAnalysisDb,
    predicates: PredicateListId<'db>,
) -> BoundWalk<'db> {
    predicates.walk_bounds(db, Some(Components(())))
}
