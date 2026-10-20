//! Type normalization module
//!
//! This module provides functionality to normalize types by resolving associated types
//! to concrete types when possible. This happens before type unification to ensure
//! that types are in their most resolved form.

use std::collections::hash_map::Entry;

use crate::core::hir_def::{ImplTrait, scope_graph::ScopeId};
use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashMap;

use super::{
    canonical::Canonical,
    canonical::Canonicalized,
    closure::callable_out_ty,
    fold::{TyFoldable, TyFolder},
    shape::Shape,
    trait_def::{
        AssocBinding, ImplementorId, TraitInstId, TraitRefId, impl_where_clause_refuted,
        impls_for_trait_inst,
    },
    trait_lower::complete_impl_assoc_ty,
    trait_resolution::{PredicateListId, TraitSolveCx},
    ty_def::{
        AssocTy, InvalidCause, ShapeCon, ShapeMode, TyBase, TyData, TyId, TyParam,
        collect_variables,
    },
    unify::UnificationTable,
    visitor::{TyVisitor, walk_ty},
};
use crate::analysis::HirAnalysisDb;

/// Normalizes a type by resolving all associated types to concrete types when possible.
///
/// This function takes a type and attempts to resolve any associated types within it
/// using the provided assumptions and scope context. It handles:
/// - Simple associated types (e.g., `T::Output`)
/// - Nested associated types (e.g., `T::Encoder::Output`)
/// - Associated types with generic parameters
pub fn normalize_ty<'db, T>(
    db: &'db dyn HirAnalysisDb,
    ty: T,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    ty.fold_with(db, &mut normalizer)
}

/// Apply declared associated equalities without implementation selection.
/// Structural slot planning uses declaration coordinates before the slots it
/// is discovering exist, so implementation lookup would create a query cycle.
pub fn normalize_from_assumptions<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    normalizer.resolve_impls = false;
    value.fold_with(db, &mut normalizer)
}

/// Apply the associated equalities carried by a single trait predicate, as
/// [`normalize_from_assumptions`] does.
pub fn normalize_with_trait_evidence<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    scope: ScopeId<'db>,
    evidence: TraitInstId<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    normalize_from_assumptions(db, value, scope, PredicateListId::new(db, vec![evidence]))
}

/// How many projections may be in the middle of being resolved at once, and
/// how deeply the generic arguments of one instance may nest.
pub(crate) const PROJECTION_DEPTH_LIMIT: usize = 64;

/// How many type nodes, counted as a tree, the projections resolved by one
/// normalization may have in total, as may the generic arguments of one
/// instance.
pub(crate) const PROJECTION_WORK_LIMIT: usize = 65536;

pub struct TypeNormalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_impls: bool,
    // Projection cache: None = in progress (cycle guard), Some(ty) = normalized result
    cache: FxHashMap<AssocTy<'db>, Option<TyId<'db>>>,
    /// Projections currently being resolved, counted against
    /// [`PROJECTION_DEPTH_LIMIT`].
    projection_depth: usize,
    /// Size of the projections resolved so far, counted against
    /// [`PROJECTION_WORK_LIMIT`].
    projection_work: usize,
}

impl<'db> TypeNormalizer<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        Self {
            db,
            scope,
            assumptions,
            resolve_impls: true,
            cache: FxHashMap::default(),
            projection_depth: 0,
            projection_work: 0,
        }
    }

    /// Starts resolving the projection `ty`, or returns an invalid type if
    /// that would exceed the normalization limits.
    ///
    /// The cycle guard stops a projection that comes back to itself, but not
    /// one whose impl defines it through a larger projection, such as
    /// `type Out = <W<(T, T)> as Tr>::Out`: that chain never repeats. Its
    /// types can also double at each step, staying small as interned values
    /// while growing exponentially as trees, which is how resolution walks
    /// them. So both the nesting and the total size are limited.
    fn enter_projection(&mut self, ty: TyId<'db>) -> Result<(), TyId<'db>> {
        let remaining = PROJECTION_WORK_LIMIT - self.projection_work;
        match tree_size_within(self.db, ty, remaining) {
            Some(size) if self.projection_depth < PROJECTION_DEPTH_LIMIT => {
                self.projection_work += size;
                self.projection_depth += 1;
                Ok(())
            }
            _ => Err(TyId::invalid(self.db, InvalidCause::TypeNormalizationLimit)),
        }
    }
}

/// The types `ty` is made of, one level down.
fn children<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Vec<TyId<'db>> {
    struct Children<'db> {
        db: &'db dyn HirAnalysisDb,
        children: Vec<TyId<'db>>,
    }
    impl<'db> TyVisitor<'db> for Children<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_ty(&mut self, ty: TyId<'db>) {
            self.children.push(ty);
        }
        // An application is one type with its constructor: `(A, B)` is made
        // of `A` and `B`, not of `(A,)` applied to `B`.
        fn visit_app(&mut self, abs: TyId<'db>, arg: TyId<'db>) {
            walk_ty(self, abs);
            self.children.push(arg);
        }
    }
    let mut children = Children {
        db,
        children: Vec::new(),
    };
    walk_ty(&mut children, ty);
    children.children
}

/// The number of nodes in `ty` counted as a tree, if it is at most `limit`.
pub(crate) fn tree_size_within<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    limit: usize,
) -> Option<usize> {
    fn size<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        limit: usize,
        memo: &mut FxHashMap<TyId<'db>, usize>,
    ) -> Option<usize> {
        if let Some(&known) = memo.get(&ty) {
            return Some(known);
        }
        let mut total = 1usize;
        for child in children(db, ty) {
            total = total.checked_add(size(db, child, limit, memo)?)?;
            if total > limit {
                return None;
            }
        }
        memo.insert(ty, total);
        Some(total)
    }
    if limit == 0 {
        return None;
    }
    size(db, ty, limit, &mut FxHashMap::default())
}

/// Whether `ty`, as a tree, nests at most `limit` levels of types.
pub(crate) fn tree_depth_within<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    limit: usize,
) -> bool {
    fn depth<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        limit: usize,
        memo: &mut FxHashMap<TyId<'db>, usize>,
    ) -> Option<usize> {
        if let Some(&known) = memo.get(&ty) {
            return (known <= limit).then_some(known);
        }
        let below = limit.checked_sub(1)?;
        let mut deepest = 0;
        for child in children(db, ty) {
            deepest = deepest.max(depth(db, child, below, memo)?);
        }
        memo.insert(ty, deepest + 1);
        Some(deepest + 1)
    }
    depth(db, ty, limit, &mut FxHashMap::default()).is_some()
}

impl<'db> TyFolder<'db> for TypeNormalizer<'db> {
    fn fold_ty_app(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        abs: TyId<'db>,
        arg: TyId<'db>,
    ) -> TyId<'db> {
        if self.resolve_impls {
            TyId::app(db, abs, arg)
        } else {
            TyId::app_structural(db, abs, arg)
        }
    }

    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyParam(p @ TyParam { owner, .. }) if p.is_trait_self() => {
                if let Some(impl_) = owner.resolve_to::<ImplTrait>(self.db) {
                    // Use the item method to obtain the implementor's self type.
                    let lowered = impl_.ty(self.db);
                    return self.fold_ty(db, lowered);
                }
                ty
            }
            TyData::AssocTy(assoc_ty) => {
                match self.cache.entry(*assoc_ty) {
                    Entry::Occupied(entry) => match entry.get() {
                        Some(cached) => return *cached,
                        None => return ty, // cycle: leave unresolved
                    },
                    Entry::Vacant(entry) => {
                        entry.insert(None);
                    }
                }

                if let Err(limit) = self.enter_projection(ty) {
                    self.cache.insert(*assoc_ty, Some(limit));
                    return limit;
                }
                // An associated shape resolves to its impl's value at the
                // mode it is named at; until it has one, it stays.
                let resolved = match assoc_ty.mode {
                    None if assoc_ty.is_shape(db) => None,
                    mode => self.try_resolve_assoc_ty(ty, assoc_ty).map(|replacement| {
                        let replacement =
                            mode.map_or(replacement, |mode| replacement.at_shape_mode(db, mode));
                        self.fold_ty(db, replacement)
                    }),
                };
                self.projection_depth -= 1;
                if let Some(normalized) = resolved {
                    self.cache.insert(*assoc_ty, Some(normalized));
                    return normalized;
                }

                // Not resolved; still fold internals (e.g., normalize self type)
                let folded = ty.super_fold_with(db, self);
                self.cache.insert(*assoc_ty, Some(folded));
                folded
            }
            // The value or carrier type of a shape reduces once the shape is
            // known.
            TyData::TyApp(..)
                if let (base, &[shape]) = ty.decompose_ty_app(db)
                    && let TyData::TyBase(TyBase::Shape(
                        con @ (ShapeCon::Value | ShapeCon::Carrier),
                    )) = base.data(db) =>
            {
                // A value's type names its shape without a mode, which every
                // mode erases alike.
                let shape = match shape.data(db) {
                    TyData::AssocTy(assoc) if assoc.mode.is_none() && *con == ShapeCon::Value => {
                        shape.with_shape_mode(db, ShapeMode::Ref)
                    }
                    _ => shape,
                };
                match self.fold_ty(db, shape).as_shape_part(db) {
                    Shape::Opaque(term) => TyId::shape_part(db, *con, term),
                    shape if *con == ShapeCon::Value => self.fold_ty(db, shape.erased_ty(db)),
                    shape => self.fold_ty(db, shape.carrier_ty(db)),
                }
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

impl<'db> TypeNormalizer<'db> {
    fn try_resolve_assoc_ty(&mut self, ty: TyId<'db>, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        // Equality evidence is separate from the projection's identity. Match
        // its entire trait reference before using an assumption's binding.
        let target = assoc.trait_.fold_with(self.db, self);
        let mut matching_bounds: IndexMap<TyId<'db>, ()> = IndexMap::new();
        for &pred in self.assumptions.list(self.db) {
            let binding = AssocBinding {
                name: assoc.name,
                mode: assoc.mode,
            };
            let Some(bound) = pred.bound_assoc_ty(self.db, binding) else {
                continue;
            };
            if self.trait_refs_match(target, pred.trait_ref(self.db)) {
                matching_bounds.insert(self.fold_ty(self.db, bound), ());
            }
        }
        if matching_bounds.len() > 1 {
            return None;
        }
        if let Some((&bound, _)) = matching_bounds.first() {
            return (bound != ty).then_some(bound);
        }

        if !self.resolve_impls {
            return None;
        }

        if let Some(&shape) = assoc.trait_.args(self.db).get(1)
            && let Some(out) = callable_out_ty(
                self.db,
                self.fold_ty(self.db, assoc.trait_.self_ty(self.db)),
                assoc.trait_.def(self.db),
                self.fold_ty(self.db, shape),
                assoc.name,
            )
        {
            return Some(out);
        }

        self.try_resolve_assoc_ty_from_impls(assoc)
    }

    /// A pure normalization query may observe established equality, but may
    /// not choose a binding by assigning an unresolved caller inference var.
    fn trait_refs_match(&mut self, target: TraitRefId<'db>, candidate: TraitRefId<'db>) -> bool {
        if target.def(self.db) != candidate.def(self.db) {
            return false;
        }
        let candidate = candidate.fold_with(self.db, self);
        if target == candidate {
            return true;
        }
        if !collect_variables(self.db, &target).is_empty()
            || !collect_variables(self.db, &candidate).is_empty()
        {
            return false;
        }
        UnificationTable::new(self.db)
            .unify::<TraitRefId<'db>>(target, candidate)
            .is_ok()
    }

    /// Resolves `assoc` through the impls whose header, every trait argument
    /// included, matches its trait reference. Nothing is proved: where clauses
    /// only choose between impls whose headers both match, and the trait
    /// reference itself is checked where the projection is written. So an
    /// impl whose where clause names the projection it defines, as
    /// `impl Tr for S where <S as Tr>::Out: Show`, needs nothing special.
    fn try_resolve_assoc_ty_from_impls(&mut self, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        let trait_inst = assoc.trait_.fold_with(self.db, self).as_predicate(self.db);
        let canonical_inst = Canonical::new(self.db, trait_inst);

        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(self.db, trait_inst);
        let mut candidates: IndexSet<ImplementorId<'db>> = [Some(primary), secondary]
            .into_iter()
            .flatten()
            .flat_map(|ingot| impls_for_trait_inst(self.db, ingot, canonical_inst))
            .collect();
        if candidates.len() > 1 {
            candidates.retain(|&implementor| {
                !impl_where_clause_refuted(self.db, solve_cx, implementor, canonical_inst)
            });
        }

        let mut dedup: IndexSet<TyId<'db>> = IndexSet::default();
        // Canonicalize the target trait instance so we can unify against it in a
        // fresh table without mixing inference keys from other tables.
        let canonical_target = Canonicalized::new(self.db, trait_inst);
        canonical_target.with_materialized(self.db, |cx| {
            let target_inst = cx.query();
            let original_target = cx.try_extract::<TraitInstId<'db>>(target_inst);
            for implementor in candidates {
                let Some(implementor) = complete_impl_assoc_ty(self.db, implementor, assoc.name)
                else {
                    continue;
                };
                let candidate = cx.with_impl_assoc_ty(
                    implementor,
                    target_inst.self_ty(self.db),
                    assoc.name,
                    |cx, inst, assoc_ty| {
                        cx.unify::<TraitInstId<'db>>(inst, target_inst).ok()?;
                        if cx.try_extract::<TraitInstId<'db>>(target_inst) != original_target {
                            return None;
                        }
                        let assoc_ty = cx.resolve::<TyId<'db>>(assoc_ty);
                        cx.try_extract::<TyId<'db>>(assoc_ty)
                    },
                );

                // Extract into the caller's inference environment before
                // continuing normalization, so scratch-local vars never
                // leak into the cache.
                if let Some(Some(folded)) = candidate {
                    dedup.insert(self.fold_ty(self.db, folded));
                }
            }
        });

        match dedup.len() {
            1 => dedup.first().copied(),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use common::indexmap::IndexMap;

    use super::normalize_ty;
    use crate::analysis::ty::{
        trait_def::TraitInstId,
        trait_resolution::PredicateListId,
        ty_def::{ShapeMode, TyId},
    };
    use crate::hir_def::{IdentId, ItemKind, ModeMarker};
    use crate::test_db::HirAnalysisTestDb;

    /// An associated shape normalizes to its impl's value at the mode it is
    /// named at, `ref` and `mut` independently, and a nested family keeps its
    /// structure.
    #[test]
    fn associated_shapes_normalize_per_mode() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("associated_shapes_normalize_per_mode.fe"),
            r#"
trait Driver {
    shape Step
}

struct Identity {}

impl Driver for Identity {
    shape Step = mut? u256
}

struct Enumerate<D> {}

impl<D: Driver> Driver for Enumerate<D> {
    shape Step = (own usize, D::Step)
}

fn probe(_ d: own Enumerate<Enumerate<Identity>>) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let items: Vec<_> = top_mod.children_non_nested(&db).collect();
        let driver = items
            .iter()
            .find_map(|item| match item {
                ItemKind::Trait(trait_) => Some(*trait_),
                _ => None,
            })
            .expect("missing Driver");
        let probe = items
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(*func),
                _ => None,
            })
            .expect("missing probe");
        let driver_ty = probe.params(&db).next().expect("missing parameter").ty(&db);
        let inst = TraitInstId::new(
            &db,
            driver,
            vec![driver_ty],
            IndexMap::new(),
            ModeMarker::None,
        );
        let step = TyId::assoc_ty(&db, inst.trait_ref(&db), IdentId::new(&db, "Step"));
        let at = |mode| {
            normalize_ty(
                &db,
                step.with_shape_mode(&db, mode),
                probe.scope(),
                PredicateListId::empty_list(&db),
            )
            .pretty_print(&db)
            .to_string()
        };
        assert_eq!(at(ShapeMode::Ref), "(own usize, (own usize, ref u256))");
        assert_eq!(at(ShapeMode::Mut), "(own usize, (own usize, mut u256))");
    }
}
