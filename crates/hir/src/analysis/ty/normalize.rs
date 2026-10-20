//! Type normalization module
//!
//! This module provides functionality to normalize types by resolving associated types
//! to concrete types when possible. This happens before type unification to ensure
//! that types are in their most resolved form.

use crate::core::hir_def::{IdentId, ImplTrait, scope_graph::ScopeId};
use crate::core::semantic::param_env;
use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHashMap;
use salsa::SalsaValue;

use super::{
    canonical::Canonical,
    canonical::Canonicalized,
    closure::callable_out_ty,
    fold::{TyFoldable, TyFolder},
    shape::Shape,
    trait_def::{
        AssocBinding, ImplementorId, ImplementorOrigin, TraitInstId, TraitRefId,
        impl_where_clause_refuted, impls_for_trait_inst,
    },
    trait_lower::complete_impl_assoc_ty,
    trait_resolution::{PredicateListId, TraitSolveCx},
    ty_def::{
        AssocTy, InvalidCause, ShapeCon, ShapeMode, TyBase, TyData, TyId, TyParam,
        collect_variables,
    },
    ty_error::first_invalid_ty_cause,
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
    ty.fold_with(db, &mut TypeNormalizer::new(db, scope, assumptions, true))
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
    value.fold_with(db, &mut TypeNormalizer::new(db, scope, assumptions, false))
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

/// How many projections may be resolved inside each other, and how deeply the
/// generic arguments of one instance may nest.
pub(crate) const PROJECTION_DEPTH_LIMIT: usize = 64;

/// How many type nodes, counted as a tree, the projections resolved for one
/// projection may have in total, each counted once and itself included, as
/// may the generic arguments of one instance.
pub(crate) const PROJECTION_WORK_LIMIT: usize = 65536;

/// Folds a value, normalizing each projection in it with
/// [`normalize_projection`].
struct TypeNormalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    /// Whether impls, and not only the assumptions, resolve projections.
    resolve_impls: bool,
    /// Whether a projection resolved through an impl whose definition
    /// diverges is marked so (see [`impl_assoc_ty_diverges`]).
    mark_divergent: bool,
    /// How many projections may still be resolved inside each other.
    budget: usize,
    /// While resolving a projection, the projections resolved for it so far.
    /// A value's projections are normalized each on its own, so the work of
    /// one doesn't count against another.
    resolved: Option<Resolved<'db>>,
}

/// The projections resolved for one projection, itself included, each with
/// its tree size, and their total, the work, which may not exceed
/// [`PROJECTION_WORK_LIMIT`].
struct Resolved<'db> {
    sizes: IndexMap<TyId<'db>, usize>,
    work: usize,
}

impl<'db> Resolved<'db> {
    fn add(&mut self, projection: TyId<'db>, size: usize) {
        if self.sizes.insert(projection, size).is_none() {
            self.work = self.work.saturating_add(size);
        }
    }

    fn exceeded(&self) -> bool {
        self.work > PROJECTION_WORK_LIMIT
    }
}

impl<'db> TypeNormalizer<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        resolve_impls: bool,
    ) -> Self {
        Self {
            db,
            scope,
            assumptions,
            resolve_impls,
            mark_divergent: true,
            budget: PROJECTION_DEPTH_LIMIT,
            resolved: None,
        }
    }
}

/// What normalizing a projection gives.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
enum NormalizedProjection<'db> {
    /// The normalized type, and the projections resolved for it.
    Ty {
        ty: TyId<'db>,
        resolved: ResolvedProjections<'db>,
    },
    /// A limit was reached, here or in a projection resolved for this one.
    Limit,
}

/// The projections resolved for a projection, itself included, each with its
/// tree size.
#[salsa::interned]
#[derive(Debug)]
struct ResolvedProjections<'db> {
    sizes: Vec<(TyId<'db>, usize)>,
}

/// A projection to normalize, with what normalizing it depends on.
#[salsa::interned]
#[derive(Debug)]
struct ProjectionQuery<'db> {
    #[returns(copy)]
    ty: TyId<'db>,
    #[returns(copy)]
    scope: ScopeId<'db>,
    #[returns(copy)]
    assumptions: PredicateListId<'db>,
    #[returns(copy)]
    resolve_impls: bool,
    #[returns(copy)]
    mark_divergent: bool,
    /// How many projections may be resolved inside each other, this one
    /// included.
    #[returns(copy)]
    budget: usize,
}

/// The normalized form of a projection.
///
/// A projection that an impl defines through itself, directly or through
/// other impls, repeats until it reaches the nesting limit. One whose impl
/// defines it through a larger one, such as `type Out = <W<(T, T)> as
/// Tr>::Out`, never repeats, and its types can double at each step, staying
/// small as interned values while growing exponentially as trees, which is
/// how resolution walks them; one that resolves into several such projections
/// multiplies them at each step. So the nesting and the total size of the
/// projections resolved for a projection are limited, and the first limit
/// reached stops its resolution. The limits depend only on the query, so a
/// projection normalizes the same way wherever and in whichever order it is
/// met.
#[salsa::tracked(returns(copy), cycle_initial = normalize_projection_cycle_initial)]
fn normalize_projection<'db>(
    db: &'db dyn HirAnalysisDb,
    query: ProjectionQuery<'db>,
) -> NormalizedProjection<'db> {
    let (ty, budget) = (query.ty(db), query.budget(db));
    let TyData::AssocTy(assoc) = ty.data(db) else {
        unreachable!("only projections are normalized one at a time")
    };
    let Some(size) = tree_size_within(db, ty, PROJECTION_WORK_LIMIT).filter(|_| budget > 0) else {
        return NormalizedProjection::Limit;
    };
    let mut normalizer = TypeNormalizer {
        db,
        scope: query.scope(db),
        assumptions: query.assumptions(db),
        resolve_impls: query.resolve_impls(db),
        mark_divergent: query.mark_divergent(db),
        budget: budget - 1,
        resolved: Some(Resolved {
            sizes: IndexMap::from_iter([(ty, size)]),
            work: size,
        }),
    };
    // An associated shape resolves to its impl's value at the mode it is
    // named at; until it has one, it stays.
    let resolved = match assoc.mode {
        None if assoc.is_shape(db) => None,
        mode => normalizer
            .try_resolve_assoc_ty(ty, assoc)
            .map(|replacement| {
                mode.map_or(replacement, |mode| replacement.at_shape_mode(db, mode))
            }),
    };
    let normalized = match resolved {
        // An impl that defines the projection as itself.
        Some(replacement) if replacement == ty => return NormalizedProjection::Limit,
        Some(replacement) => normalizer.fold_ty(db, replacement),
        // Not resolved; still fold internals (e.g., normalize self type)
        None => ty.super_fold_with(db, &mut normalizer),
    };
    let resolved = normalizer
        .resolved
        .expect("resolving a projection counts its work");
    if resolved.exceeded() {
        NormalizedProjection::Limit
    } else {
        NormalizedProjection::Ty {
            ty: normalized,
            resolved: ResolvedProjections::new(db, resolved.sizes.into_iter().collect::<Vec<_>>()),
        }
    }
}

/// Resolving a projection can lower an impl's associated type, which can ask
/// for the projection again through trait solving; within that cycle it is
/// unresolved.
fn normalize_projection_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    query: ProjectionQuery<'db>,
) -> NormalizedProjection<'db> {
    NormalizedProjection::Ty {
        ty: query.ty(db),
        resolved: ResolvedProjections::new(db, Vec::new()),
    }
}

/// Whether the definition of `name` in `impl_` can't be normalized within the
/// limits in the impl's own context, as `type Out = <S as Tr>::Out` or `type
/// Out = <W<(T, T)> as Tr>::Out` can't, or resolves through such a
/// definition. That is an error at the definition; a projection resolved
/// through it is an invalid type that isn't reported again.
#[salsa::tracked(returns(copy), cycle_initial = impl_assoc_ty_diverges_cycle_initial)]
pub(crate) fn impl_assoc_ty_diverges<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: ImplTrait<'db>,
    name: IdentId<'db>,
) -> bool {
    let Some(ty) = impl_
        .assoc_types(db)
        .find(|view| view.name(db) == Some(name))
        .and_then(|view| view.ty(db))
    else {
        return false;
    };
    // Normalized without marking divergent definitions, so that whether one
    // diverges doesn't depend on whether another does.
    let mut normalizer = TypeNormalizer::new(db, impl_.scope(), param_env(db, impl_.into()), true);
    normalizer.mark_divergent = false;
    let normalized = ty.fold_with(db, &mut normalizer);
    first_invalid_ty_cause(db, normalized) == Some(InvalidCause::TypeNormalizationLimit)
}

fn impl_assoc_ty_diverges_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _impl_: ImplTrait<'db>,
    _name: IdentId<'db>,
) -> bool {
    false
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
            // A limit reached while resolving a projection is the limit for
            // all of it, so the rest is not resolved.
            TyData::AssocTy(_) if self.resolved.as_ref().is_some_and(Resolved::exceeded) => {
                TyId::invalid(db, InvalidCause::TypeNormalizationLimit)
            }
            TyData::AssocTy(_) => {
                let query = ProjectionQuery::new(
                    db,
                    ty,
                    self.scope,
                    self.assumptions,
                    self.resolve_impls,
                    self.mark_divergent,
                    self.budget,
                );
                match normalize_projection(db, query) {
                    NormalizedProjection::Ty { ty, resolved } => {
                        let Some(own) = &mut self.resolved else {
                            return ty;
                        };
                        for &(projection, size) in resolved.sizes(db) {
                            own.add(projection, size);
                        }
                        if own.exceeded() {
                            TyId::invalid(db, InvalidCause::TypeNormalizationLimit)
                        } else {
                            ty
                        }
                    }
                    NormalizedProjection::Limit => {
                        if let Some(own) = &mut self.resolved {
                            own.work = usize::MAX;
                        }
                        TyId::invalid(db, InvalidCause::TypeNormalizationLimit)
                    }
                }
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

        // The definitions the candidates give, each with the impl giving it.
        let mut definitions: IndexMap<TyId<'db>, ImplementorId<'db>> = IndexMap::default();
        // Canonicalize the target trait instance so we can unify against it in a
        // fresh table without mixing inference keys from other tables.
        let canonical_target = Canonicalized::new(self.db, trait_inst);
        canonical_target.with_materialized(self.db, |cx| {
            let target_inst = cx.query();
            let original_target = cx.try_extract::<TraitInstId<'db>>(target_inst);
            for implementor in candidates {
                let Some(completed) = complete_impl_assoc_ty(self.db, implementor, assoc.name)
                else {
                    continue;
                };
                // Extracted into the caller's inference environment, so
                // scratch-local vars never leak into the cache.
                let candidate = cx.with_impl_assoc_ty(
                    completed,
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
                if let Some(Some(definition)) = candidate {
                    definitions.entry(definition).or_insert(implementor);
                }
            }
        });

        let (&definition, implementor) = match definitions.len() {
            1 => definitions.first()?,
            _ => return None,
        };
        Some(match implementor.origin(self.db) {
            ImplementorOrigin::Hir(impl_)
                if self.mark_divergent && impl_assoc_ty_diverges(self.db, impl_, assoc.name) =>
            {
                TyId::invalid(self.db, InvalidCause::DivergentAssocTy)
            }
            _ => definition,
        })
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
