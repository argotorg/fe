//! Type normalization module
//!
//! This module provides functionality to normalize types by resolving associated types
//! to concrete types when possible. This happens before type unification to ensure
//! that types are in their most resolved form.

use std::collections::hash_map::Entry;

use crate::core::hir_def::{ImplTrait, scope_graph::ScopeId};
use crate::span::DynLazySpan;
use common::indexmap::IndexMap;
use rustc_hash::FxHashMap;

use super::{
    binder::Binder,
    canonical::Canonical,
    canonical::Canonicalized,
    diagnostics::{TyDiagCollection, TyLowerDiag},
    fold::{TyFoldable, TyFolder},
    layout_holes::LayoutRootUse,
    trait_def::{
        ImplementorOrigin, TraitInstId, TraitRefId,
        impls_for_trait_and_ty_with_possible_constraints, resolve_trait_impl_instance,
    },
    trait_lower::complete_impl_assoc_ty,
    trait_resolution::{PredicateListId, Selection, TraitSolveCx},
    ty_def::{AssocTy, InvalidCause, TyData, TyId, TyParam, collect_variables},
    unify::UnificationTable,
    visitor::{TyVisitor, walk_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{FindAssociatedTypeError, find_associated_type},
};

/// Normalizes a type by resolving all associated types to concrete types when possible.
///
/// This function takes a type and attempts to resolve any associated types within it
/// using the provided assumptions and scope context. It handles:
/// - Simple associated types (e.g., `T::Output`)
/// - Nested associated types (e.g., `T::Encoder::Output`)
/// - Associated types with generic parameters
///
///
/// Normalizing can reach a limit (see [`NormalizationLimit`]). That is an
/// error of the program, never a type: the caller reports it where the type
/// is used, or passes it on to code that does. The value that stands for a
/// reported limit afterwards can only be made from the [`LimitReported`]
/// that reporting returns.
pub fn normalize_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<TyId<'db>, NormalizationLimit> {
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    let normalized = ty.fold_with(db, &mut normalizer);
    match normalizer.limit {
        Some(limit) => Err(limit),
        None => Ok(normalized),
    }
}

/// A limit that normalizing a type reached. Whether a type reaches a limit
/// depends only on the type, its scope and assumptions, and the limits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum NormalizationLimit {
    /// More than [`PROJECTION_DEPTH_LIMIT`] projections in the middle of
    /// being resolved at once.
    Nesting,
    /// Projections larger in total than [`PROJECTION_WORK_LIMIT`] type
    /// nodes.
    Work,
}

impl NormalizationLimit {
    /// How many nested steps the nesting limit allows.
    pub const NESTING: usize = PROJECTION_DEPTH_LIMIT;
    /// How many type nodes the work limit allows.
    pub const WORK: usize = PROJECTION_WORK_LIMIT;

    /// What resolving the associated types of a type that reached this limit
    /// needs, as a clause: "resolving the associated types here {reason}".
    pub fn reason(self) -> String {
        match self {
            Self::Nesting => format!("needs more than {} nested steps", grouped(Self::NESTING)),
            Self::Work => format!("needs more than {} type nodes of work", grouped(Self::WORK)),
        }
    }

    /// Reports this limit for the type at `span`: the diagnostic, and the
    /// proof that it was reported.
    pub(crate) fn report<'db>(
        self,
        span: DynLazySpan<'db>,
    ) -> (TyDiagCollection<'db>, LimitReported) {
        let diag = TyLowerDiag::TypeNormalizationLimit { span, limit: self }.into();
        (diag, LimitReported(self))
    }
}

/// Why an invalid type may stand for a type that reached a normalization
/// limit, so that no further error is reported for its uses.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LimitStandIn {
    /// The type is written in the source and its lowering reached the limit.
    /// Like every lowering error, the limit is reported where the type is
    /// written (see `diag_from_invalid_cause`).
    Written(WrittenType),
    /// The limit was reported where the type was met.
    Reported(LimitReported),
}

/// Proof that a type is being lowered from where it is written. Only type
/// lowering, and constant evaluation of an expression written in a type,
/// make one; the test `only_lowering_and_const_evaluation_make_a_written_type`
/// holds the list of callers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WrittenType(());

impl WrittenType {
    /// For type lowering and constant evaluation only (see above).
    pub(in crate::analysis::ty) fn new() -> Self {
        Self(())
    }
}

/// `n` with its digits in groups of three: `65,536`.
fn grouped(n: usize) -> String {
    let digits = n.to_string();
    let mut out = String::new();
    for (idx, digit) in digits.chars().enumerate() {
        if idx > 0 && (digits.len() - idx).is_multiple_of(3) {
            out.push(',');
        }
        out.push(digit);
    }
    out
}

/// Normalizes `ty`, keeping it as it is if a limit is reached. For code that
/// works on types already normalized, and any limit in them reported, where
/// they arise: the instances that passed admission, and the types of checked
/// bodies and signatures. A kept type stays unresolved, so it matches only
/// itself and is never taken for another type.
pub fn normalize_or_keep<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    normalize_ty(db, ty, scope, assumptions).unwrap_or(ty)
}

/// Proof that a normalization limit was reported to the user. Only
/// [`NormalizationLimit::report`] makes one. It is not `Copy`: each proof
/// stands for the one report it came with.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LimitReported(NormalizationLimit);

impl LimitReported {
    /// The value that stands for a type that reached the reported limit. It
    /// matches any type, so no further error is reported for it.
    pub(crate) fn recovery_ty(self, db: &dyn HirAnalysisDb) -> TyId<'_> {
        let limit = self.0;
        TyId::invalid(
            db,
            InvalidCause::NormalizationLimit {
                limit,
                stand_in: LimitStandIn::Reported(self),
            },
        )
    }
}

/// Apply declared associated equalities without implementation selection.
/// Structural slot planning uses declaration coordinates before the slots it
/// is discovering exist, so implementation lookup would create a query cycle.
/// A projection that would pass a limit is left unresolved.
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

/// The layout roots that `ty`'s associated types expose once resolved. A
/// limit in resolving them, or in normalizing a root's owner, is returned:
/// without it the roots could differ, and with them the layout.
pub(crate) fn normalize_layout_root_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<Vec<LayoutRootUse<'db>>, NormalizationLimit> {
    fn collect<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        visiting: &mut rustc_hash::FxHashSet<TyId<'db>>,
        uses: &mut Vec<LayoutRootUse<'db>>,
    ) -> Result<(), NormalizationLimit> {
        if !visiting.insert(ty) {
            return Ok(());
        }
        if let TyData::AssocTy(assoc) = ty.data(db) {
            let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
            if let Selection::Unique(resolved) =
                resolve_trait_impl_instance(db, solve_cx, assoc.trait_.as_predicate(db))
                && let ImplementorOrigin::Hir(impl_trait) = resolved.selected().origin(db)
            {
                for root_use in resolved.assoc_ty_layout_root_uses(db, assoc.name) {
                    let owner = root_use
                        .owner
                        .map(|owner| {
                            let owner = Binder::bind(impl_trait.into(), owner)
                                .instantiate(db, resolved.impl_args(db));
                            normalize_ty(db, owner, scope, assumptions)
                        })
                        .transpose()?;
                    let root_use = LayoutRootUse {
                        value: Binder::bind(impl_trait.into(), root_use.value)
                            .instantiate(db, resolved.impl_args(db)),
                        owner,
                        selector: root_use.selector,
                    };
                    if !uses.contains(&root_use) {
                        uses.push(root_use);
                    }
                }
                if let Some(instantiated) = resolved.instantiated_assoc_ty(db, assoc.name) {
                    collect(db, instantiated, scope, assumptions, visiting, uses)?;
                }
            }
        } else {
            let (base, args) = ty.decompose_ty_app(db);
            if base != ty {
                collect(db, base, scope, assumptions, visiting, uses)?;
            }
            for arg in args {
                collect(db, *arg, scope, assumptions, visiting, uses)?;
            }
        }
        visiting.remove(&ty);
        Ok(())
    }

    let mut uses = Vec::new();
    collect(
        db,
        ty,
        scope,
        assumptions,
        &mut rustc_hash::FxHashSet::default(),
        &mut uses,
    )?;
    Ok(uses)
}

/// How many projections may be in the middle of being resolved at once.
const PROJECTION_DEPTH_LIMIT: usize = 64;

/// How many type nodes, counted as a tree, the projections resolved by one
/// normalization may have in total.
const PROJECTION_WORK_LIMIT: usize = 65536;

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
    /// The first limit reached. Once one is reached nothing more is resolved
    /// or cached, and the whole normalization fails.
    limit: Option<NormalizationLimit>,
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
            limit: None,
        }
    }

    /// Starts resolving the projection `ty`, or records the limit it would
    /// exceed.
    ///
    /// The cycle guard stops a projection that comes back to itself, but not
    /// one whose impl defines it through a larger projection, such as
    /// `type Out = <W<(T, T)> as Tr>::Out`: that chain never repeats. Its
    /// types can also double at each step, staying small as interned values
    /// while growing exponentially as trees, which is how resolution walks
    /// them. So both the nesting and the total size are limited.
    fn enter_projection(&mut self, ty: TyId<'db>) -> Result<(), NormalizationLimit> {
        if self.projection_depth >= PROJECTION_DEPTH_LIMIT {
            return Err(self.reach(NormalizationLimit::Nesting));
        }
        let remaining = PROJECTION_WORK_LIMIT - self.projection_work;
        let Some(size) = tree_size_within(self.db, ty, remaining) else {
            return Err(self.reach(NormalizationLimit::Work));
        };
        self.projection_work += size;
        self.projection_depth += 1;
        Ok(())
    }

    fn reach(&mut self, limit: NormalizationLimit) -> NormalizationLimit {
        *self.limit.get_or_insert(limit)
    }
}

/// The number of nodes in `ty` counted as a tree, if it is at most `limit`.
fn tree_size_within<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, limit: usize) -> Option<usize> {
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
    }
    fn size<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        limit: usize,
        memo: &mut FxHashMap<TyId<'db>, usize>,
    ) -> Option<usize> {
        if let Some(&known) = memo.get(&ty) {
            return Some(known);
        }
        let mut children = Children {
            db,
            children: Vec::new(),
        };
        walk_ty(&mut children, ty);
        let mut total = 1usize;
        for child in children.children {
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
        // The normalization has already failed; stop working on it.
        if self.limit.is_some() {
            return ty;
        }
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

                if self.enter_projection(ty).is_err() {
                    self.cache.remove(assoc_ty);
                    return ty;
                }
                let resolved = self
                    .try_resolve_assoc_ty(ty, assoc_ty)
                    .map(|replacement| self.fold_ty(db, replacement));
                self.projection_depth -= 1;
                // Not resolved; still fold internals (e.g., normalize self type)
                let result = resolved.unwrap_or_else(|| ty.super_fold_with(db, self));
                // A limit makes the whole normalization fail: nothing after
                // it is kept as an answer.
                if self.limit.is_some() {
                    self.cache.remove(assoc_ty);
                } else {
                    self.cache.insert(*assoc_ty, Some(result));
                }
                result
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
            let Some(bound) = pred.bound_assoc_ty(self.db, assoc.name) else {
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

        // 3) Fall back to the general associated type search used by path resolution,
        //    but restrict results to the same trait as `assoc` and deduplicate by
        //    the resulting type. If all viable candidates agree on a single type,
        //    normalize to that type.
        //
        // First attempt an impl-based lookup across relevant ingots (Self's + trait's),
        // mirroring trait-method resolution. This allows normalization to succeed even
        // when the calling scope is in a different ingot (e.g., core code instantiated
        // with std types).
        if let Some(resolved) = self.try_resolve_assoc_ty_from_impls(assoc) {
            return Some(resolved);
        }

        //    Search by the trait's self type: `SelfTy::assoc.name`.
        // Normalize the trait's self type before candidate search.
        let self_ty = self.fold_ty(self.db, assoc.trait_.self_ty(self.db));
        let mut raw_cands = match find_associated_type(
            self.db,
            self.scope,
            Canonicalized::new(self.db, self_ty),
            assoc.name,
            self.assumptions,
        ) {
            Ok(raw_cands) => raw_cands,
            Err(FindAssociatedTypeError::InfiniteBoundRecursion) => return None,
        };

        raw_cands.retain(|(inst, _)| self.trait_refs_match(target, inst.trait_ref(self.db)));

        // Deduplicate by normalized result type (to handle cases where multiple
        // impls yield the same associated type, e.g., Output = Self for all impls).
        let mut dedup: IndexMap<TyId<'db>, ()> = IndexMap::new();
        for (_, t) in raw_cands.into_iter() {
            // Continue folding so nested associated types are also normalized
            let norm_t = self.fold_ty(self.db, t);
            dedup.entry(norm_t).or_insert(());
        }

        match dedup.len() {
            0 => None,
            1 => {
                let (unique, _) = dedup.first().unwrap();
                // Only replace if we're actually making progress
                if *unique != ty { Some(*unique) } else { None }
            }
            _ => None,
        }
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

    fn try_resolve_assoc_ty_from_impls(&mut self, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        let trait_inst = assoc.trait_.fold_with(self.db, self).as_predicate(self.db);
        let trait_def = trait_inst.def(self.db);
        let canonical_self_ty = Canonical::new(self.db, trait_inst.self_ty(self.db));

        let mut dedup: IndexMap<TyId<'db>, ()> = IndexMap::new();

        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(self.db, trait_inst);
        let search_ingots = [Some(primary), secondary];

        // Canonicalize the target trait instance so we can unify against it in a
        // fresh table without mixing inference keys from other tables.
        let canonical_target = Canonicalized::new(self.db, trait_inst);
        canonical_target.with_materialized(self.db, |cx| {
            let target_inst = cx.query();
            let original_target = cx.try_extract::<TraitInstId<'db>>(target_inst);
            for ingot in search_ingots.into_iter().flatten() {
                for implementor in impls_for_trait_and_ty_with_possible_constraints(
                    self.db,
                    ingot,
                    trait_def,
                    canonical_self_ty,
                    self.assumptions,
                ) {
                    let Some(implementor) =
                        complete_impl_assoc_ty(self.db, implementor, assoc.name)
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
                        let norm = self.fold_ty(self.db, folded);
                        dedup.entry(norm).or_insert(());
                    }
                }
            }
        });

        match dedup.len() {
            0 => None,
            1 => Some(*dedup.first().unwrap().0),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    /// Only type lowering and constant evaluation of an expression written in
    /// a type make a [`super::WrittenType`]: the stand-in it allows is
    /// reported where the type is written, which only those two know.
    #[test]
    fn only_lowering_and_const_evaluation_make_a_written_type() {
        fn visit(dir: &std::path::Path, found: &mut Vec<String>) {
            for entry in std::fs::read_dir(dir).unwrap() {
                let path = entry.unwrap().path();
                if path.is_dir() {
                    visit(&path, found);
                } else if path.extension().is_some_and(|ext| ext == "rs") {
                    let text = std::fs::read_to_string(&path).unwrap();
                    let calls = text.matches(concat!("WrittenType", "::new(")).count();
                    for _ in 0..calls {
                        found.push(path.file_name().unwrap().to_string_lossy().into_owned());
                    }
                }
            }
        }
        let mut found = Vec::new();
        visit(
            &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src"),
            &mut found,
        );
        found.sort();
        assert_eq!(found, ["const_ty.rs", "ty_lower.rs"]);
    }
}
