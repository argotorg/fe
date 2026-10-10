//! Type normalization module
//!
//! This module provides functionality to normalize types by resolving associated types
//! to concrete types when possible. This happens before type unification to ensure
//! that types are in their most resolved form.

use crate::core::hir_def::{ImplTrait, scope_graph::ScopeId};
use crate::span::DynLazySpan;
use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    binder::Binder,
    candidates::{self, Counting, Holds, Item, Question},
    canonical::Canonical,
    canonical::Canonicalized,
    diagnostics::{TyDiagCollection, TyLowerDiag},
    fold::{TyFoldable, TyFolder},
    layout_holes::LayoutRootUse,
    trait_def::{ImplementorOrigin, TraitInstId, TraitRefId, resolve_trait_impl_instance},
    trait_lower::complete_impl_assoc_ty,
    trait_resolution::{PredicateListId, Selection, TraitSolveCx},
    ty_def::{AssocTy, InvalidCause, TyData, TyId, TyParam, collect_variables},
    unify::UnificationTable,
    visitor::{TyVisitor, walk_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{FindAssociatedTypeError, find_associated_type_for_trait},
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
///
/// The outcome depends only on the type, the scope and the assumptions, so
/// it is computed once for each and shared by every use.
pub fn normalize_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<TyId<'db>, NormalizationLimit> {
    normalize_ty_query(db, ty, scope, assumptions).ty
}

/// Normalizes `ty` like [`normalize_ty`], and also returns what resolving
/// its associated types cost: the sum, over the distinct projections it names
/// outside any projection, of the type nodes each is charged against
/// [`PROJECTION_WORK_LIMIT`] as a use of its own. A projection named again
/// adds nothing: the copies of its result are counted by the size of the
/// type. Each projection counts its own cost whether it was resolved first or
/// met again after resolving inside another one, so the sum does not depend
/// on the order of the parts.
pub(crate) fn normalize_ty_with_cost<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> (Result<TyId<'db>, NormalizationLimit>, usize) {
    let normalized = normalize_ty_query(db, ty, scope, assumptions);
    (normalized.ty, normalized.cost)
}

/// A type's normal form, or the limit it reached, and what resolving its
/// associated types cost.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
struct Normalized<'db> {
    ty: Result<TyId<'db>, NormalizationLimit>,
    cost: usize,
}

/// Normalizing a type needs the same normalization again only through a
/// cycle, which is the outcome until it resolves.
#[salsa::tracked(
    cycle_fn=normalize_ty_cycle_recover,
    cycle_initial=normalize_ty_cycle_initial
)]
fn normalize_ty_query<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Normalized<'db> {
    if is_already_normal(db, ty) {
        return Normalized {
            ty: Ok(ty),
            cost: 0,
        };
    }
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    let normalized = ty.fold_with(db, &mut normalizer);
    Normalized {
        ty: match normalizer.outcome() {
            Some(limit) => Err(limit),
            None => Ok(normalized),
        },
        cost: normalizer.cost,
    }
}

/// Normalizes `ty` without the shared query, for trait solving.
///
/// Trait solving and normalization call each other: selecting the impl that
/// defines a projection proves the impl's where clauses, which may name that
/// projection again. Such a loop is closed by the solver's own query, from
/// its single starting answer, whichever side the loop was entered from.
/// Inside the solver, normalization is therefore computed directly, never
/// through [`normalize_ty`]'s query, whose cycle answer would otherwise
/// decide the loop when normalization happened to be entered first. The
/// outcome is the same function of the type, the scope and the assumptions.
pub(crate) fn normalize_ty_in_solver<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<TyId<'db>, NormalizationLimit> {
    if is_already_normal(db, ty) {
        return Ok(ty);
    }
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    let normalized = ty.fold_with(db, &mut normalizer);
    match normalizer.outcome() {
        Some(limit) => Err(limit),
        None => Ok(normalized),
    }
}

/// Whether `ty` has nothing to normalize: no projection, no type parameter
/// (`Self` in an impl stands for the impl's type) and no constant to
/// evaluate. Such a type is its own normal form, however deep it is; it is
/// checked without recursion, so a deep type does not exhaust the stack.
fn is_already_normal<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if ty.has_projection(db) || ty.has_param(db) {
        return false;
    }
    let mut seen = FxHashSet::default();
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty) {
            continue;
        }
        if matches!(ty.data(db), TyData::ConstTy(_)) {
            return false;
        }
        pending.extend(child_tys(db, ty));
    }
    true
}

/// The types directly inside `ty`, in order.
fn child_tys<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Vec<TyId<'db>> {
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
    let mut children = Children {
        db,
        children: Vec::new(),
    };
    walk_ty(&mut children, ty);
    children.children
}

fn normalize_ty_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _ty: TyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> Normalized<'db> {
    Normalized {
        ty: Err(NormalizationLimit::Cycle),
        cost: 0,
    }
}

fn normalize_ty_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Normalized<'db>,
    _count: u32,
    _ty: TyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> salsa::CycleRecoveryAction<Normalized<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

/// A limit that normalizing a type reached. Whether a type reaches a limit
/// depends only on the type, its scope and assumptions, and the limits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum NormalizationLimit {
    /// One use of an associated type needs more than
    /// [`PROJECTION_DEPTH_LIMIT`] projections resolved inside each other.
    Nesting,
    /// One use costs more than [`PROJECTION_WORK_LIMIT`] type nodes, counting
    /// the projection, its result and every projection resolved for it, as
    /// trees.
    Work,
    /// The result of one use is nested more than [`TYPE_DEPTH_LIMIT`] levels
    /// deep.
    Depth,
    /// A projection whose resolution needs the projection itself.
    Cycle,
}

impl NormalizationLimit {
    /// The order in which limits are named when parts of a type reach
    /// different ones: a cycle first, then nesting, depth, and work.
    pub(crate) fn priority(self) -> u8 {
        match self {
            Self::Cycle => 0,
            Self::Nesting => 1,
            Self::Depth => 2,
            Self::Work => 3,
        }
    }

    /// The limit named when two parts of an answer depend on different
    /// limits: the one first in [`Self::priority`] order, whatever the order
    /// the parts were met in.
    pub(crate) fn join(earlier: Option<Self>, limit: Self) -> Self {
        match earlier {
            Some(earlier) if earlier.priority() <= limit.priority() => earlier,
            _ => limit,
        }
    }

    /// How many nested steps the nesting limit allows.
    pub const NESTING: usize = PROJECTION_DEPTH_LIMIT;
    /// How many type nodes the work limit allows.
    pub const WORK: usize = PROJECTION_WORK_LIMIT;
    /// How deeply a type may be nested.
    pub const DEPTH: usize = TYPE_DEPTH_LIMIT;

    /// What resolving the associated types of a type that reached this limit
    /// needs, as a clause: "resolving the associated types here {reason}".
    pub fn reason(self) -> String {
        match self {
            Self::Nesting => format!("needs more than {} nested steps", grouped(Self::NESTING)),
            Self::Work => format!("needs more than {} type nodes of work", grouped(Self::WORK)),
            Self::Depth => format!(
                "gives a type nested more than {} levels deep",
                grouped(Self::DEPTH)
            ),
            Self::Cycle => "needs the type it is resolving".to_string(),
        }
    }

    /// Reports this limit for the type at `span`: the diagnostic, and the
    /// proof that it was reported.
    pub(crate) fn report<'db>(
        self,
        span: DynLazySpan<'db>,
    ) -> (TyDiagCollection<'db>, LimitReported) {
        let diag = match self {
            Self::Cycle => TyLowerDiag::TypeLoweringCycle(span).into(),
            limit => TyLowerDiag::TypeNormalizationLimit { span, limit }.into(),
        };
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
pub(crate) fn grouped(n: usize) -> String {
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
            let resolved =
                match resolve_trait_impl_instance(db, solve_cx, assoc.trait_.as_predicate(db)) {
                    Selection::Unique(resolved) => Some(resolved),
                    Selection::NormalizationLimit(limit) => return Err(limit),
                    Selection::Ambiguous(_) | Selection::NotFound => None,
                };
            if let Some(resolved) = resolved
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

// The limits apply to each use of an associated type: each projection or
// written in a type, or met by the type checker, is
// measured on its own, by a function of the projection, the scope and the
// assumptions. A type reaches a limit exactly when one of its uses does, so
// how its parts are grouped or ordered never matters.

/// How many projections one use may need resolved inside each other.
const PROJECTION_DEPTH_LIMIT: usize = 64;

/// How many type nodes one use may cost, counted as trees: the projection,
/// its result, and the cost of each projection resolved for it.
const PROJECTION_WORK_LIMIT: usize = 65536;

/// How deeply nested the result of one use may be. Later passes walk types
/// recursively, so a type thousands of levels deep would exhaust the stack
/// even when it is small as a tree.
const TYPE_DEPTH_LIMIT: usize = 1024;

pub struct TypeNormalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_impls: bool,
    /// Projections being resolved, and the results of those resolved.
    cache: FxHashMap<TyId<'db>, CacheEntry<'db>>,
    /// The projections being resolved, innermost last.
    frames: Vec<Frame<'db>>,
    /// Projections currently being resolved, counted against
    /// [`PROJECTION_DEPTH_LIMIT`].
    projection_depth: usize,
    /// The depth of each result put in the cache, see [`Self::depth_of`].
    depths: FxHashMap<TyId<'db>, usize>,
    /// How deep in the type being built the fold is. A projection's result
    /// is built in place; past [`TYPE_DEPTH_LIMIT`] levels below where the
    /// outermost use being resolved stands, its result is too deep, and the
    /// fold stops there rather than going deeper first.
    fold_depth: usize,
    /// The first limit reached by the outermost use being resolved. Once one
    /// is reached nothing more is resolved or cached for that use.
    limit: Option<NormalizationLimit>,
    /// The limit the whole type reaches: of the limits its outermost uses
    /// reached, the first by [`NormalizationLimit::priority`], so that it
    /// does not depend on the order of the parts.
    reached: Option<NormalizationLimit>,
    /// What the distinct outermost uses met so far cost, summed.
    cost: usize,
    /// The outermost uses already summed in [`Self::cost`].
    costed: FxHashSet<TyId<'db>>,
}

#[derive(Clone, Copy)]
enum CacheEntry<'db> {
    /// Being resolved. Meeting it again is a cycle.
    InProgress,
    /// Resolved to `result`, which needed `height` levels of nesting,
    /// counting this projection, is `depth` levels deep, and cost `cost`
    /// type nodes.
    Done {
        result: TyId<'db>,
        height: usize,
        depth: usize,
        cost: usize,
    },
}

/// A projection being resolved.
struct Frame<'db> {
    /// Its nesting depth, counting itself.
    depth: usize,
    /// The greatest height of the projections resolved inside it.
    below: usize,
    /// The greatest height of the projections resolved while folding its
    /// parts after it could not be resolved, at its own depth.
    beside: usize,
    /// What it has cost so far, in type nodes counted as trees.
    cost: usize,
    /// The projections whose cost it includes. Each is counted once: the
    /// normalizer may fold the same part several times while resolving one
    /// projection (its trait reference, then each candidate), and that must
    /// not change what the projection costs. Repeated occurrences in a
    /// result are counted by the result's size as a tree.
    counted: FxHashSet<TyId<'db>>,
    /// The fold depth where it stands.
    fold_base: usize,
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
            frames: Vec::new(),
            projection_depth: 0,
            depths: FxHashMap::default(),
            fold_depth: 0,
            limit: None,
            reached: None,
            cost: 0,
            costed: FxHashSet::default(),
        }
    }

    /// The limit the type reached, if any.
    fn outcome(&self) -> Option<NormalizationLimit> {
        self.reached
    }

    /// Starts resolving the projection `ty`, or records the limit it would
    /// exceed.
    ///
    /// A projection that comes back to itself is a cycle. One whose impl
    /// defines it through a larger projection, such as
    /// `type Out = <W<(T, T)> as Tr>::Out`, never repeats. Its types can also
    /// double at each step, staying small as interned values while growing
    /// exponentially as trees, which is how resolution walks them. So the
    /// nesting, the total size and the depth are limited.
    fn enter_projection(&mut self, ty: TyId<'db>) -> Result<(), NormalizationLimit> {
        if self.projection_depth >= PROJECTION_DEPTH_LIMIT {
            return Err(self.reach(NormalizationLimit::Nesting));
        }
        let size = tree_size_within(self.db, ty, PROJECTION_WORK_LIMIT, TYPE_DEPTH_LIMIT)
            .map_err(|limit| self.reach(limit))?;
        self.projection_depth += 1;
        let depth = self.projection_depth;
        self.cache.insert(ty, CacheEntry::InProgress);
        self.frames.push(Frame {
            depth,
            below: 0,
            beside: 0,
            cost: size,
            counted: FxHashSet::default(),
            // Its result is placed where it stands, one level above the fold.
            fold_base: self.fold_depth.saturating_sub(1),
        });
        Ok(())
    }

    /// Adds the cost of `ty` to the innermost projection being resolved, once,
    /// the work limit past [`PROJECTION_WORK_LIMIT`]. Outside any projection
    /// nothing is added: each outermost use is measured on its own.
    fn add_cost(&mut self, ty: TyId<'db>, cost: usize) {
        let Some(frame) = self.frames.last_mut() else {
            return;
        };
        if !frame.counted.insert(ty) {
            return;
        }
        frame.cost = frame.cost.saturating_add(cost);
        if frame.cost > PROJECTION_WORK_LIMIT {
            self.reach(NormalizationLimit::Work);
        }
    }

    /// Adds the cost of `ty`, an outermost use, to [`Self::cost`], once.
    fn add_outer_cost(&mut self, ty: TyId<'db>, cost: usize) {
        if self.frames.is_empty() && self.costed.insert(ty) {
            self.cost = self.cost.saturating_add(cost);
        }
    }

    /// Records `limit`. Inside a use, the first limit stops that use; at the
    /// outermost level, it is kept by priority and the next part goes on.
    fn reach(&mut self, limit: NormalizationLimit) -> NormalizationLimit {
        if self.frames.is_empty() {
            self.settle(limit);
            limit
        } else {
            *self.limit.get_or_insert(limit)
        }
    }

    /// Keeps the first of `limit` and the limit already reached by
    /// [`NormalizationLimit::priority`].
    fn settle(&mut self, limit: NormalizationLimit) {
        self.reached = Some(match self.reached {
            Some(reached) if reached.priority() <= limit.priority() => reached,
            _ => limit,
        });
    }

    /// How many levels deep `ty` is, counting itself. Called only on results
    /// that passed the depth limit, so the walk is shallow.
    fn depth_of(&mut self, ty: TyId<'db>) -> usize {
        if let Some(&depth) = self.depths.get(&ty) {
            return depth;
        }
        let depth = 1 + child_tys(self.db, ty)
            .into_iter()
            .map(|child| self.depth_of(child))
            .max()
            .unwrap_or(0);
        self.depths.insert(ty, depth);
        depth
    }

    /// The cached result for `assoc_ty`, if any. A complete result counts as
    /// the nesting it needed, so using it never passes the depth limit that
    /// resolving it again here would reach. A projection still being
    /// resolved is a cycle.
    fn lookup(&mut self, ty: TyId<'db>) -> Option<TyId<'db>> {
        match *self.cache.get(&ty)? {
            CacheEntry::InProgress => {
                self.reach(NormalizationLimit::Cycle);
                Some(ty)
            }
            CacheEntry::Done {
                result,
                height,
                depth,
                cost,
            } => {
                if self.projection_depth + height > PROJECTION_DEPTH_LIMIT {
                    self.reach(NormalizationLimit::Nesting);
                    return Some(ty);
                }
                // Inside a use being resolved, the result goes into that
                // use's result, as deep as the fold is below where it stands.
                if let Some(frame) = self.frames.first()
                    && self.fold_depth.saturating_sub(frame.fold_base) + depth
                        > TYPE_DEPTH_LIMIT + 1
                {
                    self.reach(NormalizationLimit::Depth);
                    return Some(ty);
                }
                self.record_height(height);
                self.add_cost(ty, cost);
                self.add_outer_cost(ty, cost);
                Some(result)
            }
        }
    }

    /// Records a projection of `height` met by the innermost one being
    /// resolved.
    fn record_height(&mut self, height: usize) {
        let depth = self.projection_depth;
        if let Some(frame) = self.frames.last_mut() {
            if depth == frame.depth {
                frame.below = frame.below.max(height);
            } else {
                frame.beside = frame.beside.max(height);
            }
        }
    }

    /// Ends resolving `assoc_ty` with `result`, which is charged like the
    /// projection was. The result is cached unless a limit was reached.
    fn finish(&mut self, ty: TyId<'db>, result: TyId<'db>) -> TyId<'db> {
        let frame = self.frames.pop().expect("a projection is being resolved");
        let height = (frame.below + 1).max(frame.beside);
        self.record_height(height);
        let mut cost = frame.cost;
        if self.limit.is_none() {
            match tree_size_within(
                self.db,
                result,
                PROJECTION_WORK_LIMIT - cost.min(PROJECTION_WORK_LIMIT),
                TYPE_DEPTH_LIMIT,
            ) {
                Ok(size) => cost += size,
                Err(limit) => {
                    self.limit.get_or_insert(limit);
                }
            }
        }
        if self.limit.is_some() {
            self.cache.remove(&ty);
        } else {
            let depth = self.depth_of(result);
            self.cache.insert(
                ty,
                CacheEntry::Done {
                    result,
                    height,
                    depth,
                    cost,
                },
            );
            self.add_cost(ty, cost);
            self.add_outer_cost(ty, cost);
        }
        // The outermost use is done: its limit, if any, is the type's, and
        // the next part is measured afresh.
        if self.frames.is_empty()
            && let Some(limit) = self.limit.take()
        {
            self.settle(limit);
        }
        result
    }
}

/// The number of nodes in `ty` counted as a tree, if it is at most `limit`
/// and no path from the root has more than `depth_limit` nodes.
pub(crate) fn tree_size_within<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    limit: usize,
    depth_limit: usize,
) -> Result<usize, NormalizationLimit> {
    /// Size and depth of each type measured so far.
    type Memo<'db> = FxHashMap<TyId<'db>, (usize, usize)>;
    fn measure<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        limit: usize,
        depth_limit: usize,
        memo: &mut Memo<'db>,
    ) -> Result<(usize, usize), NormalizationLimit> {
        if let Some(&(size, depth)) = memo.get(&ty) {
            return if depth <= depth_limit {
                Ok((size, depth))
            } else {
                Err(NormalizationLimit::Depth)
            };
        }
        // Checked before descending, so the walk itself stays shallow.
        if depth_limit == 0 {
            return Err(NormalizationLimit::Depth);
        }
        let mut total = 1usize;
        let mut depth = 0;
        for child in child_tys(db, ty) {
            let (size, child_depth) = measure(db, child, limit, depth_limit - 1, memo)?;
            total = total
                .checked_add(size)
                .filter(|&total| total <= limit)
                .ok_or(NormalizationLimit::Work)?;
            depth = depth.max(child_depth);
        }
        memo.insert(ty, (total, depth + 1));
        Ok((total, depth + 1))
    }
    if limit == 0 {
        return Err(NormalizationLimit::Work);
    }
    measure(db, ty, limit, depth_limit, &mut FxHashMap::default()).map(|(size, _)| size)
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
        if let Some(frame) = self.frames.first()
            && self.fold_depth.saturating_sub(frame.fold_base) > TYPE_DEPTH_LIMIT
        {
            self.reach(NormalizationLimit::Depth);
            return ty;
        }
        self.fold_depth += 1;
        let folded = self.fold_ty_inner(db, ty);
        self.fold_depth -= 1;
        folded
    }
}

impl<'db> TypeNormalizer<'db> {
    fn fold_ty_inner(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyParam(p @ TyParam { owner, .. }) if p.is_trait_self() => {
                if let Some(impl_) = owner.resolve_to::<ImplTrait>(self.db) {
                    // Use the item method to obtain the implementor's self type.
                    let lowered = impl_.ty(self.db);
                    return self.fold_in_place(lowered);
                }
                ty
            }
            TyData::AssocTy(assoc_ty) => {
                if let Some(cached) = self.lookup(ty) {
                    return cached;
                }
                if self.enter_projection(ty).is_err() {
                    return ty;
                }
                let resolved = self
                    .try_resolve_assoc_ty(ty, assoc_ty)
                    .map(|replacement| self.fold_candidate(ty, replacement));
                self.projection_depth -= 1;
                if let Some(normalized) = resolved {
                    return self.finish(ty, normalized);
                }

                // Not resolved; still fold internals (e.g., normalize self type)
                let folded = ty.super_fold_with(db, self);
                self.finish(ty, folded)
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
                matching_bounds.insert(self.fold_candidate(ty, bound), ());
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
        if let Some(resolved) = self.try_resolve_assoc_ty_from_impls(ty, assoc) {
            return Some(resolved);
        }

        //    Search by the trait's self type: `SelfTy::assoc.name`.
        // Normalize the trait's self type before candidate search.
        let mut raw_cands = match find_associated_type_for_trait(
            self.db,
            self.scope,
            Canonicalized::new(self.db, target.as_predicate(self.db)),
            assoc.name,
            self.assumptions,
        ) {
            Ok(raw_cands) => raw_cands,
            Err(FindAssociatedTypeError::InfiniteBoundRecursion) => return None,
            Err(FindAssociatedTypeError::NormalizationLimit(limit)) => {
                self.reach(limit);
                return None;
            }
        };

        raw_cands.retain(|(inst, _)| self.trait_refs_match(target, inst.trait_ref(self.db)));

        // Deduplicate by normalized result type (to handle cases where multiple
        // impls yield the same associated type, e.g., Output = Self for all impls).
        let mut dedup: IndexMap<TyId<'db>, ()> = IndexMap::new();
        for (_, t) in raw_cands.into_iter() {
            // Continue folding so nested associated types are also normalized
            let norm_t = self.fold_candidate(ty, t);
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

    /// Folds `candidate`, a type that the projection `ty` may stand for. A
    /// candidate that is `ty` itself, as a bound `T: Tr` without a binding
    /// gives for `T::Out`, says nothing more about it: it is kept, and not
    /// resolved again, which would be taken for a cycle. An impl that defines
    /// `ty` as itself is a cycle, and is caught before this.
    fn fold_candidate(&mut self, ty: TyId<'db>, candidate: TyId<'db>) -> TyId<'db> {
        if candidate == ty {
            ty
        } else {
            self.fold_in_place(candidate)
        }
    }

    /// Folds `ty`, which stands where the type being folded is, at its depth.
    fn fold_in_place(&mut self, ty: TyId<'db>) -> TyId<'db> {
        self.fold_depth -= 1;
        let folded = self.fold_ty(self.db, ty);
        self.fold_depth += 1;
        folded
    }

    fn try_resolve_assoc_ty_from_impls(
        &mut self,
        ty: TyId<'db>,
        assoc: &AssocTy<'db>,
    ) -> Option<TyId<'db>> {
        let trait_inst = assoc.trait_.fold_with(self.db, self).as_predicate(self.db);
        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(self.db, trait_inst);
        let question = Question::header(
            self.db,
            Canonical::new(self.db, trait_inst),
            Item::AssocTy(assoc.name),
            [Some(primary), secondary],
        );
        let candidates = candidates::impl_candidates(self.db, question);

        // An impl is chosen by its header when only one header applies: its
        // where clauses are then checked where the impl is used (where the
        // type is written, or where `S: Tr` is needed), not here, so
        // normalizing does not depend on proving them. Only when several
        // headers apply do the where clauses choose among them.
        let holds: Vec<Holds> = if candidates.len() == 1 {
            vec![Holds::Yes]
        } else {
            candidates
                .iter()
                .map(|&candidate| {
                    candidates::impl_holds(
                        self.db,
                        solve_cx,
                        question,
                        candidate,
                        Counting::POSSIBLE,
                    )
                })
                .collect()
        };

        // The type each candidate that may apply gives.
        let mut types: Vec<Option<TyId<'db>>> = vec![None; candidates.len()];
        let canonical_target = Canonicalized::new(self.db, trait_inst);
        canonical_target.with_materialized(self.db, |cx| {
            let target_inst = cx.query();
            let original_target = cx.try_extract::<TraitInstId<'db>>(target_inst);
            for (idx, &implementor) in candidates.iter().enumerate() {
                if holds[idx] == Holds::No {
                    continue;
                }
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
                    // An impl that defines the projection as itself, as
                    // `type Out = <S as Tr>::Out` in `impl Tr for S`,
                    // needs the type it is resolving: the same cycle as
                    // `type Out = Self::Out`.
                    if folded == ty {
                        self.reach(NormalizationLimit::Cycle);
                        return;
                    }
                    types[idx] = Some(self.fold_candidate(ty, folded));
                }
            }
        });

        // The projection stands for the one type the applying impls give.
        let unknown: Vec<_> = holds
            .iter()
            .filter_map(|holds| match holds {
                Holds::Unknown(limit) => Some(*limit),
                _ => None,
            })
            .collect();
        let decided = candidates::decide(&unknown, |setting| {
            let mut setting = setting.iter();
            let mut results: IndexSet<TyId<'db>> = IndexSet::default();
            for (holds, result) in holds.iter().zip(&types) {
                let applies = match holds {
                    Holds::Yes | Holds::Undecided => true,
                    Holds::No => false,
                    Holds::Unknown(_) => *setting.next().unwrap(),
                };
                if applies && let Some(result) = result {
                    results.insert(*result);
                }
            }
            match results.len() {
                1 => results.first().copied().filter(|&unique| unique != ty),
                _ => None,
            }
        });
        match decided.or_limit() {
            Ok(resolved) => resolved,
            Err(limit) => {
                self.reach(limit);
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::ty::{
        normalize::normalize_ty_with_cost, trait_resolution::PredicateListId, ty_def::TyId,
    };
    use crate::test_db::{HirAnalysisTestDb, find_func};

    /// A type's work counts each of its projections once, the same whether
    /// one is resolved before or inside another (`<A as Tr>::Out` needs
    /// `<B as Tr>::Out`).
    #[test]
    fn the_work_of_a_type_does_not_depend_on_the_order_of_its_parts() {
        let src = "trait Tr { type Out }\nstruct A {}\nstruct B {}\n\
            impl Tr for A { type Out = (<B as Tr>::Out, <B as Tr>::Out) }\n\
            impl Tr for B { type Out = (u8, u8) }\n\
            fn parts(_ a: own <A as Tr>::Out, _ b: own <B as Tr>::Out, _ t: own (u8, u8)) {}\n";
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("work_order.fe".into(), src);
        let (top_mod, _) = db.top_mod(file);
        let func = find_func(&db, top_mod, "parts");
        let arg = |idx: usize| func.arg_tys(&db)[idx].instantiate_identity();
        let tuple = arg(2).decompose_ty_app(&db).0;
        let pair = |first, second| TyId::app(&db, TyId::app(&db, tuple, first), second);
        let cost =
            |ty| normalize_ty_with_cost(&db, ty, func.scope(), PredicateListId::empty_list(&db)).1;
        let (a, b) = (arg(0), arg(1));
        assert!(cost(b) > 0);
        assert_eq!(cost(pair(a, b)), cost(pair(b, a)));
        assert_eq!(cost(pair(a, b)), cost(a) + cost(b));
    }

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
