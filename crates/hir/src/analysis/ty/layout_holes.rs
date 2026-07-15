use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    const_ty::{
        BoundHoleId, CallableInputLayoutHoleOrigin, ConstTyData, ConstTyId, HoleAnchor, HoleId,
        LayoutBoundaryIdentity, LayoutInstantiationContext, LayoutInstantiationId,
        LayoutOccurrencePath, LayoutOccurrenceStep, LayoutRootId, LayoutShapeHoleKind,
        StructuralHoleId,
    },
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId, TyParam},
    ty_lower::func_implicit_param_plan,
    visitor::{TyVisitable, TyVisitor, walk_ty},
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::CallableDef;
use salsa::Update;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LayoutPlaceholderPolicy {
    HolesOnly,
    HolesAndImplicitParams,
}

pub(crate) fn layout_hole_fallback_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    hole_ty: TyId<'db>,
) -> TyId<'db> {
    if hole_ty.has_invalid(db) {
        TyId::u256(db)
    } else {
        hole_ty
    }
}

pub(crate) fn layout_hole_with_fallback_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    hole_ty: TyId<'db>,
    hole_id: HoleId<'db>,
) -> TyId<'db> {
    TyId::const_ty(
        db,
        ConstTyId::hole_with_id(db, layout_hole_fallback_ty(db, hole_ty), hole_id),
    )
}

fn is_layout_placeholder<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    policy: LayoutPlaceholderPolicy,
) -> bool {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return false;
    };

    match const_ty.data(db) {
        ConstTyData::Hole(..) => true,
        ConstTyData::TyParam(param, _)
            if policy == LayoutPlaceholderPolicy::HolesAndImplicitParams && param.is_implicit() =>
        {
            true
        }
        _ => false,
    }
}

pub(crate) fn structural_hole_id<'db>(
    db: &'db dyn HirAnalysisDb,
    placeholder: TyId<'db>,
) -> Option<StructuralHoleId<'db>> {
    let TyData::ConstTy(const_ty) = placeholder.data(db) else {
        return None;
    };
    let ConstTyData::Hole(_, HoleId::Structural(hole)) = const_ty.data(db) else {
        return None;
    };
    Some(*hole)
}

pub fn layout_root_id<'db>(
    db: &'db dyn HirAnalysisDb,
    placeholder: TyId<'db>,
) -> Option<LayoutRootId<'db>> {
    structural_hole_id(db, placeholder).map(|hole| hole.root(db))
}

/// Returns the semantic root and accepted const type carried by a structural
/// layout placeholder.
///
/// Consumers that need an assigned value must still resolve the root through
/// an [`AssignedLayoutTy`](crate::semantic::AssignedLayoutTy). This query only
/// exposes the stable identity embedded in the placeholder; it never
/// fabricates a scalar assignment for an indexed or multi-landing root.
pub fn layout_root_placeholder<'db>(
    db: &'db dyn HirAnalysisDb,
    placeholder: TyId<'db>,
) -> Option<(LayoutRootId<'db>, TyId<'db>)> {
    structural_hole_id(db, placeholder).map(|hole| {
        (
            hole.root(db),
            layout_hole_fallback_ty(db, hole.expected_ty(db)),
        )
    })
}

pub fn layout_root_descends_from<'db>(
    db: &'db dyn HirAnalysisDb,
    root: LayoutRootId<'db>,
    ancestor: LayoutRootId<'db>,
) -> bool {
    layout_root_lineage(db, root).any(|candidate| candidate == ancestor)
}

pub(crate) fn layout_root_lineage<'db>(
    db: &'db dyn HirAnalysisDb,
    root: LayoutRootId<'db>,
) -> impl Iterator<Item = LayoutRootId<'db>> + 'db {
    std::iter::successors(Some(root), move |root| match root.identity(db) {
        super::const_ty::LayoutRootIdentity::Source { .. } => None,
        super::const_ty::LayoutRootIdentity::Landing { source, .. } => Some(source),
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct LayoutIndexDimension<'db> {
    pub instance: LayoutInstantiationId<'db>,
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutRootUse<'db> {
    pub value: TyId<'db>,
    pub owner: Option<TyId<'db>>,
    pub selector: LayoutOccurrencePath,
    pub index_dimensions: Vec<LayoutIndexDimension<'db>>,
}

impl<'db> LayoutRootUse<'db> {
    pub fn root(&self, db: &'db dyn HirAnalysisDb) -> Option<LayoutRootId<'db>> {
        layout_root_id(db, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutInstantiation<'db> {
    pub ty: TyId<'db>,
    pub root_uses: Vec<LayoutRootUse<'db>>,
    pub instance: LayoutInstantiationId<'db>,
}

/// Source-level type shape with structural layout roots alpha-numbered in
/// deterministic first-occurrence order. Root provenance and landing identity
/// are intentionally absent, while the root equality partition, hole kind,
/// expected type, and every concrete const remain part of the key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct LayoutShapeKey<'db>(pub TyId<'db>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LayoutShapeIdentity<'db> {
    Structural(LayoutRootId<'db>),
    ImplicitParam(TyParam<'db>),
    Bound(BoundHoleId<'db>),
}

struct LayoutShapeFolder<'db> {
    ordinal_by_identity: FxHashMap<LayoutShapeIdentity<'db>, u32>,
    next: u32,
}

impl<'db> LayoutShapeFolder<'db> {
    fn placeholder(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        identity: LayoutShapeIdentity<'db>,
        kind: LayoutShapeHoleKind,
        expected_ty: TyId<'db>,
    ) -> TyId<'db> {
        let ordinal = *self.ordinal_by_identity.entry(identity).or_insert_with(|| {
            let ordinal = self.next;
            self.next += 1;
            ordinal
        });
        TyId::const_ty(
            db,
            ConstTyId::hole_with_id(
                db,
                expected_ty,
                HoleId::Bound(BoundHoleId::LayoutShape { ordinal, kind }),
            ),
        )
    }
}

impl<'db> TyFolder<'db> for LayoutShapeFolder<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        let TyData::ConstTy(const_ty) = ty.data(db) else {
            return ty.super_fold_with(db, self);
        };
        match const_ty.data(db) {
            ConstTyData::Hole(expected_ty, HoleId::Structural(hole)) => {
                let expected_ty = expected_ty.fold_with(db, self);
                self.placeholder(
                    db,
                    LayoutShapeIdentity::Structural(hole.root(db)),
                    hole.origin(db).shape_kind(),
                    expected_ty,
                )
            }
            ConstTyData::Hole(
                expected_ty,
                HoleId::Bound(
                    bound @ (BoundHoleId::CallableInput { .. } | BoundHoleId::LayoutShape { .. }),
                ),
            ) => {
                let kind = match bound {
                    BoundHoleId::LayoutShape { kind, .. } => *kind,
                    BoundHoleId::CallableInput { .. } => LayoutShapeHoleKind::DefaultHoleParam,
                    BoundHoleId::Opaque => unreachable!(),
                };
                let expected_ty = expected_ty.fold_with(db, self);
                self.placeholder(db, LayoutShapeIdentity::Bound(*bound), kind, expected_ty)
            }
            ConstTyData::TyParam(param, expected_ty) if param.is_implicit() => {
                let expected_ty = expected_ty.fold_with(db, self);
                self.placeholder(
                    db,
                    LayoutShapeIdentity::ImplicitParam(param.clone()),
                    LayoutShapeHoleKind::DefaultHoleParam,
                    expected_ty,
                )
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

pub(crate) fn layout_shape_value<'db, T>(db: &'db dyn HirAnalysisDb, value: T) -> T
where
    T: TyFoldable<'db>,
{
    value.fold_with(
        db,
        &mut LayoutShapeFolder {
            ordinal_by_identity: FxHashMap::default(),
            next: 0,
        },
    )
}

pub fn layout_shape_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    layout_shape_value(db, ty)
}

pub fn layout_shape_key<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> LayoutShapeKey<'db> {
    LayoutShapeKey(layout_shape_ty(db, ty))
}

fn layout_view_ty_descends_from<'db>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
    ancestor: TyId<'db>,
) -> bool {
    if value == ancestor {
        return true;
    }
    match (layout_root_id(db, value), layout_root_id(db, ancestor)) {
        (Some(value), Some(ancestor)) => return layout_root_descends_from(db, value, ancestor),
        (Some(_), None) | (None, Some(_)) => return false,
        (None, None) => {}
    }
    let (value_base, value_args) = value.decompose_ty_app(db);
    let (ancestor_base, ancestor_args) = ancestor.decompose_ty_app(db);
    !value_args.is_empty()
        && value_base == ancestor_base
        && value_args.len() == ancestor_args.len()
        && value_args
            .iter()
            .zip(ancestor_args)
            .all(|(value, ancestor)| layout_view_ty_descends_from(db, *value, *ancestor))
}

/// Whether `value` is the same semantic view state as `ancestor` after one or
/// more layout landings. Alpha-normalized shape alone is insufficient: it
/// would identify `Handle<LEFT, RIGHT>` with `Handle<RIGHT, LEFT>`. Every
/// structural root must also descend from the root at the same type position.
fn layout_view_state_descends_from<'db>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
    ancestor: TyId<'db>,
) -> bool {
    layout_shape_key(db, value) == layout_shape_key(db, ancestor)
        && layout_view_ty_descends_from(db, value, ancestor)
}

fn match_layout_view_argument<'db>(
    db: &'db dyn HirAnalysisDb,
    value_args: &[TyId<'db>],
    ancestor_args: &[TyId<'db>],
    value_idx: usize,
    visited: &mut [bool],
    matched_values: &mut [Option<usize>],
) -> bool {
    for (ancestor_idx, ancestor) in ancestor_args.iter().enumerate() {
        if visited[ancestor_idx]
            || !layout_view_ty_descends_from(db, value_args[value_idx], *ancestor)
        {
            continue;
        }
        visited[ancestor_idx] = true;
        if let Some(matched_value) = matched_values[ancestor_idx]
            && !match_layout_view_argument(
                db,
                value_args,
                ancestor_args,
                matched_value,
                visited,
                matched_values,
            )
        {
            continue;
        }
        matched_values[ancestor_idx] = Some(value_idx);
        return true;
    }
    false
}

/// Whether two applications differ only by a finite permutation of their
/// arguments. This permits a recursive target graph to unroll until the exact
/// view state recurs without admitting const transformations that manufacture
/// an unbounded sequence of new states.
fn layout_view_states_are_permutations<'db>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
    ancestor: TyId<'db>,
) -> bool {
    let (value_base, value_args) = value.decompose_ty_app(db);
    let (ancestor_base, ancestor_args) = ancestor.decompose_ty_app(db);
    if value_base != ancestor_base || value_args.len() != ancestor_args.len() {
        return false;
    }
    let mut matched_values = vec![None; ancestor_args.len()];
    value_args.iter().enumerate().all(|(value_idx, _)| {
        match_layout_view_argument(
            db,
            value_args,
            ancestor_args,
            value_idx,
            &mut vec![false; ancestor_args.len()],
            &mut matched_values,
        )
    })
}

/// Whether `value` is a proper nested type state of `ancestor`. Repeating a
/// nominal carrier while descending through `Handle<Handle<T>>` is finite and
/// must not be mistaken for a recursive target cycle.
fn layout_view_state_is_strict_subterm<'db>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
    ancestor: TyId<'db>,
) -> bool {
    let (_, args) = ancestor.decompose_ty_app(db);
    args.iter().any(|arg| {
        layout_view_state_descends_from(db, value, *arg)
            || layout_view_state_is_strict_subterm(db, value, *arg)
    })
}

/// The action required when a semantic layout-view walk reaches `value`.
///
/// Provider targets and callable layout schemas must make this decision from
/// the same ancestor set. Keeping the classifier here prevents the storage
/// walker and the runtime-evidence ABI from accepting different finite view
/// graphs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LayoutViewRecurrence {
    /// `value` is the same landed state as an ancestor and closes a back-edge.
    BackEdge { ancestor: usize },
    /// `value` is new, or remains inside a provably finite permutation/subterm
    /// family, so the walk must continue.
    Expand,
    /// Repeating the nominal carrier would create an unbounded view family.
    NonRegular { ancestor: usize },
}

/// Classifies one semantic view state against its active ancestors.
///
/// `ancestors` must be ordered nearest-first. Exact recurrence selects the
/// nearest matching state. A repeated expansion family remains finite when it
/// is a permutation or a proper subterm of *any* active state in that family;
/// restricting the check to the nearest state can reject a value that
/// legitimately returns to an older finite orbit. Expansion families are
/// selected provider implementations (or structural ADTs), not just carrier
/// names: a finite chain may revisit one carrier through different impls.
pub(crate) fn classify_layout_view_recurrence<'db, Family: PartialEq>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
    family: Family,
    ancestors: impl IntoIterator<Item = (usize, TyId<'db>, Family)>,
) -> LayoutViewRecurrence {
    let mut nearest_same_family = None;
    let mut finite_family = false;
    for (idx, ancestor, ancestor_family) in ancestors {
        if layout_view_state_descends_from(db, value, ancestor) {
            return LayoutViewRecurrence::BackEdge { ancestor: idx };
        }
        if family == ancestor_family {
            nearest_same_family.get_or_insert(idx);
            finite_family |= layout_view_states_are_permutations(db, value, ancestor)
                || layout_view_state_is_strict_subterm(db, value, ancestor);
        }
    }
    if let Some(ancestor) = nearest_same_family
        && !finite_family
    {
        LayoutViewRecurrence::NonRegular { ancestor }
    } else {
        LayoutViewRecurrence::Expand
    }
}

pub(crate) fn rewrite_layout_roots<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    roots: &FxHashMap<LayoutRootId<'db>, TyId<'db>>,
) -> T
where
    T: TyFoldable<'db>,
{
    rewrite_structural_holes(db, value, |hole, _| roots.get(&hole.root(db)).copied())
}

fn landed_hole<'db>(
    db: &'db dyn HirAnalysisDb,
    hole: StructuralHoleId<'db>,
    hole_ty: TyId<'db>,
    instance: LayoutInstantiationId<'db>,
) -> TyId<'db> {
    let mut trace = hole.trace(db).clone();
    trace.landings.push(instance);
    TyId::const_ty(
        db,
        ConstTyId::hole_with_id(
            db,
            hole_ty,
            HoleId::Structural(StructuralHoleId::new(
                db,
                hole.expected_ty(db),
                LayoutRootId::landing(db, hole.root(db), instance),
                hole.origin(db),
                trace,
            )),
        ),
    )
}

fn collect_root_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    value: TyId<'db>,
    selector: LayoutOccurrencePath,
    out: &mut Vec<LayoutRootUse<'db>>,
) {
    for placeholder in collect_layout_placeholders_in_order_with_policy(
        db,
        value,
        LayoutPlaceholderPolicy::HolesOnly,
    ) {
        if layout_root_id(db, placeholder).is_some() {
            out.push(LayoutRootUse {
                value: placeholder,
                owner: None,
                selector: selector.clone(),
                index_dimensions: Vec::new(),
            });
        }
    }
}

/// Instantiates a root-bearing type template at one explicit semantic
/// boundary. Type parameters clone every distinct root in their argument once
/// per parameter occurrence; const parameters forward their exact value. Body
/// roots land once per template application. Diagnostic traces are extended,
/// but only the resulting `LayoutRootId` controls sharing.
pub(crate) fn instantiate_layout_template<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TyId<'db>,
    params: &[TyId<'db>],
    args: &[TyId<'db>],
    context: LayoutInstantiationContext<'db>,
    boundary: LayoutBoundaryIdentity<'db>,
    occurrence: LayoutOccurrencePath,
) -> LayoutInstantiation<'db> {
    let instance = LayoutInstantiationId::new(db, context, boundary, occurrence);
    let mut instantiator = LayoutTemplateInstantiator {
        db,
        params,
        args,
        boundary,
        instance,
        path: Vec::new(),
        body_landings: FxHashMap::default(),
        root_uses: Vec::new(),
    };
    let ty = template.fold_with(db, &mut instantiator);
    debug_assert_eq!(
        collect_layout_placeholders_in_order_with_policy(
            db,
            ty,
            LayoutPlaceholderPolicy::HolesOnly,
        )
        .into_iter()
        .filter_map(|placeholder| layout_root_id(db, placeholder))
        .collect::<Vec<_>>(),
        instantiator
            .root_uses
            .iter()
            .filter_map(|root_use| root_use.root(db))
            .collect::<Vec<_>>(),
        "layout instantiation root uses must exactly cover the instantiated type",
    );
    LayoutInstantiation {
        ty,
        root_uses: instantiator.root_uses,
        instance,
    }
}

struct LayoutTemplateInstantiator<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    params: &'a [TyId<'db>],
    args: &'a [TyId<'db>],
    boundary: LayoutBoundaryIdentity<'db>,
    instance: LayoutInstantiationId<'db>,
    path: LayoutOccurrencePath,
    body_landings: FxHashMap<LayoutRootId<'db>, TyId<'db>>,
    root_uses: Vec<LayoutRootUse<'db>>,
}

impl<'a, 'db> LayoutTemplateInstantiator<'a, 'db> {
    fn param_index(&self, ty: TyId<'db>) -> Option<usize> {
        self.params.iter().position(|param| *param == ty)
    }

    fn with_path<T>(&mut self, step: LayoutOccurrenceStep, f: impl FnOnce(&mut Self) -> T) -> T {
        self.path.push(step);
        let value = f(self);
        self.path.pop();
        value
    }

    fn type_argument(&mut self, idx: usize, arg: TyId<'db>) -> TyId<'db> {
        let mut occurrence = self.path.clone();
        occurrence.push(LayoutOccurrenceStep::TypeParam(idx as u32));
        let instance = LayoutInstantiationId::new(
            self.db,
            LayoutInstantiationContext::Nested(self.instance),
            self.boundary,
            occurrence.clone(),
        );
        let mut landed = FxHashMap::default();
        for hole in collect_unique_structural_holes_in_order(self.db, arg) {
            landed
                .entry(hole.root(self.db))
                .or_insert_with(|| landed_hole(self.db, hole, hole.expected_ty(self.db), instance));
        }
        let arg = rewrite_layout_roots(self.db, arg, &landed);
        collect_root_uses(self.db, arg, occurrence, &mut self.root_uses);
        arg
    }

    fn const_argument(&mut self, idx: usize, arg: TyId<'db>) -> TyId<'db> {
        let mut selector = self.path.clone();
        selector.push(LayoutOccurrenceStep::ConstParam(idx as u32));
        collect_root_uses(self.db, arg, selector, &mut self.root_uses);
        arg
    }

    fn body_hole(&mut self, hole: StructuralHoleId<'db>, hole_ty: TyId<'db>) -> TyId<'db> {
        let mut occurrence = self.path.clone();
        occurrence.push(LayoutOccurrenceStep::TemplateBody);
        let instance = LayoutInstantiationId::new(
            self.db,
            LayoutInstantiationContext::Nested(self.instance),
            self.boundary,
            vec![LayoutOccurrenceStep::TemplateBody],
        );
        let placeholder = *self
            .body_landings
            .entry(hole.root(self.db))
            .or_insert_with(|| landed_hole(self.db, hole, hole_ty, instance));
        self.root_uses.push(LayoutRootUse {
            value: placeholder,
            owner: None,
            selector: occurrence,
            index_dimensions: Vec::new(),
        });
        placeholder
    }

    fn fold_application(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        base: TyId<'db>,
        args: &[TyId<'db>],
        step: impl Fn(usize) -> LayoutOccurrenceStep,
    ) -> TyId<'db> {
        let base = base.fold_with(db, self);
        let args = args
            .iter()
            .copied()
            .enumerate()
            .map(|(idx, arg)| self.with_path(step(idx), |this| arg.fold_with(db, this)))
            .collect::<Vec<_>>();
        TyId::foldl(db, base, &args)
    }
}

impl<'a, 'db> TyFolder<'db> for LayoutTemplateInstantiator<'a, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let Some(idx) = self.param_index(ty)
            && let Some(&arg) = self.args.get(idx)
        {
            return match ty.data(db) {
                TyData::TyParam(_) => self.type_argument(idx, arg),
                TyData::ConstTy(const_ty)
                    if matches!(const_ty.data(db), ConstTyData::TyParam(..)) =>
                {
                    self.const_argument(idx, arg)
                }
                _ => ty,
            };
        }

        if let Some(hole) = structural_hole_id(db, ty) {
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                unreachable!()
            };
            let ConstTyData::Hole(hole_ty, _) = const_ty.data(db) else {
                unreachable!()
            };
            return self.body_hole(hole, *hole_ty);
        }

        if let Some((kind, inner)) = ty.as_capability(db) {
            let inner = self.with_path(LayoutOccurrenceStep::GenericArg(0), |this| {
                inner.fold_with(db, this)
            });
            return match kind {
                super::ty_def::CapabilityKind::Mut => TyId::borrow_mut_of(db, inner),
                super::ty_def::CapabilityKind::Ref => TyId::borrow_ref_of(db, inner),
                super::ty_def::CapabilityKind::View => TyId::view_of(db, inner),
            };
        }

        let (base, args) = ty.decompose_ty_app(db);
        if !args.is_empty() {
            if ty.is_tuple(db) {
                return self.fold_application(db, base, args, |idx| {
                    LayoutOccurrenceStep::TupleElem(idx as u32)
                });
            }
            return self.fold_application(db, base, args, |idx| {
                LayoutOccurrenceStep::GenericArg(idx as u32)
            });
        }

        ty.super_fold_with(db, self)
    }

    fn fold_ty_app(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        abs: TyId<'db>,
        arg: TyId<'db>,
    ) -> TyId<'db> {
        TyId::new(db, TyData::TyApp(abs, arg))
    }
}

pub fn ty_contains_const_hole<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    struct HoleFinder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for HoleFinder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }

            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && matches!(const_ty.data(self.db), ConstTyData::Hole(..))
            {
                self.found = true;
                return;
            }

            walk_ty(self, ty);
        }
    }

    let mut finder = HoleFinder { db, found: false };
    ty.visit_with(&mut finder);
    finder.found
}

/// Chooses the canonical type produced by successful directional equality.
///
/// Layout holes are wildcards rather than inference variables, so ordinary
/// unification validates them without recording a substitution. At an
/// actual-to-expected boundary, an actual hole adopts the corresponding
/// expected term while an expected hole accepts the actual term. Recursing
/// structurally avoids replacing unrelated, already-concrete siblings merely
/// because another argument contained a hole.
pub(crate) fn merge_equated_layout_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    actual: TyId<'db>,
    expected: TyId<'db>,
) -> TyId<'db> {
    match (actual.data(db), expected.data(db)) {
        (TyData::ConstTy(actual), _) if matches!(actual.data(db), ConstTyData::Hole(..)) => {
            expected
        }
        (_, TyData::ConstTy(expected)) if matches!(expected.data(db), ConstTyData::Hole(..)) => {
            actual
        }
        (TyData::TyApp(actual_abs, actual_arg), TyData::TyApp(expected_abs, expected_arg)) => {
            let abs = merge_equated_layout_holes(db, *actual_abs, *expected_abs);
            let arg = merge_equated_layout_holes(db, *actual_arg, *expected_arg);
            if abs == *actual_abs && arg == *actual_arg {
                actual
            } else {
                TyId::new(db, TyData::TyApp(abs, arg))
            }
        }
        _ => actual,
    }
}

pub(crate) fn collect_layout_placeholders_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    struct LayoutPlaceholderCollector<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        policy: LayoutPlaceholderPolicy,
        out: &'a mut Vec<TyId<'db>>,
    }

    impl<'a, 'db> TyVisitor<'db> for LayoutPlaceholderCollector<'a, 'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if is_layout_placeholder(self.db, ty, self.policy) {
                self.out.push(ty);
            }
            walk_ty(self, ty);
        }
    }

    let mut out = Vec::new();
    value.visit_with(&mut LayoutPlaceholderCollector {
        db,
        policy,
        out: &mut out,
    });
    out
}

pub(crate) fn substitute_layout_placeholders_by_placeholder<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    layout_args: &FxHashMap<TyId<'db>, TyId<'db>>,
    policy: LayoutPlaceholderPolicy,
) -> T
where
    T: TyFoldable<'db>,
{
    if layout_args.is_empty() {
        return value;
    }

    substitute_layout_placeholders_with(db, value, policy, |placeholder| {
        layout_args.get(&placeholder).copied()
    })
}

fn substitute_layout_placeholders_with<'db, T, F>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
    mut lookup: F,
) -> T
where
    T: TyFoldable<'db>,
    F: FnMut(TyId<'db>) -> Option<TyId<'db>>,
{
    struct LayoutPlaceholderSubst<'a, 'db, F> {
        db: &'db dyn HirAnalysisDb,
        policy: LayoutPlaceholderPolicy,
        lookup: &'a mut F,
    }

    impl<'a, 'db, F> TyFolder<'db> for LayoutPlaceholderSubst<'a, 'db, F>
    where
        F: FnMut(TyId<'db>) -> Option<TyId<'db>>,
    {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if is_layout_placeholder(self.db, ty, self.policy)
                && let Some(arg) = (self.lookup)(ty)
            {
                return arg;
            }

            ty.super_fold_with(db, self)
        }

        fn fold_ty_app(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            abs: TyId<'db>,
            arg: TyId<'db>,
        ) -> TyId<'db> {
            TyId::new(db, TyData::TyApp(abs, arg))
        }
    }

    let mut folder = LayoutPlaceholderSubst {
        db,
        policy,
        lookup: &mut lookup,
    };
    value.fold_with(db, &mut folder)
}

pub(crate) fn substitute_layout_holes_by_placeholder_in<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    layout_args: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> T
where
    T: TyFoldable<'db>,
{
    substitute_layout_placeholders_by_placeholder(
        db,
        value,
        layout_args,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn substitute_layout_holes_by_placeholder<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    layout_args: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> TyId<'db> {
    substitute_layout_holes_by_placeholder_in(db, ty, layout_args)
}

pub(crate) fn collect_unique_layout_placeholders_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    let mut seen = FxHashSet::default();
    collect_layout_placeholders_in_order_with_policy(db, value, policy)
        .into_iter()
        .filter(|ty| seen.insert(*ty))
        .collect()
}

pub(crate) fn collect_unique_layout_placeholders_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_unique_layout_placeholders_in_order_with_policy(
        db,
        value,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn collect_unique_structural_holes_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<StructuralHoleId<'db>>
where
    T: TyVisitable<'db>,
{
    let mut seen = FxHashSet::default();
    collect_layout_placeholders_in_order_with_policy(db, value, LayoutPlaceholderPolicy::HolesOnly)
        .into_iter()
        .filter_map(|placeholder| structural_hole_id(db, placeholder))
        .filter(|hole| seen.insert(*hole))
        .collect()
}

pub(crate) fn collect_unique_app_bound_structural_holes_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<StructuralHoleId<'db>>
where
    T: TyVisitable<'db>,
{
    let holes = collect_unique_structural_holes_in_order(db, value);
    assert!(
        holes.iter().all(|hole| !matches!(
            hole.root(db).identity(db),
            super::const_ty::LayoutRootIdentity::Source {
                anchor: HoleAnchor::AliasTemplate(_),
                ..
            }
        )),
        "template-only structural hole escaped into semantic binding"
    );
    holes
}

pub(crate) fn rewrite_structural_holes<'db, T, F>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    mut rewrite: F,
) -> T
where
    T: TyFoldable<'db>,
    F: FnMut(StructuralHoleId<'db>, TyId<'db>) -> Option<TyId<'db>>,
{
    struct StructuralHoleRewriter<'a, 'db, F> {
        db: &'db dyn HirAnalysisDb,
        rewrite: &'a mut F,
    }

    impl<'a, 'db, F> TyFolder<'db> for StructuralHoleRewriter<'a, 'db, F>
    where
        F: FnMut(StructuralHoleId<'db>, TyId<'db>) -> Option<TyId<'db>>,
    {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && let ConstTyData::Hole(hole_ty, HoleId::Structural(hole_id)) =
                    const_ty.data(self.db)
                && let Some(replacement) = (self.rewrite)(*hole_id, *hole_ty)
            {
                return replacement;
            }

            ty.super_fold_with(db, self)
        }

        fn fold_ty_app(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            abs: TyId<'db>,
            arg: TyId<'db>,
        ) -> TyId<'db> {
            TyId::new(db, TyData::TyApp(abs, arg))
        }
    }

    value.fold_with(
        db,
        &mut StructuralHoleRewriter {
            db,
            rewrite: &mut rewrite,
        },
    )
}

/// Re-anchors every lowering-template hole in `value` at `anchor`, assigning
/// fresh sequential ordinals in first-visit order. Sharing-preserving: all
/// occurrences of one hole id map to one re-anchored id.
pub(crate) fn reanchor_template_holes<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    anchor: HoleAnchor<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    let mut next = 0u32;
    let mut renumbered: FxHashMap<LayoutRootId<'db>, LayoutRootId<'db>> = FxHashMap::default();
    rewrite_structural_holes(db, value, |hole_id, hole_ty| {
        if !hole_id.anchor(db).is_lowering_template() {
            return None;
        }
        let root = *renumbered.entry(hole_id.root(db)).or_insert_with(|| {
            let ordinal = next;
            next += 1;
            LayoutRootId::source(db, anchor, ordinal)
        });
        Some(TyId::const_ty(
            db,
            ConstTyId::hole_with_id(
                db,
                hole_ty,
                HoleId::Structural(StructuralHoleId::with_intro(
                    db,
                    hole_id.expected_ty(db),
                    root,
                    hole_id.origin(db),
                    hole_id.introduced_at(db).clone(),
                )),
            ),
        ))
    })
}

pub(crate) fn collect_layout_hole_tys_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_layout_placeholder_tys_in_order_with_policy(
        db,
        value,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn collect_layout_placeholder_tys_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_layout_placeholder_pairs_in_order_with_policy(db, value, policy)
        .into_iter()
        .map(|(_, hole_ty)| hole_ty)
        .collect()
}

/// Like [`collect_layout_placeholder_tys_in_order_with_policy`], but pairs
/// each (fallbacked) placeholder value type with the placeholder itself.
pub(crate) fn collect_layout_placeholder_pairs_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<(TyId<'db>, TyId<'db>)>
where
    T: TyVisitable<'db>,
{
    collect_layout_placeholders_in_order_with_policy(db, value, policy)
        .into_iter()
        .filter_map(|placeholder| {
            let TyData::ConstTy(const_ty) = placeholder.data(db) else {
                return None;
            };
            match const_ty.data(db) {
                ConstTyData::Hole(hole_ty, _) => {
                    Some((placeholder, layout_hole_fallback_ty(db, *hole_ty)))
                }
                ConstTyData::TyParam(param, ty) if param.is_implicit() => Some((placeholder, *ty)),
                _ => None,
            }
        })
        .collect()
}

pub(crate) fn callable_input_layout_bindings_by_origin<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
) -> FxHashMap<CallableInputLayoutHoleOrigin, Vec<(TyId<'db>, TyId<'db>)>> {
    let CallableDef::Func(func) = method else {
        return FxHashMap::default();
    };
    func_implicit_param_plan(db, func).bindings_by_origin
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use rustc_hash::FxHashMap;

    use super::{
        LayoutPlaceholderPolicy, collect_unique_app_bound_structural_holes_in_order,
        collect_unique_layout_placeholders_in_order, landed_hole, layout_shape_key,
        layout_view_states_are_permutations, merge_equated_layout_holes, reanchor_template_holes,
        structural_hole_id, substitute_layout_placeholders_by_placeholder,
    };
    use crate::analysis::ty::{
        const_ty::{
            ConstTyData, ConstTyId, HoleAnchor, HoleId, LayoutBoundaryIdentity, LayoutHoleArgSite,
            LayoutInstantiationContext, LayoutInstantiationId, LayoutIntroSite,
            LayoutOccurrenceStep, LayoutRootId, StructuralHoleOrigin,
        },
        trait_resolution::PredicateListId,
        ty_def::{Kind, PrimTy, TyBase, TyData, TyId, TyParam},
        ty_lower::lower_hir_ty,
    };
    use crate::core::semantic::trait_self_predicate;
    use crate::hir_def::{GenericArgListId, IdentId, ItemKind, PathId, scope_graph::ScopeId};
    use crate::test_db::HirAnalysisTestDb;

    fn usize_ty<'db>(db: &'db HirAnalysisTestDb) -> TyId<'db> {
        TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)))
    }

    fn mk_implicit_param_ty<'db>(
        db: &'db HirAnalysisTestDb,
        scope: ScopeId<'db>,
        idx: usize,
        name: &str,
    ) -> TyId<'db> {
        let param =
            TyParam::implicit_param(IdentId::new(db, name.to_string()), idx, Kind::Star, scope);
        TyId::const_ty(
            db,
            ConstTyId::new(db, ConstTyData::TyParam(param, usize_ty(db))),
        )
    }

    fn mk_hole_ty<'db>(db: &'db HirAnalysisTestDb) -> TyId<'db> {
        TyId::const_ty(db, ConstTyId::hole_with_ty(db, usize_ty(db)))
    }

    fn mk_structural_hole_ty<'db>(
        db: &'db HirAnalysisTestDb,
        identity: (HoleAnchor<'db>, u32),
    ) -> TyId<'db> {
        let root = LayoutRootId::source(db, identity.0, identity.1);
        TyId::const_ty(
            db,
            ConstTyId::structural_hole(
                db,
                usize_ty(db),
                StructuralHoleOrigin::ExplicitWildcard {
                    site: LayoutHoleArgSite::GenericArgList(GenericArgListId::none(db)),
                    arg_idx: 0,
                },
                LayoutIntroSite::lowering(
                    LayoutHoleArgSite::GenericArgList(GenericArgListId::none(db)),
                    0,
                ),
                root,
            ),
        )
    }

    fn mk_structural_hole_with_origin<'db>(
        db: &'db HirAnalysisTestDb,
        origin: StructuralHoleOrigin<'db>,
        introduced_at: LayoutIntroSite<'db>,
        identity: (HoleAnchor<'db>, u32),
    ) -> TyId<'db> {
        let root = LayoutRootId::source(db, identity.0, identity.1);
        TyId::const_ty(
            db,
            ConstTyId::structural_hole(db, usize_ty(db), origin, introduced_at, root),
        )
    }

    fn template_anchor<'db>(db: &'db HirAnalysisTestDb, scope: ScopeId<'db>) -> HoleAnchor<'db> {
        HoleAnchor::TemplatePath {
            path: PathId::from_str(db, "__template"),
            scope,
            assumptions: PredicateListId::empty_list(db),
        }
    }

    fn expect_structural_hole<'db>(
        db: &'db HirAnalysisTestDb,
        ty: TyId<'db>,
    ) -> super::StructuralHoleId<'db> {
        let TyData::ConstTy(const_ty) = ty.data(db) else {
            panic!("expected const-ty hole");
        };
        let ConstTyData::Hole(_, HoleId::Structural(hole_id)) = const_ty.data(db) else {
            panic!("expected structural hole");
        };
        *hole_id
    }

    fn mk_array_with_len<'db>(db: &'db HirAnalysisTestDb, len: TyId<'db>) -> TyId<'db> {
        TyId::app(db, TyId::array(db, TyId::u256(db)), len)
    }

    fn array_len_hole<'db>(
        db: &'db HirAnalysisTestDb,
        array_ty: TyId<'db>,
    ) -> super::StructuralHoleId<'db> {
        let (_, args) = array_ty.decompose_ty_app(db);
        expect_structural_hole(db, args[1])
    }

    #[test]
    fn equated_layout_holes_merge_per_argument() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("equated_layout_holes_merge_per_argument.fe"),
            "fn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let expected_first = mk_implicit_param_ty(&db, scope, 0, "__first");
        let actual_second = mk_implicit_param_ty(&db, scope, 1, "__second");
        let actual = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, mk_hole_ty(&db)),
                mk_array_with_len(&db, actual_second),
            ],
        );
        let expected = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, expected_first),
                mk_array_with_len(&db, mk_hole_ty(&db)),
            ],
        );

        let merged = merge_equated_layout_holes(&db, actual, expected);
        let (_, fields) = merged.decompose_ty_app(&db);
        let first = fields[0].decompose_ty_app(&db).1[1];
        let second = fields[1].decompose_ty_app(&db).1[1];
        assert_eq!(first, expected_first);
        assert_eq!(second, actual_second);
    }

    #[test]
    fn template_hole_identity_includes_assumptions() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("template_hole_identity_includes_assumptions.fe"),
            "struct Slot<const ROOT: u256 = _> {}\ntrait Marker {}\nfn f(value: Slot) {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let mut func = None;
        let mut marker = None;
        for item in top_mod.children_non_nested(&db) {
            match item {
                ItemKind::Func(item) => func = Some(item),
                ItemKind::Trait(item) => marker = Some(item),
                _ => {}
            }
        }
        let func = func.expect("missing `f` function");
        let marker = marker.expect("missing `Marker` trait");
        let hir_ty = func
            .params(&db)
            .next()
            .and_then(|param| param.hir_ty(&db))
            .expect("missing `value` parameter type");
        let empty = PredicateListId::empty_list(&db);
        let marker_assumption = PredicateListId::new(&db, vec![trait_self_predicate(&db, marker)]);

        let root = |assumptions| {
            let ty = lower_hir_ty(&db, hir_ty, func.scope(), assumptions);
            let placeholders = collect_unique_layout_placeholders_in_order(&db, ty);
            assert_eq!(placeholders.len(), 1);
            structural_hole_id(&db, placeholders[0])
                .expect("expected structural layout hole")
                .root(&db)
        };

        assert_ne!(root(empty), root(marker_assumption));
    }

    #[test]
    fn substitute_layout_placeholders_by_placeholder_reuses_mapped_placeholder() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("layout_holes_test_scope.fe"), "fn f() {}");
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let repeated_hole = mk_hole_ty(&db);
        let replacement = mk_implicit_param_ty(&db, scope, 0, "__replacement");
        let value = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, repeated_hole),
                mk_array_with_len(&db, repeated_hole),
            ],
        );
        let layout_args = FxHashMap::from_iter([(repeated_hole, replacement)]);

        let substituted = substitute_layout_placeholders_by_placeholder(
            &db,
            value,
            &layout_args,
            LayoutPlaceholderPolicy::HolesOnly,
        );

        let fields = substituted.field_types(&db);
        assert_eq!(fields[0].generic_args(&db)[1], replacement);
        assert_eq!(fields[1].generic_args(&db)[1], replacement);
    }

    #[test]
    fn substitute_layout_placeholders_by_placeholder_leaves_unmatched_and_respects_policy() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("layout_holes_test_scope.fe"), "fn f() {}");
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let implicit = mk_implicit_param_ty(&db, scope, 0, "__implicit");
        let hole = mk_hole_ty(&db);
        let unmatched_implicit = mk_implicit_param_ty(&db, scope, 3, "__unmatched_implicit");
        let implicit_replacement = mk_implicit_param_ty(&db, scope, 1, "__implicit_replacement");
        let hole_replacement = mk_implicit_param_ty(&db, scope, 2, "__hole_replacement");
        let value = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, implicit),
                mk_array_with_len(&db, hole),
                mk_array_with_len(&db, unmatched_implicit),
            ],
        );
        let layout_args =
            FxHashMap::from_iter([(implicit, implicit_replacement), (hole, hole_replacement)]);

        let holes_only = substitute_layout_placeholders_by_placeholder(
            &db,
            value,
            &layout_args,
            LayoutPlaceholderPolicy::HolesOnly,
        );
        let holes_and_implicit = substitute_layout_placeholders_by_placeholder(
            &db,
            value,
            &layout_args,
            LayoutPlaceholderPolicy::HolesAndImplicitParams,
        );

        let holes_only_fields = holes_only.field_types(&db);
        assert_eq!(holes_only_fields[0].generic_args(&db)[1], implicit);
        assert_eq!(holes_only_fields[1].generic_args(&db)[1], hole_replacement);
        assert_eq!(
            holes_only_fields[2].generic_args(&db)[1],
            unmatched_implicit
        );
        let holes_and_implicit_fields = holes_and_implicit.field_types(&db);
        assert_eq!(
            holes_and_implicit_fields[0].generic_args(&db)[1],
            implicit_replacement
        );
        assert_eq!(
            holes_and_implicit_fields[1].generic_args(&db)[1],
            hole_replacement
        );
        assert_eq!(
            holes_and_implicit_fields[2].generic_args(&db)[1],
            unmatched_implicit
        );
    }

    #[test]
    fn layout_shape_key_preserves_repeated_placeholder_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("alpha_rename_preserves_repeated_placeholder_identity.fe"),
            "fn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");

        let expected = TyId::tuple_with_elems(
            &db,
            &[
                mk_implicit_param_ty(&db, scope, 0, "__p0"),
                mk_implicit_param_ty(&db, scope, 0, "__p0"),
            ],
        );
        let actual = TyId::tuple_with_elems(
            &db,
            &[
                mk_implicit_param_ty(&db, scope, 1, "__p1"),
                mk_implicit_param_ty(&db, scope, 2, "__p2"),
            ],
        );
        assert_ne!(
            layout_shape_key(&db, expected),
            layout_shape_key(&db, actual)
        );
    }

    #[test]
    fn reanchor_template_holes_changes_identity_and_preserves_sharing() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("reanchor_template_holes_changes_identity.fe"),
            "type A = u256\nfn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let alias = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::TypeAlias(alias) => Some(alias),
                _ => None,
            })
            .expect("missing `A` alias");

        // One template hole occurring twice plus a distinct sibling, embedded
        // as array lengths (tuple elements must be star-kinded). The fourth
        // occurrence describes the shared root with a distinct diagnostic
        // trace and must still re-anchor to the same semantic root.
        let anchor = template_anchor(&db, scope);
        let shared = mk_structural_hole_ty(&db, (anchor, 0));
        let sibling = mk_structural_hole_ty(&db, (anchor, 1));
        let site = LayoutHoleArgSite::GenericArgList(GenericArgListId::none(&db));
        let restamped_shared = mk_structural_hole_with_origin(
            &db,
            StructuralHoleOrigin::ExplicitWildcard { site, arg_idx: 0 },
            LayoutIntroSite::lowering(site, 1),
            (anchor, 0),
        );
        let tuple = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, shared),
                mk_array_with_len(&db, sibling),
                mk_array_with_len(&db, shared),
                mk_array_with_len(&db, restamped_shared),
            ],
        );

        let reanchored = reanchor_template_holes(&db, tuple, HoleAnchor::AliasTemplate(alias));
        assert_ne!(reanchored, tuple);

        let (_, elems) = reanchored.decompose_ty_app(&db);
        let ids: Vec<_> = elems
            .iter()
            .map(|elem| array_len_hole(&db, *elem))
            .collect();
        // Sharing preserved, distinctness preserved, anchors rewritten.
        assert_eq!(ids[0], ids[2]);
        assert_ne!(ids[0], ids[1]);
        assert_ne!(ids[0], ids[3]);
        assert_eq!(ids[0].root(&db), ids[3].root(&db));
        assert!(
            ids.iter()
                .all(|id| id.anchor(&db) == HoleAnchor::AliasTemplate(alias))
        );
        assert_ne!(ids[0].root(&db), ids[1].root(&db));
    }

    #[test]
    fn layout_root_identity_is_separate_from_trace_and_structural_landings() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("layout_root_identity_is_separate_from_trace.fe"),
            "type A = u256\nfn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let alias = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::TypeAlias(alias) => Some(alias),
                _ => None,
            })
            .expect("missing `A` alias");
        let anchor = template_anchor(&db, scope);
        let site = LayoutHoleArgSite::GenericArgList(GenericArgListId::none(&db));
        let origin = StructuralHoleOrigin::ExplicitWildcard { site, arg_idx: 0 };
        let introduced_at = LayoutIntroSite::lowering(site, 0);
        let first = mk_structural_hole_with_origin(&db, origin, introduced_at.clone(), (anchor, 0));
        let second =
            mk_structural_hole_with_origin(&db, origin, introduced_at.clone(), (anchor, 1));
        let first_id = expect_structural_hole(&db, first);
        let second_id = expect_structural_hole(&db, second);

        assert_ne!(first_id.root(&db), second_id.root(&db));
        assert_eq!(first_id.origin(&db), second_id.origin(&db));
        assert_eq!(first_id.trace(&db), second_id.trace(&db));

        let restamped = TyId::const_ty(
            &db,
            ConstTyId::structural_hole(
                &db,
                usize_ty(&db),
                origin,
                LayoutIntroSite::lowering(site, 1),
                first_id.root(&db),
            ),
        );
        let restamped_id = expect_structural_hole(&db, restamped);
        assert_eq!(first_id.root(&db), restamped_id.root(&db));
        assert_ne!(first_id.introduced_at(&db), restamped_id.introduced_at(&db));
        let shared_shape = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, first),
                mk_array_with_len(&db, restamped),
            ],
        );
        let repeated_shape = TyId::tuple_with_elems(
            &db,
            &[mk_array_with_len(&db, first), mk_array_with_len(&db, first)],
        );
        let distinct_shape = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, first),
                mk_array_with_len(&db, second),
            ],
        );
        assert_eq!(
            layout_shape_key(&db, shared_shape),
            layout_shape_key(&db, repeated_shape)
        );
        assert_ne!(
            layout_shape_key(&db, shared_shape),
            layout_shape_key(&db, distinct_shape)
        );

        let first_instance = LayoutInstantiationId::new(
            &db,
            LayoutInstantiationContext::Lowering(anchor),
            LayoutBoundaryIdentity::AliasUse(alias),
            vec![LayoutOccurrenceStep::TypeParam(0)],
        );
        let second_instance = LayoutInstantiationId::new(
            &db,
            LayoutInstantiationContext::Lowering(anchor),
            LayoutBoundaryIdentity::AliasUse(alias),
            vec![LayoutOccurrenceStep::TypeParam(1)],
        );
        let first_landing = LayoutRootId::landing(&db, first_id.root(&db), first_instance);
        assert_eq!(
            first_landing,
            LayoutRootId::landing(&db, first_id.root(&db), first_instance),
        );
        assert_ne!(
            first_landing,
            LayoutRootId::landing(&db, first_id.root(&db), second_instance),
        );
        assert_ne!(
            first_landing,
            LayoutRootId::landing(&db, second_id.root(&db), first_instance),
        );
    }

    #[test]
    fn layout_view_permutation_matching_handles_overlapping_lineages() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("layout_view_permutation_matching.fe"),
            "type A = u256\nfn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let mut scope = None;
        let mut alias = None;
        for item in top_mod.children_non_nested(&db) {
            match item {
                ItemKind::Func(item) => scope = Some(item.scope()),
                ItemKind::TypeAlias(item) => alias = Some(item),
                _ => {}
            }
        }
        let scope = scope.expect("missing `f` function");
        let alias = alias.expect("missing `A` alias");
        let anchor = template_anchor(&db, scope);
        let instance = |idx| {
            LayoutInstantiationId::new(
                &db,
                LayoutInstantiationContext::Lowering(anchor),
                LayoutBoundaryIdentity::AliasUse(alias),
                vec![LayoutOccurrenceStep::TypeParam(idx)],
            )
        };

        // C descends from B and A, while D only descends from A. A greedy
        // C-to-A pairing rejects the valid C-to-B, D-to-A permutation.
        let a = mk_structural_hole_ty(&db, (anchor, 0));
        let b = landed_hole(
            &db,
            expect_structural_hole(&db, a),
            usize_ty(&db),
            instance(0),
        );
        let c = landed_hole(
            &db,
            expect_structural_hole(&db, b),
            usize_ty(&db),
            instance(1),
        );
        let d = landed_hole(
            &db,
            expect_structural_hole(&db, a),
            usize_ty(&db),
            instance(2),
        );
        let ancestor =
            TyId::tuple_with_elems(&db, &[mk_array_with_len(&db, a), mk_array_with_len(&db, b)]);
        let value =
            TyId::tuple_with_elems(&db, &[mk_array_with_len(&db, c), mk_array_with_len(&db, d)]);

        assert!(layout_view_states_are_permutations(&db, value, ancestor));
    }

    #[test]
    fn collect_unique_app_bound_structural_holes_accepts_use_site_holes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("collect_unique_app_bound_accepts_use_site.fe"),
            "fn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");

        let holes = collect_unique_app_bound_structural_holes_in_order(
            &db,
            mk_structural_hole_ty(&db, (template_anchor(&db, scope), 0)),
        );

        assert_eq!(holes.len(), 1);
    }

    #[test]
    #[should_panic(expected = "template-only structural hole escaped into semantic binding")]
    fn collect_unique_app_bound_structural_holes_rejects_alias_template_holes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("collect_unique_app_bound_rejects_alias_template.fe"),
            "type A = u256",
        );
        let (top_mod, _) = db.top_mod(file);
        let alias = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::TypeAlias(alias) => Some(alias),
                _ => None,
            })
            .expect("missing `A` alias");

        let _ = collect_unique_app_bound_structural_holes_in_order(
            &db,
            mk_structural_hole_ty(&db, (HoleAnchor::AliasTemplate(alias), 0)),
        );
    }
}
