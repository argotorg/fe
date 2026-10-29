use common::layout::enum_tag_bits;
use cranelift_entity::EntityRef;
use num_bigint::BigInt;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BinaryOp, FieldIndex, SBlockId, SConst, SExpr, SLocalId, SOperand, SPlace, SStmtKind,
            STerminatorKind, SValueId, SemOrigin, VariantIndex, bool_const, bytes_const, int_const,
        },
        ty::{
            decision_tree::{
                Case, DecisionTree, LeafNode, Projection, ProjectionPath, SwitchNode,
                build_decision_tree, build_pattern_branch_decision_tree,
            },
            normalize::normalize_ty,
            pattern_ir::{ConstructorKind, PatternStore, ValidatedPatId, ValidatedPatKind},
            pattern_types::{
                PatternProjectionStep, apply_pattern_borrow_mode, destructure_pattern_source,
                pattern_match_expected_ty, project_pattern_child_carrier_ty,
                project_pattern_child_source_ty,
            },
            provider::AddressSpace,
            ty_check::PatBindingMode,
            ty_def::{BorrowKind, PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{LitKind, MatchArm, PatId, expr::CompBinOp},
};

use super::body::SmirLowerCtxt;

#[derive(Clone, Copy)]
pub(super) struct PatternCarrierTy<'db>(pub(super) TyId<'db>);

#[derive(Clone, Copy)]
pub(super) struct PatternValue<'db> {
    pub(super) value: SValueId,
    // Runtime/source type threaded through pattern lowering. This intentionally
    // stays separate from validated-pattern match types and final binding types.
    pub(super) carrier_ty: PatternCarrierTy<'db>,
}

/// What a pattern destructures: a value, or a matched place whose bindings
/// take their components where they lie.
#[derive(Clone)]
pub(super) enum PatternSource<'db> {
    Value(PatternValue<'db>),
    /// A place matched by component: a `ref` or `mut` binding borrows its
    /// component's path, and the others read it, so nothing is opened on the
    /// place beyond them. The tests of an `inspected` place, one with no
    /// `ref` or `mut` binding, read its value instead of borrowing it.
    Place {
        place: SPlace<'db>,
        ty: TyId<'db>,
        provider: Option<AddressSpace>,
        inspected: bool,
    },
}

#[derive(Clone)]
struct DecisionTreeProjectionCache<'db> {
    entries: Vec<(ProjectionPath<'db>, PatternValue<'db>)>,
    carrier_tys: Vec<(ProjectionPath<'db>, TyId<'db>)>,
    /// A matched place, and what it holds: read through a read access only
    /// when a test first needs its value, and bound by component.
    place: Option<(SPlace<'db>, Option<AddressSpace>, TyId<'db>)>,
    /// Whether the matched place's tests read its value (`inspected`).
    inspected: bool,
}

fn assigned_pattern_child_carrier_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    pattern_store: &PatternStore<'db>,
    parent_carrier_ty: TyId<'db>,
    child: ValidatedPatId,
    projection: PatternProjectionStep<'db>,
) -> TyId<'db> {
    let (container_ty, parent_mode) = destructure_pattern_source(db, parent_carrier_ty);
    let projected_source = project_pattern_child_source_ty(db, container_ty, projection);
    let assigned_match = pattern_store.node(child).match_ty().raw();
    let assigned_carrier = with_source_carriers(db, projected_source, assigned_match);
    apply_pattern_borrow_mode(db, parent_mode, assigned_carrier)
}

/// `matched`, a pattern's match type, with the carriers `source` holds at the
/// same positions. A match type has none, while the carrier of a projection's
/// shape has one per access component, inside its tuples and sums too:
/// `Option<(mut T, mut T)>` matches as `Option<(T, T)>`.
fn with_source_carriers<'db>(
    db: &'db dyn HirAnalysisDb,
    source: TyId<'db>,
    matched: TyId<'db>,
) -> TyId<'db> {
    // A value of an opaque shape is held by the shape's carrier.
    if matched.as_shape_value(db).is_some() {
        return source;
    }
    let (source_base, source_args) = source.decompose_ty_app(db);
    if let Some((_, inner)) = source.as_capability(db)
        && matched.as_capability(db).is_none()
    {
        return TyId::app(db, source_base, with_source_carriers(db, inner, matched));
    }
    let (matched_base, matched_args) = matched.decompose_ty_app(db);
    if source_base != matched_base || source_args.len() != matched_args.len() {
        return matched;
    }
    source_args
        .iter()
        .zip(matched_args)
        .fold(matched_base, |ty, (&source, &matched)| {
            TyId::app(db, ty, with_source_carriers(db, source, matched))
        })
}

impl<'db> DecisionTreeProjectionCache<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        pattern_store: &PatternStore<'db>,
        roots: &[ValidatedPatId],
        source: PatternSource<'db>,
    ) -> Self {
        let (entries, root_ty, place, inspected) = match source {
            PatternSource::Value(root) => (
                vec![(ProjectionPath::default(), root)],
                root.carrier_ty.0,
                None,
                false,
            ),
            PatternSource::Place {
                place,
                ty,
                provider,
                inspected,
            } => (
                Vec::new(),
                if inspected {
                    ty
                } else {
                    TyId::borrow_ref_of(db, ty)
                },
                Some((place, provider, ty)),
                inspected,
            ),
        };
        let mut cache = Self {
            entries,
            carrier_tys: vec![(ProjectionPath::default(), root_ty)],
            place,
            inspected,
        };
        for root in roots {
            cache.collect_pattern_carrier_tys(
                db,
                pattern_store,
                *root,
                root_ty,
                &ProjectionPath::default(),
            );
        }
        cache
    }

    fn collect_pattern_carrier_tys(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        pattern_store: &PatternStore<'db>,
        pat: ValidatedPatId,
        carrier_ty: TyId<'db>,
        path: &ProjectionPath<'db>,
    ) {
        match pattern_store.node(pat).kind() {
            ValidatedPatKind::Wildcard { .. } => {}
            ValidatedPatKind::Or(pats) => {
                for pat in pats {
                    self.collect_pattern_carrier_tys(db, pattern_store, *pat, carrier_ty, path);
                }
            }
            ValidatedPatKind::Constructor { ctor, fields } => {
                for (field_idx, field) in fields.iter().copied().enumerate() {
                    let (projection, pattern_projection) = match ctor {
                        ConstructorKind::Variant(variant, enum_ty) => (
                            Projection::VariantField {
                                variant: *variant,
                                enum_ty: *enum_ty,
                                field_idx,
                            },
                            PatternProjectionStep::VariantField {
                                variant: *variant,
                                field_idx,
                            },
                        ),
                        ConstructorKind::Type(_) => (
                            Projection::Field(field_idx),
                            PatternProjectionStep::Field(field_idx),
                        ),
                        ConstructorKind::Literal(..) => continue,
                    };
                    let child_ty = assigned_pattern_child_carrier_ty(
                        db,
                        pattern_store,
                        carrier_ty,
                        field,
                        pattern_projection,
                    );
                    let mut child_path = path.clone();
                    child_path.push(projection);
                    if let Some((_, existing)) = self
                        .carrier_tys
                        .iter()
                        .find(|(existing_path, _)| existing_path == &child_path)
                    {
                        assert_eq!(
                            *existing, child_ty,
                            "one decision-tree projection must have one carrier type"
                        );
                    } else {
                        self.carrier_tys.push((child_path.clone(), child_ty));
                    }
                    self.collect_pattern_carrier_tys(
                        db,
                        pattern_store,
                        field,
                        child_ty,
                        &child_path,
                    );
                }
            }
        }
    }

    fn get(&self, path: &ProjectionPath<'db>) -> Option<PatternValue<'db>> {
        self.entries
            .iter()
            .find_map(|(cached_path, value)| (cached_path == path).then_some(*value))
    }

    fn insert(&mut self, path: ProjectionPath<'db>, value: PatternValue<'db>) {
        if self.get(&path).is_none() {
            self.entries.push((path, value));
        }
    }

    fn carrier_ty(&self, path: &ProjectionPath<'db>) -> Option<TyId<'db>> {
        self.carrier_tys
            .iter()
            .find_map(|(candidate, ty)| (candidate == path).then_some(*ty))
    }

    fn longest_prefix(
        &self,
        path: &ProjectionPath<'db>,
    ) -> (ProjectionPath<'db>, PatternValue<'db>) {
        self.entries
            .iter()
            .filter(|(cached_path, _)| cached_path.is_prefix_of(path))
            .max_by_key(|(cached_path, _)| cached_path.len())
            .map(|(cached_path, value)| (cached_path.clone(), *value))
            .expect("decision-tree projection cache always contains the root occurrence")
    }
}

/// What a decision-tree leaf does once a pattern row is known to match.
#[derive(Clone, Copy)]
enum DecisionTreeTarget<'a> {
    /// `match` expression: lower the arm body, assign the result, jump to the
    /// join block.
    MatchArms {
        result: SLocalId,
        join_bb: SBlockId,
        arms: &'a [MatchArm],
    },
    /// Refutable single-pattern test (`if let` / `while let`): row 0 is the
    /// pattern (bind, then jump to `then_bb`), row 1 is the synthetic wildcard
    /// fallback (jump to `else_bb`).
    Branch {
        then_bb: SBlockId,
        else_bb: SBlockId,
        pat: PatId,
    },
}

impl DecisionTreeTarget<'_> {
    /// Where a test the tree makes on the way to arm `arm` is reported: that
    /// arm's pattern.
    fn test_origin<'db>(&self, arm: Option<usize>) -> SemOrigin<'db> {
        match self {
            Self::MatchArms { arms, .. } => arm
                .and_then(|arm| arms.get(arm))
                .map_or(SemOrigin::Synthetic, |arm| SemOrigin::Pat(arm.pat)),
            Self::Branch { pat, .. } => SemOrigin::Pat(*pat),
        }
    }
}

/// The first arm `tree` can reach.
fn first_arm(tree: &DecisionTree<'_>) -> Option<usize> {
    match tree {
        DecisionTree::Unreachable => None,
        DecisionTree::Leaf(leaf) => Some(leaf.arm_index),
        DecisionTree::Switch(switch) => switch_first_arm(switch),
    }
}

fn switch_first_arm(switch: &SwitchNode<'_>) -> Option<usize> {
    switch
        .arms
        .iter()
        .filter_map(|(_, tree)| first_arm(tree))
        .min()
}

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    fn validated_pattern_is_irrefutable(&self, pat: ValidatedPatId) -> bool {
        self.typed_body.pattern_store().is_irrefutable(self.db, pat)
    }

    pub(super) fn pattern_is_irrefutable(&self, pat: PatId) -> bool {
        self.typed_body
            .pattern_root(pat)
            .is_none_or(|root| self.validated_pattern_is_irrefutable(root))
    }

    pub(super) fn bind_pattern(&mut self, pat: PatId, source: PatternSource<'db>) {
        let Some(root) = self.typed_body.pattern_root(pat) else {
            return;
        };
        match source {
            PatternSource::Value(value) => self.bind_validated_pattern(root, value),
            PatternSource::Place {
                place,
                ty,
                provider,
                ..
            } => self.bind_validated_pattern_place(root, place, provider, ty),
        }
    }

    /// The source of a pattern matched against `value`.
    pub(super) fn value_pattern_source(&self, value: SValueId) -> PatternSource<'db> {
        PatternSource::Value(self.owned_pattern_value(value, self.locals[value.index()].ty))
    }

    /// Test `pat` against `value`, jumping to `then_bb` with the pattern's
    /// bindings assigned, or to `else_bb` if the pattern doesn't match.
    pub(super) fn lower_pattern_branch(
        &mut self,
        pat: PatId,
        source: PatternSource<'db>,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) {
        let Some(root) = self.typed_body.pattern_root(pat) else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
            return;
        };
        let pattern_store = self.typed_body.pattern_store();
        let tree = build_pattern_branch_decision_tree(self.db, pattern_store, root);
        let mut projections =
            DecisionTreeProjectionCache::new(self.db, pattern_store, &[root], source);
        self.lower_decision_tree(
            &tree,
            &mut projections,
            DecisionTreeTarget::Branch {
                then_bb,
                else_bb,
                pat,
            },
        );
    }

    fn owned_pattern_value(&self, value: SValueId, carrier_ty: TyId<'db>) -> PatternValue<'db> {
        PatternValue {
            value,
            carrier_ty: PatternCarrierTy(carrier_ty),
        }
    }

    fn project_pattern_field(
        &mut self,
        origin: SemOrigin<'db>,
        base: PatternValue<'db>,
        field_idx: usize,
        assigned_ty: Option<TyId<'db>>,
    ) -> PatternValue<'db> {
        let ty = assigned_ty.unwrap_or_else(|| {
            project_pattern_child_carrier_ty(
                self.db,
                base.carrier_ty.0,
                PatternProjectionStep::Field(field_idx),
            )
        });
        let value = self.emit_expr_with_origin(
            origin,
            ty,
            SExpr::Field {
                base: SOperand::synthetic(base.value),
                field: FieldIndex(field_idx as u16),
            },
        );
        PatternValue {
            value,
            carrier_ty: PatternCarrierTy(ty),
        }
    }

    fn project_pattern_variant_field(
        &mut self,
        origin: SemOrigin<'db>,
        base: PatternValue<'db>,
        variant: crate::hir_def::EnumVariant<'db>,
        field_idx: usize,
        assigned_ty: Option<TyId<'db>>,
    ) -> PatternValue<'db> {
        let ty = assigned_ty.unwrap_or_else(|| {
            project_pattern_child_carrier_ty(
                self.db,
                base.carrier_ty.0,
                PatternProjectionStep::VariantField { variant, field_idx },
            )
        });
        let value = self.emit_expr_with_origin(
            origin,
            ty,
            SExpr::ExtractEnumField {
                value: SOperand::synthetic(base.value),
                variant: VariantIndex(variant.idx),
                field: FieldIndex(field_idx as u16),
            },
        );
        PatternValue {
            value,
            carrier_ty: PatternCarrierTy(ty),
        }
    }

    fn debug_assert_pattern_binding_ty_matches(
        &self,
        dst: crate::analysis::semantic::SLocalId,
        src: PatternValue<'db>,
    ) {
        let scope = self.body.scope();
        let src_ty = normalize_ty(self.db, src.carrier_ty.0, scope, self.assumptions);
        let dst_ty = normalize_ty(
            self.db,
            self.locals[dst.index()].ty,
            scope,
            self.assumptions,
        );
        // A by-value binding of an access's part reads through its carrier.
        let src_ty = match src_ty.as_capability(self.db) {
            Some((_, target)) if dst_ty.as_capability(self.db).is_none() => target,
            _ => src_ty,
        };
        debug_assert_eq!(
            src_ty,
            dst_ty,
            "pattern binding type drift: owner={:?} binding={:?} src_local={:?} src_local_source={:?} src_raw={:?} src_data={:?} dst_raw={:?} dst_data={:?} src={} dst={}",
            self.template_owner,
            self.locals[dst.index()].source,
            src.value,
            self.locals[src.value.index()].source,
            src.carrier_ty.0,
            src.carrier_ty.0.data(self.db),
            self.locals[dst.index()].ty,
            self.locals[dst.index()].ty.data(self.db),
            src_ty.pretty_print(self.db),
            dst_ty.pretty_print(self.db),
        );
    }

    fn validated_pattern_child_carrier_ty(
        &self,
        parent: PatternValue<'db>,
        child: ValidatedPatId,
        projection: PatternProjectionStep<'db>,
    ) -> TyId<'db> {
        assigned_pattern_child_carrier_ty(
            self.db,
            self.typed_body.pattern_store(),
            parent.carrier_ty.0,
            child,
            projection,
        )
    }

    /// Binds `pat`'s bindings to the parts of `value` they name. A part no
    /// binding names is not projected, so it stays in `value`: projecting
    /// would move it out, as a value's projection does.
    fn bind_validated_pattern(&mut self, pat: ValidatedPatId, value: PatternValue<'db>) {
        let store = self.typed_body.pattern_store();
        let kind = store.node(pat).kind().clone();
        match kind {
            ValidatedPatKind::Wildcard {
                binding: Some(binding),
            } => self.bind_value(binding.representative_pat, value),
            ValidatedPatKind::Wildcard { binding: None } => {}
            ValidatedPatKind::Constructor { ctor, fields } => match ctor {
                ConstructorKind::Variant(variant, _) => {
                    for (idx, field_pat) in fields
                        .into_iter()
                        .enumerate()
                        .filter(|(_, field)| store.binds(*field))
                    {
                        let assigned_ty = self.validated_pattern_child_carrier_ty(
                            value,
                            field_pat,
                            PatternProjectionStep::VariantField {
                                variant,
                                field_idx: idx,
                            },
                        );
                        let field = self.project_pattern_variant_field(
                            SemOrigin::Synthetic,
                            value,
                            variant,
                            idx,
                            Some(assigned_ty),
                        );
                        self.bind_validated_pattern(field_pat, field);
                    }
                }
                ConstructorKind::Type(_) => {
                    for (idx, field_pat) in fields
                        .into_iter()
                        .enumerate()
                        .filter(|(_, field)| store.binds(*field))
                    {
                        let assigned_ty = self.validated_pattern_child_carrier_ty(
                            value,
                            field_pat,
                            PatternProjectionStep::Field(idx),
                        );
                        let field = self.project_pattern_field(
                            SemOrigin::Synthetic,
                            value,
                            idx,
                            Some(assigned_ty),
                        );
                        self.bind_validated_pattern(field_pat, field);
                    }
                }
                ConstructorKind::Literal(..) => {}
            },
            ValidatedPatKind::Or(pats) => {
                if let Some(first) = pats.first() {
                    self.bind_validated_pattern(*first, value);
                }
            }
        }
    }

    /// Binds `pat`, a binding's pattern, to `value`: the value, or for a
    /// `ref` or `mut` binding the access it is or reads through.
    fn bind_value(&mut self, pat: PatId, value: PatternValue<'db>) {
        let Some(binding) = self.typed_body.pat_binding(pat) else {
            return;
        };
        let dst = self.alloc_binding_local(binding);
        let dst_ty = self.locals[dst.index()].ty;
        let by_borrow = matches!(
            self.typed_body.pat_binding_mode(pat),
            Some(PatBindingMode::Access { .. })
        );
        let source_matches_dst = {
            let scope = self.body.scope();
            normalize_ty(self.db, value.carrier_ty.0, scope, self.assumptions)
                == normalize_ty(self.db, dst_ty, scope, self.assumptions)
        };
        let needs_borrow_read = by_borrow && !source_matches_dst;
        let binding_value = if needs_borrow_read {
            PatternValue {
                value: value.value,
                carrier_ty: PatternCarrierTy(dst_ty),
            }
        } else {
            value
        };
        self.debug_assert_pattern_binding_ty_matches(dst, binding_value);
        self.push_stmt(
            SemOrigin::Pat(pat),
            SStmtKind::Assign {
                dst,
                expr: if needs_borrow_read {
                    SExpr::ReadPlace {
                        place: SPlace::new(value.value),
                    }
                } else {
                    SExpr::UseValue(SOperand::inherited(value.value))
                },
            },
        );
    }

    /// Binds `pat`'s bindings to the components of `place`, which holds a
    /// `ty`, where they lie. A component that holds an access, such as an
    /// access part of a product or of a projection's result, is read and
    /// matched as a value: its bindings take what the access reaches.
    fn bind_validated_pattern_place(
        &mut self,
        pat: ValidatedPatId,
        place: SPlace<'db>,
        provider: Option<AddressSpace>,
        ty: TyId<'db>,
    ) {
        let store = self.typed_body.pattern_store();
        if ty.as_capability(self.db).is_some() {
            if store.binds(pat) {
                let carrier = self.emit_expr(ty, SExpr::ReadPlace { place });
                self.bind_validated_pattern(pat, self.owned_pattern_value(carrier, ty));
            }
            return;
        }
        let kind = store.node(pat).kind().clone();
        match kind {
            ValidatedPatKind::Wildcard {
                binding: Some(binding),
            } => self.bind_place_component(binding.representative_pat, place, provider),
            ValidatedPatKind::Wildcard { binding: None }
            | ValidatedPatKind::Constructor {
                ctor: ConstructorKind::Literal(..),
                ..
            } => {}
            ValidatedPatKind::Constructor { ctor, fields } => {
                for (idx, field_pat) in fields.into_iter().enumerate() {
                    let (projection, step) = match ctor {
                        ConstructorKind::Variant(variant, enum_ty) => (
                            crate::projection::Projection::VariantField {
                                variant: VariantIndex(variant.idx),
                                enum_ty,
                                field_idx: idx,
                            },
                            PatternProjectionStep::VariantField {
                                variant,
                                field_idx: idx,
                            },
                        ),
                        _ => (
                            crate::projection::Projection::Field(idx),
                            PatternProjectionStep::Field(idx),
                        ),
                    };
                    let mut component = place.clone();
                    component.path.push(projection);
                    let field_ty = project_pattern_child_source_ty(self.db, ty, step);
                    self.bind_validated_pattern_place(field_pat, component, provider, field_ty);
                }
            }
            ValidatedPatKind::Or(pats) => {
                if let Some(first) = pats.first() {
                    self.bind_validated_pattern_place(*first, place, provider, ty);
                }
            }
        }
    }

    /// Binds `pat` to the component `place`: its access for a `ref` or `mut`
    /// binding, or its value.
    fn bind_place_component(
        &mut self,
        pat: PatId,
        place: SPlace<'db>,
        provider: Option<AddressSpace>,
    ) {
        let Some(binding) = self.typed_body.pat_binding(pat) else {
            return;
        };
        let dst = self.alloc_binding_local(binding);
        let expr = match self.typed_body.pat_binding_mode(pat) {
            Some(PatBindingMode::Access { kind, .. }) => SExpr::Borrow {
                place,
                kind,
                provider,
            },
            _ => SExpr::ReadPlace { place },
        };
        self.push_stmt(SemOrigin::Pat(pat), SStmtKind::Assign { dst, expr });
    }

    pub(super) fn lower_match_expr_with_decision_tree(
        &mut self,
        source: PatternSource<'db>,
        result: crate::analysis::semantic::SLocalId,
        join_bb: SBlockId,
        arms: &[MatchArm],
    ) -> SValueId {
        let roots = arms
            .iter()
            .map(|arm| self.typed_body.pattern_root(arm.pat))
            .collect::<Option<Vec<_>>>()
            .unwrap_or_else(|| panic!("decision-tree match lowering requires validated patterns"));
        let tree = build_decision_tree(self.db, self.typed_body.pattern_store(), &roots);
        let mut projections = DecisionTreeProjectionCache::new(
            self.db,
            self.typed_body.pattern_store(),
            &roots,
            source,
        );
        let target = DecisionTreeTarget::MatchArms {
            result,
            join_bb,
            arms,
        };
        let join_reachable = self.lower_decision_tree(&tree, &mut projections, target);
        self.switch_to(join_bb);
        if join_reachable {
            self.hold_defined(result);
        } else {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        result
    }

    fn lower_decision_tree(
        &mut self,
        tree: &DecisionTree<'db>,
        projections: &mut DecisionTreeProjectionCache<'db>,
        target: DecisionTreeTarget<'_>,
    ) -> bool {
        match tree {
            DecisionTree::Unreachable => match target {
                DecisionTreeTarget::MatchArms { .. } => {
                    // Semantic IR has no dedicated unreachable terminator. Keep the
                    // impossible path terminal and fail safely if malformed runtime
                    // input ever reaches it.
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Assert { message: None },
                    );
                    false
                }
                DecisionTreeTarget::Branch { else_bb, .. } => {
                    self.set_synthetic_terminator(self.current, STerminatorKind::Goto(else_bb));
                    true
                }
            },
            DecisionTree::Leaf(leaf) => match target {
                // An arm's scope holds what it binds.
                DecisionTreeTarget::MatchArms {
                    result,
                    join_bb,
                    arms,
                } => {
                    self.push_drop_scope(true);
                    self.bind_decision_tree_leaf(leaf, projections);
                    let arm = &arms[leaf.arm_index];
                    let arm_value = self.lower_expr(arm.body);
                    let falls = !self.is_terminated(self.current);
                    if falls {
                        // The arm's value passes to the join, which takes it.
                        let arm_value = self.take_operand(arm.body, arm_value);
                        self.assign_branch_result(result, SOperand::expr(arm_value, arm.body));
                    }
                    self.pop_drop_scope(None);
                    if falls {
                        self.set_terminator(
                            self.current,
                            SemOrigin::Pat(arm.pat),
                            STerminatorKind::Goto(join_bb),
                        );
                    }
                    falls
                }
                DecisionTreeTarget::Branch {
                    then_bb, else_bb, ..
                } => {
                    if leaf.arm_index == 0 {
                        self.bind_decision_tree_leaf(leaf, projections);
                        let origin = target.test_origin(Some(0));
                        self.set_terminator(self.current, origin, STerminatorKind::Goto(then_bb));
                    } else {
                        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(else_bb));
                    }
                    true
                }
            },
            DecisionTree::Switch(switch) => {
                self.lower_decision_tree_switch(switch, projections, target)
            }
        }
    }

    fn lower_decision_tree_switch(
        &mut self,
        switch: &SwitchNode<'db>,
        projections: &mut DecisionTreeProjectionCache<'db>,
        target: DecisionTreeTarget<'_>,
    ) -> bool {
        let origin = target.test_origin(switch_first_arm(switch));
        let occurrence = self.project_decision_tree_path(origin, projections, &switch.occurrence);
        let case_blocks = switch
            .arms
            .iter()
            .map(|(case, tree)| (case, tree, self.new_block()))
            .collect::<Vec<_>>();

        let mut enum_ty = None;
        let mut enum_cases = Vec::new();
        let mut enum_default = None;
        let mut is_enum_switch = true;
        for (case, _, case_bb) in &case_blocks {
            match case {
                Case::Constructor(ConstructorKind::Variant(variant, case_enum_ty)) => {
                    if enum_ty.is_some_and(|ty| ty != *case_enum_ty) {
                        is_enum_switch = false;
                        break;
                    }
                    enum_ty = Some(*case_enum_ty);
                    enum_cases.push((VariantIndex(variant.idx), *case_bb));
                }
                Case::Default => enum_default = Some(*case_bb),
                Case::Constructor(_) => {
                    is_enum_switch = false;
                    break;
                }
            }
        }

        if is_enum_switch && enum_ty.is_some() {
            // The switch reads the scrutinee's representation: a sum yield
            // shape's payload is a carrier.
            let enum_ty = self.projectable_place_ty(self.locals[occurrence.value.index()].ty);
            self.set_terminator(
                self.current,
                origin,
                STerminatorKind::MatchEnum {
                    value: SOperand::synthetic(occurrence.value),
                    enum_ty,
                    cases: enum_cases.into_boxed_slice(),
                    default: enum_default,
                },
            );
        } else {
            let mut dispatch_bb = self.current;
            for (idx, (case, case_tree, case_bb)) in case_blocks.iter().enumerate() {
                if idx > 0 {
                    self.switch_to(dispatch_bb);
                }
                let is_last = idx + 1 == case_blocks.len();
                match case {
                    Case::Default | Case::Constructor(ConstructorKind::Type(_)) => {
                        self.set_synthetic_terminator(
                            self.current,
                            STerminatorKind::Goto(*case_bb),
                        );
                        break;
                    }
                    Case::Constructor(ctor) if is_last => {
                        let _ = ctor;
                        self.set_synthetic_terminator(
                            self.current,
                            STerminatorKind::Goto(*case_bb),
                        );
                        break;
                    }
                    Case::Constructor(ctor) => {
                        let test = match ctor {
                            ConstructorKind::Literal(lit, ty) => {
                                let rhs = self.literal_pattern_value(*ty, *lit);
                                SExpr::Binary {
                                    op: BinaryOp::Comp(CompBinOp::Eq),
                                    lhs: SOperand::synthetic(occurrence.value),
                                    rhs: SOperand::synthetic(rhs),
                                }
                            }
                            ConstructorKind::Variant(variant, _) => SExpr::IsEnumVariant {
                                value: SOperand::synthetic(occurrence.value),
                                variant: VariantIndex(variant.idx),
                            },
                            ConstructorKind::Type(_) => unreachable!(),
                        };
                        let next_bb = self.new_block();
                        let origin = target.test_origin(first_arm(case_tree));
                        let cond = self.emit_expr_with_origin(origin, TyId::bool(self.db), test);
                        self.set_terminator(
                            self.current,
                            origin,
                            STerminatorKind::Branch {
                                cond: SOperand::synthetic(cond),
                                then_bb: *case_bb,
                                else_bb: next_bb,
                            },
                        );
                        dispatch_bb = next_bb;
                    }
                }
            }
        }

        let mut join_reachable = false;
        for (_, subtree, block) in case_blocks {
            self.switch_to(block);
            let mut subtree_projections = projections.clone();
            join_reachable |= self.lower_decision_tree(subtree, &mut subtree_projections, target);
        }
        join_reachable
    }

    fn bind_decision_tree_leaf(
        &mut self,
        leaf: &LeafNode<'db>,
        projections: &mut DecisionTreeProjectionCache<'db>,
    ) {
        if let Some((place, provider, place_ty)) = projections.place.clone() {
            for (binding_ref, path) in &leaf.bindings {
                let pat = binding_ref.representative_pat;
                // A binding at or below a component that holds an access
                // takes what the access reaches from the tests' reading of
                // the place, as a value pattern does; the place owns no part
                // of it.
                let mut ty = place_ty;
                let carried = projections.inspected
                    && path.iter().any(|projection| {
                        let step = match projection {
                            Projection::Field(field) => PatternProjectionStep::Field(*field),
                            Projection::VariantField {
                                variant, field_idx, ..
                            } => PatternProjectionStep::VariantField {
                                variant: *variant,
                                field_idx: *field_idx,
                            },
                            _ => unreachable!("a pattern binds through fields and variant fields"),
                        };
                        ty = project_pattern_child_source_ty(self.db, ty, step);
                        ty.as_capability(self.db).is_some()
                    });
                if carried {
                    let value =
                        self.project_decision_tree_path(SemOrigin::Pat(pat), projections, path);
                    self.bind_value(pat, value);
                } else {
                    let mut component = place.clone();
                    for projection in path.iter() {
                        component.path.push(semantic_projection(projection));
                    }
                    self.bind_place_component(pat, component, provider);
                }
            }
            return;
        }
        for (binding_ref, path) in &leaf.bindings {
            if let Some(binding) = self.typed_body.pat_binding(binding_ref.representative_pat) {
                let dst = self.alloc_binding_local(binding);
                let origin = SemOrigin::Pat(binding_ref.representative_pat);
                let src = self.project_decision_tree_path(origin, projections, path);
                self.debug_assert_pattern_binding_ty_matches(dst, src);
                self.push_stmt(
                    origin,
                    SStmtKind::Assign {
                        dst,
                        expr: SExpr::UseValue(SOperand::inherited(src.value)),
                    },
                );
            }
        }
    }

    /// The value at `path` of the scrutinee, read where `origin` reports.
    fn project_decision_tree_path(
        &mut self,
        origin: SemOrigin<'db>,
        projections: &mut DecisionTreeProjectionCache<'db>,
        path: &ProjectionPath<'db>,
    ) -> PatternValue<'db> {
        if let Some(value) = projections.get(path) {
            return value;
        }
        // A test reads a matched place through a read access of it, which
        // its last test closes, or reads an inspected place's value.
        if projections.entries.is_empty()
            && let Some((place, provider, _)) = projections.place.clone()
        {
            let root_path = ProjectionPath::default();
            let carrier_ty = projections
                .carrier_ty(&root_path)
                .expect("a matched place has a root carrier type");
            let read = if projections.inspected {
                SExpr::InspectPlace { place }
            } else {
                SExpr::Borrow {
                    place,
                    kind: BorrowKind::Ref,
                    provider,
                }
            };
            let value = self.emit_expr_with_origin(origin, carrier_ty, read);
            projections.insert(
                root_path,
                PatternValue {
                    value,
                    carrier_ty: PatternCarrierTy(carrier_ty),
                },
            );
        }

        let (mut current_path, mut value) = projections.longest_prefix(path);
        for projection in path
            .strip_prefix(&current_path)
            .expect("longest cached prefix should be a prefix")
            .iter()
        {
            current_path.push(projection.clone());
            value = self.project_decision_tree_value(
                origin,
                value,
                projection,
                projections.carrier_ty(&current_path),
            );
            projections.insert(current_path.clone(), value);
        }
        value
    }

    fn project_decision_tree_value(
        &mut self,
        origin: SemOrigin<'db>,
        base: PatternValue<'db>,
        projection: &Projection<'db>,
        assigned_ty: Option<TyId<'db>>,
    ) -> PatternValue<'db> {
        match projection {
            Projection::Field(field) => {
                self.project_pattern_field(origin, base, *field, assigned_ty)
            }
            Projection::VariantField {
                variant, field_idx, ..
            } => {
                self.project_pattern_variant_field(origin, base, *variant, *field_idx, assigned_ty)
            }
            Projection::Discriminant => {
                let ty = enum_tag_ty(
                    self.db,
                    pattern_match_expected_ty(self.db, base.carrier_ty.0),
                );
                let value = self.emit_expr_with_origin(
                    origin,
                    ty,
                    SExpr::GetEnumTag {
                        value: SOperand::synthetic(base.value),
                    },
                );
                PatternValue {
                    value,
                    carrier_ty: PatternCarrierTy(ty),
                }
            }
            Projection::Deref => {
                panic!("decision-tree lowering does not support deref projections yet")
            }
            Projection::Index(_) | Projection::Entry(_) => {
                panic!("decision-tree lowering does not support index projections yet")
            }
        }
    }

    fn literal_pattern_value(&mut self, ty: TyId<'db>, lit: LitKind<'db>) -> SValueId {
        let value = match lit {
            LitKind::Int(int_id) => {
                int_const(self.db, ty, BigInt::from(int_id.data(self.db).clone()))
            }
            LitKind::String(string_id) => {
                let mut bytes = string_id.data(self.db).as_bytes().to_vec();
                if let Some(capacity) = self.fixed_string_capacity_bytes(ty)
                    && bytes.len() < capacity
                {
                    let mut padded = vec![0u8; capacity - bytes.len()];
                    padded.extend(bytes);
                    bytes = padded;
                }
                bytes_const(self.db, ty, bytes)
            }
            LitKind::Bool(value) => bool_const(self.db, value),
        };
        self.emit_expr(
            ty,
            SExpr::Const(SConst::from_trusted_source(self.db, value)),
        )
    }
}

pub(crate) fn enum_tag_ty<'db>(db: &'db dyn HirAnalysisDb, enum_ty: TyId<'db>) -> TyId<'db> {
    let variant_count = enum_ty
        .as_enum(db)
        .map_or(0, |enum_| enum_.len_variants(db));
    let prim = match enum_tag_bits(variant_count) {
        8 => PrimTy::U8,
        16 => PrimTy::U16,
        32 => PrimTy::U32,
        64 => PrimTy::U64,
        _ => unreachable!("enum tag width must be a primitive integer width"),
    };
    TyId::new(db, TyData::TyBase(TyBase::Prim(prim)))
}

/// A decision-tree path step as a place's projection.
fn semantic_projection<'db>(
    projection: &Projection<'db>,
) -> crate::projection::Projection<TyId<'db>, VariantIndex, SLocalId> {
    match projection {
        Projection::Field(field) => crate::projection::Projection::Field(*field),
        Projection::VariantField {
            variant,
            enum_ty,
            field_idx,
        } => crate::projection::Projection::VariantField {
            variant: VariantIndex(variant.idx),
            enum_ty: *enum_ty,
            field_idx: *field_idx,
        },
        Projection::Discriminant => crate::projection::Projection::Discriminant,
        Projection::Index(_) | Projection::Entry(_) | Projection::Deref => {
            unreachable!("a pattern binds through fields and variant fields")
        }
    }
}
