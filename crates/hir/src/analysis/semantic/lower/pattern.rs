use common::layout::enum_tag_bits;
use cranelift_entity::EntityRef;
use num_bigint::BigInt;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            FieldIndex, SBlockId, SConst, SExpr, SLocalId, SOperand, SPlace, SStmtKind,
            STerminatorKind, SValueId, VariantIndex, bool_const, bytes_const, int_const,
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
            ty_check::PatBindingMode,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        LitKind, MatchArm, PatId,
        expr::{BinOp, CompBinOp},
    },
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

#[derive(Clone)]
struct DecisionTreeProjectionCache<'db> {
    entries: Vec<(ProjectionPath<'db>, PatternValue<'db>)>,
    carrier_tys: Vec<(ProjectionPath<'db>, TyId<'db>)>,
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
    let (_, child_mode) = destructure_pattern_source(db, projected_source);
    let assigned_match = pattern_store.node(child).match_ty().raw();
    let assigned_carrier = apply_pattern_borrow_mode(db, child_mode, assigned_match);
    apply_pattern_borrow_mode(db, parent_mode, assigned_carrier)
}

impl<'db> DecisionTreeProjectionCache<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        pattern_store: &PatternStore<'db>,
        roots: &[ValidatedPatId],
        root_value: PatternValue<'db>,
    ) -> Self {
        let mut cache = Self {
            entries: vec![(ProjectionPath::default(), root_value)],
            carrier_tys: vec![(ProjectionPath::default(), root_value.carrier_ty.0)],
        };
        for root in roots {
            cache.collect_pattern_carrier_tys(
                db,
                pattern_store,
                *root,
                root_value.carrier_ty.0,
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
    },
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

    pub(super) fn bind_pattern(&mut self, pat: PatId, value: SValueId) {
        if let Some(root) = self.typed_body.pattern_root(pat) {
            let value = self.owned_pattern_value(value, self.locals[value.index()].ty);
            self.bind_validated_pattern(root, value);
        }
    }

    /// Test `pat` against `value`, jumping to `then_bb` with the pattern's
    /// bindings assigned, or to `else_bb` if the pattern doesn't match.
    pub(super) fn lower_pattern_branch(
        &mut self,
        pat: PatId,
        value: SValueId,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) {
        let Some(root) = self.typed_body.pattern_root(pat) else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
            return;
        };
        let value = self.owned_pattern_value(value, self.locals[value.index()].ty);
        let pattern_store = self.typed_body.pattern_store();
        let tree = build_pattern_branch_decision_tree(self.db, pattern_store, root);
        let mut projections =
            DecisionTreeProjectionCache::new(self.db, pattern_store, &[root], value);
        self.lower_decision_tree(
            &tree,
            &mut projections,
            DecisionTreeTarget::Branch { then_bb, else_bb },
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
        let value = self.emit_expr(
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
        let value = self.emit_expr(
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

    fn bind_validated_pattern(&mut self, pat: ValidatedPatId, value: PatternValue<'db>) {
        let kind = self.typed_body.pattern_store().node(pat).kind().clone();
        match kind {
            ValidatedPatKind::Wildcard {
                binding: Some(binding),
            } => {
                if let Some(local_binding) = self.typed_body.pat_binding(binding.representative_pat)
                {
                    let dst = self.alloc_binding_local(local_binding);
                    let dst_ty = self.locals[dst.index()].ty;
                    let by_borrow = self.typed_body.pat_binding_mode(binding.representative_pat)
                        == Some(PatBindingMode::ByBorrow);
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
                    self.push_synthetic_stmt(SStmtKind::Assign {
                        dst,
                        expr: if needs_borrow_read {
                            SExpr::ReadPlace {
                                place: SPlace::new(value.value),
                            }
                        } else {
                            SExpr::UseValue(SOperand::synthetic(value.value))
                        },
                    });
                }
            }
            ValidatedPatKind::Wildcard { binding: None } => {}
            ValidatedPatKind::Constructor { ctor, fields } => match ctor {
                ConstructorKind::Variant(variant, _) => {
                    for (idx, field_pat) in fields.into_iter().enumerate() {
                        let assigned_ty = self.validated_pattern_child_carrier_ty(
                            value,
                            field_pat,
                            PatternProjectionStep::VariantField {
                                variant,
                                field_idx: idx,
                            },
                        );
                        let field = self.project_pattern_variant_field(
                            value,
                            variant,
                            idx,
                            Some(assigned_ty),
                        );
                        self.bind_validated_pattern(field_pat, field);
                    }
                }
                ConstructorKind::Type(_) => {
                    for (idx, field_pat) in fields.into_iter().enumerate() {
                        let assigned_ty = self.validated_pattern_child_carrier_ty(
                            value,
                            field_pat,
                            PatternProjectionStep::Field(idx),
                        );
                        let field = self.project_pattern_field(value, idx, Some(assigned_ty));
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

    pub(super) fn lower_match_expr_with_decision_tree(
        &mut self,
        value: SValueId,
        result: crate::analysis::semantic::SLocalId,
        join_bb: SBlockId,
        arms: &[MatchArm],
    ) -> SValueId {
        let value = self.owned_pattern_value(value, self.locals[value.index()].ty);
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
            value,
        );
        let target = DecisionTreeTarget::MatchArms {
            result,
            join_bb,
            arms,
        };
        if !self.lower_decision_tree(&tree, &mut projections, target) {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
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
                DecisionTreeTarget::MatchArms {
                    result,
                    join_bb,
                    arms,
                } => {
                    self.bind_decision_tree_leaf(leaf, projections);
                    let arm = &arms[leaf.arm_index];
                    let arm_value = self.lower_expr(arm.body);
                    if self.is_terminated(self.current) {
                        false
                    } else {
                        self.push_synthetic_stmt(SStmtKind::Assign {
                            dst: result,
                            expr: SExpr::Forward(SOperand::expr(arm_value, arm.body)),
                        });
                        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
                        true
                    }
                }
                DecisionTreeTarget::Branch { then_bb, else_bb } => {
                    if leaf.arm_index == 0 {
                        self.bind_decision_tree_leaf(leaf, projections);
                        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
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
        let occurrence = self.project_decision_tree_path(projections, &switch.occurrence);
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

        if is_enum_switch && let Some(enum_ty) = enum_ty {
            self.set_synthetic_terminator(
                self.current,
                STerminatorKind::MatchEnum {
                    value: SOperand::synthetic(occurrence.value),
                    enum_ty,
                    cases: enum_cases.into_boxed_slice(),
                    default: enum_default,
                },
            );
        } else {
            let mut dispatch_bb = self.current;
            for (idx, (case, _, case_bb)) in case_blocks.iter().enumerate() {
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
                                    op: BinOp::Comp(CompBinOp::Eq),
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
                        let cond = self.emit_expr(TyId::bool(self.db), test);
                        self.set_synthetic_terminator(
                            self.current,
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
        for (binding_ref, path) in &leaf.bindings {
            if let Some(binding) = self.typed_body.pat_binding(binding_ref.representative_pat) {
                let dst = self.alloc_binding_local(binding);
                let src = self.project_decision_tree_path(projections, path);
                self.debug_assert_pattern_binding_ty_matches(dst, src);
                self.push_synthetic_stmt(SStmtKind::Assign {
                    dst,
                    expr: SExpr::UseValue(SOperand::synthetic(src.value)),
                });
            }
        }
    }

    fn project_decision_tree_path(
        &mut self,
        projections: &mut DecisionTreeProjectionCache<'db>,
        path: &ProjectionPath<'db>,
    ) -> PatternValue<'db> {
        if let Some(value) = projections.get(path) {
            return value;
        }

        let (mut current_path, mut value) = projections.longest_prefix(path);
        for projection in path
            .strip_prefix(&current_path)
            .expect("longest cached prefix should be a prefix")
            .iter()
        {
            current_path.push(projection.clone());
            value = self.project_decision_tree_value(
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
        base: PatternValue<'db>,
        projection: &Projection<'db>,
        assigned_ty: Option<TyId<'db>>,
    ) -> PatternValue<'db> {
        match projection {
            Projection::Field(field) => self.project_pattern_field(base, *field, assigned_ty),
            Projection::VariantField {
                variant, field_idx, ..
            } => self.project_pattern_variant_field(base, *variant, *field_idx, assigned_ty),
            Projection::Discriminant => {
                let ty = enum_tag_ty(
                    self.db,
                    pattern_match_expected_ty(self.db, base.carrier_ty.0),
                );
                let value = self.emit_expr(
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
            Projection::Index(_) => {
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
        self.emit_expr(ty, SExpr::Const(SConst::Value(value)))
    }
}

fn enum_tag_ty<'db>(db: &'db dyn HirAnalysisDb, enum_ty: TyId<'db>) -> TyId<'db> {
    let variant_count = enum_ty
        .as_enum(db)
        .map(|enum_| enum_.len_variants(db))
        .unwrap_or(0);
    let prim = match enum_tag_bits(variant_count) {
        8 => PrimTy::U8,
        16 => PrimTy::U16,
        32 => PrimTy::U32,
        64 => PrimTy::U64,
        _ => unreachable!("enum tag width must be a primitive integer width"),
    };
    TyId::new(db, TyData::TyBase(TyBase::Prim(prim)))
}
