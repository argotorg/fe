use cranelift_entity::EntityRef;
use num_bigint::BigInt;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            FieldIndex, SBlockId, SConst, SExpr, SOperand, SStmtKind, STerminatorKind, SValueId,
            VariantIndex, bool_const, bytes_const, int_const,
        },
        ty::{
            decision_tree::{
                Case, DecisionTree, LeafNode, Projection, ProjectionPath, SwitchNode,
                build_decision_tree,
            },
            normalize::normalize_ty,
            pattern_analysis::PatternMatrix,
            pattern_ir::{ConstructorKind, ValidatedPatId},
            pattern_types::{
                PatternProjectionStep, pattern_match_expected_ty, project_pattern_child_carrier_ty,
            },
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
}

impl<'db> DecisionTreeProjectionCache<'db> {
    fn new(root_value: PatternValue<'db>) -> Self {
        Self {
            entries: vec![(ProjectionPath::default(), root_value)],
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
        self.lower_validated_pattern_branch(root, value, then_bb, else_bb);
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
    ) -> PatternValue<'db> {
        let ty = project_pattern_child_carrier_ty(
            self.db,
            base.carrier_ty.0,
            PatternProjectionStep::Field(field_idx),
        );
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
    ) -> PatternValue<'db> {
        let ty = project_pattern_child_carrier_ty(
            self.db,
            base.carrier_ty.0,
            PatternProjectionStep::VariantField { variant, field_idx },
        );
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

    fn bind_validated_pattern(&mut self, pat: ValidatedPatId, value: PatternValue<'db>) {
        let pattern_store = self.typed_body.pattern_store();
        if let Some(binding) = pattern_store.wildcard_binding(pat).flatten() {
            if let Some(local_binding) = self.typed_body.pat_binding(binding.representative_pat) {
                let dst = self.alloc_binding_local(local_binding);
                self.debug_assert_pattern_binding_ty_matches(dst, value);
                self.push_synthetic_stmt(SStmtKind::Assign {
                    dst,
                    expr: SExpr::UseValue(SOperand::synthetic(value.value)),
                });
            }
        } else if let Some(ctor) = pattern_store.constructor_kind(pat) {
            match ctor {
                ConstructorKind::Variant(variant, _) => {
                    for idx in 0..pattern_store.child_count(pat) {
                        let field_pat = pattern_store
                            .child(pat, idx)
                            .expect("pattern child should exist");
                        let field = self.project_pattern_variant_field(value, variant, idx);
                        self.bind_validated_pattern(field_pat, field);
                    }
                }
                ConstructorKind::Type(_) => {
                    for idx in 0..pattern_store.child_count(pat) {
                        let field_pat = pattern_store
                            .child(pat, idx)
                            .expect("pattern child should exist");
                        let field = self.project_pattern_field(value, idx);
                        self.bind_validated_pattern(field_pat, field);
                    }
                }
                ConstructorKind::Literal(..) => {}
            }
        } else if let Some(first) = pattern_store.child(pat, 0) {
            self.bind_validated_pattern(first, value);
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
        let tree = build_decision_tree(
            self.db,
            &PatternMatrix::from_roots(self.typed_body.pattern_store(), &roots),
        );
        let mut projections = DecisionTreeProjectionCache::new(value);
        if !self.lower_decision_tree(&tree, &mut projections, result, join_bb, arms) {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
        result
    }

    fn lower_validated_pattern_branch(
        &mut self,
        pat: ValidatedPatId,
        value: PatternValue<'db>,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) {
        let pattern_store = self.typed_body.pattern_store();
        if pattern_store.wildcard_binding(pat).is_some() {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
        } else if let Some(ctor) = pattern_store.constructor_kind(pat) {
            match ctor {
                ConstructorKind::Literal(lit, ty) => {
                    let rhs = self.literal_pattern_value(ty, lit);
                    let cond = self.emit_expr(
                        TyId::bool(self.db),
                        SExpr::Binary {
                            op: BinOp::Comp(CompBinOp::Eq),
                            lhs: SOperand::synthetic(value.value),
                            rhs: SOperand::synthetic(rhs),
                        },
                    );
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Branch {
                            cond: SOperand::synthetic(cond),
                            then_bb,
                            else_bb,
                        },
                    );
                }
                ConstructorKind::Variant(variant, _) => {
                    let success_bb = if pattern_store.child_count(pat) == 0 {
                        then_bb
                    } else {
                        self.new_block()
                    };
                    let cond = self.emit_expr(
                        TyId::bool(self.db),
                        SExpr::IsEnumVariant {
                            value: SOperand::synthetic(value.value),
                            variant: VariantIndex(variant.idx),
                        },
                    );
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Branch {
                            cond: SOperand::synthetic(cond),
                            then_bb: success_bb,
                            else_bb,
                        },
                    );
                    if pattern_store.child_count(pat) != 0 {
                        self.switch_to(success_bb);
                        self.lower_variant_field_branches(value, variant, pat, then_bb, else_bb);
                    }
                }
                ConstructorKind::Type(_) => {
                    self.lower_field_branches(value, pat, then_bb, else_bb);
                }
            }
        } else {
            let pat_count = pattern_store.child_count(pat);
            if pat_count == 0 {
                self.set_synthetic_terminator(self.current, STerminatorKind::Goto(else_bb));
                return;
            }
            for idx in 0..pat_count - 1 {
                let next_else = self.new_block();
                self.lower_validated_pattern_branch(
                    pattern_store
                        .child(pat, idx)
                        .expect("pattern alternative should exist"),
                    value,
                    then_bb,
                    next_else,
                );
                self.switch_to(next_else);
            }
            self.lower_validated_pattern_branch(
                pattern_store
                    .child(pat, pat_count - 1)
                    .expect("pattern alternative should exist"),
                value,
                then_bb,
                else_bb,
            );
        }
    }

    fn lower_decision_tree(
        &mut self,
        tree: &DecisionTree<'db>,
        projections: &mut DecisionTreeProjectionCache<'db>,
        result: crate::analysis::semantic::SLocalId,
        join_bb: SBlockId,
        arms: &[MatchArm],
    ) -> bool {
        match tree {
            DecisionTree::Leaf(leaf) => {
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
            DecisionTree::Switch(switch) => {
                self.lower_decision_tree_switch(switch, projections, result, join_bb, arms)
            }
        }
    }

    fn lower_decision_tree_switch(
        &mut self,
        switch: &SwitchNode<'db>,
        projections: &mut DecisionTreeProjectionCache<'db>,
        result: crate::analysis::semantic::SLocalId,
        join_bb: SBlockId,
        arms: &[MatchArm],
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
            join_reachable |=
                self.lower_decision_tree(subtree, &mut subtree_projections, result, join_bb, arms);
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
            value = self.project_decision_tree_value(value, projection);
            projections.insert(current_path.clone(), value);
        }
        value
    }

    fn project_decision_tree_value(
        &mut self,
        base: PatternValue<'db>,
        projection: &Projection<'db>,
    ) -> PatternValue<'db> {
        match projection {
            Projection::Field(field) => self.project_pattern_field(base, *field),
            Projection::VariantField {
                variant, field_idx, ..
            } => self.project_pattern_variant_field(base, *variant, *field_idx),
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

    fn lower_field_branches(
        &mut self,
        value: PatternValue<'db>,
        pat: ValidatedPatId,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) {
        let pattern_store = self.typed_body.pattern_store();
        let refutable_fields = (0..pattern_store.child_count(pat))
            .filter_map(|field_idx| {
                let field_pat = pattern_store
                    .child(pat, field_idx)
                    .expect("pattern child should exist");
                (!self.validated_pattern_is_irrefutable(field_pat))
                    .then_some((field_idx, field_pat))
            })
            .collect::<Vec<_>>();
        if refutable_fields.is_empty() {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
            return;
        }

        let refutable_len = refutable_fields.len();
        for (idx, (field_idx, field_pat)) in refutable_fields.into_iter().enumerate() {
            let field_value = self.project_pattern_field(value, field_idx);
            let next_then = if idx + 1 == refutable_len {
                then_bb
            } else {
                self.new_block()
            };
            self.lower_validated_pattern_branch(field_pat, field_value, next_then, else_bb);
            if idx + 1 != refutable_len {
                self.switch_to(next_then);
            }
        }
    }

    fn lower_variant_field_branches(
        &mut self,
        value: PatternValue<'db>,
        variant: crate::hir_def::EnumVariant<'db>,
        pat: ValidatedPatId,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) {
        let pattern_store = self.typed_body.pattern_store();
        let refutable_fields = (0..pattern_store.child_count(pat))
            .filter_map(|field_idx| {
                let field_pat = pattern_store
                    .child(pat, field_idx)
                    .expect("pattern child should exist");
                (!self.validated_pattern_is_irrefutable(field_pat))
                    .then_some((field_idx, field_pat))
            })
            .collect::<Vec<_>>();
        if refutable_fields.is_empty() {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
            return;
        }

        let refutable_len = refutable_fields.len();
        for (idx, (field_idx, field_pat)) in refutable_fields.into_iter().enumerate() {
            let field_value = self.project_pattern_variant_field(value, variant, field_idx);
            let next_then = if idx + 1 == refutable_len {
                then_bb
            } else {
                self.new_block()
            };
            self.lower_validated_pattern_branch(field_pat, field_value, next_then, else_bb);
            if idx + 1 != refutable_len {
                self.switch_to(next_then);
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
    if variant_count <= u8::MAX as usize + 1 {
        TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U8)))
    } else if variant_count <= u16::MAX as usize + 1 {
        TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U16)))
    } else if variant_count <= u32::MAX as usize + 1 {
        TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U32)))
    } else if variant_count <= u64::MAX as usize {
        TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U64)))
    } else {
        TyId::u256(db)
    }
}
