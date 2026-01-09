//! Match lowering for MIR: converts supported `match` expressions into switches and prepares
//! enum pattern bindings using decision trees for optimized codegen.

use hir::analysis::ty::{
    decision_tree::{
        Case, DecisionTree, LeafNode, Projection, ProjectionPath, SwitchNode, build_decision_tree,
    },
    pattern_analysis::PatternMatrix,
    simplified_pattern::ConstructorKind,
    ty_def::InvalidCause,
};

use super::*;

/// Context passed through decision tree lowering recursion.
///
/// Bundles the invariant data needed at each level of the tree traversal,
/// keeping the recursive function signatures manageable.
struct MatchLoweringCtx<'db> {
    scrutinee_value: ValueId,
    scrutinee_ty: TyId<'db>,
    /// Block for the wildcard arm (if any), used as default fallback.
    wildcard_arm_block: Option<BasicBlockId>,
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Returns `true` if the pattern is a wildcard (`_`).
    ///
    /// # Parameters
    /// - `pat`: Pattern id to inspect.
    ///
    /// # Returns
    /// `true` when the pattern is a wildcard.
    pub(super) fn is_wildcard_pat(&self, pat: PatId) -> bool {
        matches!(
            pat.data(self.db, self.body),
            Partial::Present(Pat::WildCard)
        )
    }

    fn pat_id_for_binding_name(&self, pat: PatId, name: &str) -> Option<PatId> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return None;
        };
        match pat_data {
            Pat::Path(path, _) => {
                let ident = path.to_opt()?.as_ident(self.db)?;
                (ident.data(self.db) == name).then_some(pat)
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => pats
                .iter()
                .find_map(|inner| self.pat_id_for_binding_name(*inner, name)),
            Pat::Record(_, fields) => fields
                .iter()
                .find_map(|field| self.pat_id_for_binding_name(field.pat, name)),
            Pat::Or(lhs, rhs) => self
                .pat_id_for_binding_name(*lhs, name)
                .or_else(|| self.pat_id_for_binding_name(*rhs, name)),
            Pat::WildCard | Pat::Rest | Pat::Lit(_) => None,
        }
    }

    /// Lowers a match expression using decision trees for optimized codegen.
    ///
    /// # Parameters
    /// - `match_expr`: Expression id of the match.
    /// - `scrutinee`: Scrutinee expression id.
    /// - `arms`: Match arms to lower.
    ///
    /// # Returns
    /// The value representing the match expression.
    pub(super) fn lower_match_with_decision_tree(
        &mut self,
        match_expr: ExprId,
        scrutinee: ExprId,
        arms: &[MatchArm],
    ) -> ValueId {
        let value = self.ensure_value(match_expr);
        let Some(block) = self.current_block() else {
            return value;
        };

        let match_ty = self.typed_body.expr_ty(self.db, match_expr);
        let produces_value = !self.is_unit_ty(match_ty) && !match_ty.is_never(self.db);

        // Lower the scrutinee to get its value.
        self.move_to_block(block);
        let scrutinee_value = self.lower_expr(scrutinee);
        let Some(scrut_block) = self.current_block() else {
            return value;
        };

        // Build pattern matrix from match arms
        let scrutinee_ty = self.typed_body.expr_ty(self.db, scrutinee);
        let Some(body) = self.typed_body.body() else {
            // No body available - this shouldn't happen for valid code.
            self.move_to_block(scrut_block);
            self.set_current_terminator(Terminator::Unreachable);
            return value;
        };
        let scope = body.scope();

        let patterns: Vec<Pat> = arms
            .iter()
            .filter_map(|arm| {
                if let Partial::Present(pat) = arm.pat.data(self.db, self.body) {
                    Some(pat.clone())
                } else {
                    None
                }
            })
            .collect();

        if patterns.len() != arms.len() {
            // Some patterns couldn't be resolved. This indicates:
            // 1. Malformed AST from parsing errors, or
            // 2. Upstream type/name resolution errors that should have emitted diagnostics
            //
            // For valid programs, all patterns will be Present. Absent patterns mean the
            // HIR layer already reported errors, so we produce Unreachable MIR rather than
            // attempting to lower patterns we can't understand. This prevents cascading
            // errors from incomplete pattern information.
            debug_assert!(
                false,
                "MIR lowering: {} of {} match arm patterns are Absent - \
                 upstream errors should have been reported",
                arms.len() - patterns.len(),
                arms.len()
            );
            self.move_to_block(scrut_block);
            self.set_current_terminator(Terminator::Unreachable);
            return value;
        }

        let matrix =
            PatternMatrix::from_hir_patterns(self.db, &patterns, self.body, scope, scrutinee_ty);

        // Build decision tree from pattern matrix
        let tree = build_decision_tree(self.db, &matrix);

        let leaf_bindings = self.collect_leaf_bindings(&tree);

        let result_local = produces_value.then(|| {
            let local = self.alloc_temp_local(match_ty, true, "match");
            self.builder.body.values[value.index()].origin = ValueOrigin::Local(local);
            local
        });
        if !produces_value {
            self.builder.body.values[value.index()].origin = ValueOrigin::Unit;
        }

        // Pre-lower each arm body to determine termination status and create blocks.
        let mut merge_block: Option<BasicBlockId> = None;
        let mut arm_blocks: Vec<BasicBlockId> = Vec::with_capacity(arms.len());
        let mut wildcard_arm_block = None;
        for arm in arms {
            let arm_entry = self.alloc_block();
            self.move_to_block(arm_entry);

            if wildcard_arm_block.is_none() && self.is_wildcard_pat(arm.pat) {
                wildcard_arm_block = Some(arm_entry);
            }

            let arm_idx = arm_blocks.len();
            if let Some(bindings) = leaf_bindings.get(&arm_idx) {
                for (name, path) in bindings {
                    let Some(binding_pat) = self.pat_id_for_binding_name(arm.pat, name) else {
                        continue;
                    };
                    let binding =
                        self.typed_body
                            .pat_binding(binding_pat)
                            .unwrap_or(LocalBinding::Local {
                                pat: binding_pat,
                                is_mut: false,
                            });
                    let Some(local) = self.local_for_binding(binding) else {
                        continue;
                    };
                    let (_place, value_id) =
                        self.lower_projection_path_for_binding(path, scrutinee_value, scrutinee_ty);
                    self.push_inst_here(MirInst::Assign {
                        stmt: None,
                        dest: Some(local),
                        rvalue: crate::ir::Rvalue::Value(value_id),
                    });
                }
            }

            let arm_value = self.lower_expr(arm.body);
            let arm_end = self.current_block();
            if let Some(end_block) = arm_end {
                let merge = match merge_block {
                    Some(block) => block,
                    None => {
                        let block = self.alloc_block();
                        merge_block = Some(block);
                        block
                    }
                };
                self.move_to_block(end_block);
                if let Some(result_local) = result_local {
                    self.push_inst_here(MirInst::Assign {
                        stmt: None,
                        dest: Some(result_local),
                        rvalue: crate::ir::Rvalue::Value(arm_value),
                    });
                }
                self.goto(merge);
            }
            arm_blocks.push(arm_entry);
        }

        let ctx = MatchLoweringCtx {
            scrutinee_value,
            scrutinee_ty,
            wildcard_arm_block,
        };
        let tree_entry = self.lower_decision_tree(&tree, &arm_blocks, &ctx);

        // Set scrut_block to jump to the tree entry
        self.move_to_block(scrut_block);
        if let Some(result_local) = result_local {
            self.push_inst_here(MirInst::Assign {
                stmt: None,
                dest: Some(result_local),
                rvalue: crate::ir::Rvalue::ZeroInit,
            });
        }
        self.goto(tree_entry);

        if let Some(merge) = merge_block {
            self.move_to_block(merge);
        }
        value
    }

    /// Recursively lowers a decision tree to MIR basic blocks.
    ///
    /// # Parameters
    /// - `tree`: Decision tree node to lower.
    /// - `arm_blocks`: Pre-created blocks and termination status for each arm.
    /// - `ctx`: Match lowering context with scrutinee info and merge block.
    ///
    /// # Returns
    /// The entry basic block for this tree node.
    fn lower_decision_tree(
        &mut self,
        tree: &DecisionTree<'db>,
        arm_blocks: &[BasicBlockId],
        ctx: &MatchLoweringCtx<'db>,
    ) -> BasicBlockId {
        match tree {
            DecisionTree::Leaf(leaf) => self.lower_leaf_node(leaf, arm_blocks),
            DecisionTree::Switch(switch_node) => {
                self.lower_switch_node(switch_node, arm_blocks, ctx)
            }
        }
    }

    /// Lowers a leaf node (match arm execution) to the pre-created arm block.
    fn lower_leaf_node(
        &mut self,
        leaf: &LeafNode<'db>,
        arm_blocks: &[BasicBlockId],
    ) -> BasicBlockId {
        // Return the pre-created block for this arm
        // The arm body was already lowered during the pre-lowering phase
        arm_blocks[leaf.arm_index]
    }

    /// Lowers a switch node (test and branch) to MIR basic blocks.
    fn lower_switch_node(
        &mut self,
        switch_node: &SwitchNode<'db>,
        arm_blocks: &[BasicBlockId],
        ctx: &MatchLoweringCtx<'db>,
    ) -> BasicBlockId {
        // For Type constructors (tuples/structs), there's no discriminant to switch on.
        // We skip straight to the subtree and let the inner switches handle the actual values.
        let is_structural_only = switch_node.arms.iter().all(|(case, _)| {
            matches!(
                case,
                Case::Constructor(ConstructorKind::Type(_)) | Case::Default
            )
        });

        if is_structural_only && !switch_node.arms.is_empty() {
            // Find the structural subtree to descend into
            let structural_subtree = switch_node
                .arms
                .iter()
                .find(|(case, _)| matches!(case, Case::Constructor(ConstructorKind::Type(_))))
                .or_else(|| {
                    switch_node
                        .arms
                        .iter()
                        .find(|(case, _)| matches!(case, Case::Default))
                })
                .map(|(_, subtree)| subtree);

            if let Some(subtree) = structural_subtree {
                // For structural types, directly lower the subtree - no switch needed at this level
                return self.lower_decision_tree(subtree, arm_blocks, ctx);
            }
        }

        let test_block = self.alloc_block();
        self.move_to_block(test_block);

        // Extract the value to test based on the occurrence path.
        // Any scalar loads needed to compute the test value are emitted into `test_block`.
        let test_value = self.lower_occurrence(
            &switch_node.occurrence,
            ctx.scrutinee_value,
            ctx.scrutinee_ty,
        );

        // Recursively lower each case
        let mut targets = vec![];
        let mut default_block = None;

        for (case, subtree) in &switch_node.arms {
            let subtree_entry = self.lower_decision_tree(subtree, arm_blocks, ctx);

            match case {
                Case::Constructor(ctor) => {
                    if let Some(switch_val) = self.constructor_to_switch_value(ctor) {
                        targets.push(SwitchTarget {
                            value: switch_val,
                            block: subtree_entry,
                        });
                    }
                }
                Case::Default => {
                    default_block = Some(subtree_entry);
                }
            }
        }

        // Use the decision tree's default, then wildcard arm, then unreachable.
        // This ensures MIR explicitly routes defaults to the wildcard arm rather than
        // having codegen rediscover it.
        let default = default_block.or(ctx.wildcard_arm_block).unwrap_or_else(|| {
            let unreachable = self.alloc_block();
            self.set_terminator(unreachable, Terminator::Unreachable);
            unreachable
        });

        self.move_to_block(test_block);
        self.switch(test_value, targets, default);

        test_block
    }

    fn try_lower_non_ref_scrutinee_projection_as_transparent_cast(
        &mut self,
        context: &'static str,
        scrutinee_value: ValueId,
        scrutinee_ty: TyId<'db>,
        path: &ProjectionPath<'db>,
        result_ty: TyId<'db>,
    ) -> Option<ValueId> {
        let scrutinee_repr = self.builder.body.value(scrutinee_value).repr;
        if matches!(scrutinee_repr, ValueRepr::Ref(_)) {
            return None;
        }

        // Non-`Ref` scrutinee (word/opaque pointer): only transparent-newtype peeling is valid.
        //
        // For nested newtypes (`A { inner: B { inner: u256 } }`), the decision-tree projection
        // path can contain multiple `Field(0)` steps. These are all representation-preserving
        // casts that must not be lowered as place/projection loads.
        let mut current_ty = scrutinee_ty;
        for proj in path.iter() {
            if let Projection::Field(0) = proj
                && let Some(inner) = crate::repr::transparent_newtype_field_ty(self.db, current_ty)
            {
                current_ty = inner;
                continue;
            }

            panic!(
                "{context} requires `Ref` scrutinee (ty={}, repr={:?}, path_len={})",
                scrutinee_ty.pretty_print(self.db),
                scrutinee_repr,
                path.len()
            );
        }

        debug_assert_eq!(
            current_ty,
            result_ty,
            "transparent-newtype projection produced unexpected type (got={}, expected={})",
            current_ty.pretty_print(self.db),
            result_ty.pretty_print(self.db),
        );

        let space = scrutinee_repr
            .address_space()
            .unwrap_or(AddressSpaceKind::Memory);
        Some(self.builder.body.alloc_value(ValueData {
            ty: result_ty,
            origin: ValueOrigin::TransparentCast {
                value: scrutinee_value,
            },
            repr: self.value_repr_for_ty(result_ty, space),
        }))
    }

    /// Extracts a value from the scrutinee based on a projection path.
    ///
    /// Uses Place with projections - offset computation is deferred to codegen.
    /// This keeps MIR semantic and enables better analysis.
    fn lower_occurrence(
        &mut self,
        path: &ProjectionPath<'db>,
        scrutinee_value: ValueId,
        scrutinee_ty: TyId<'db>,
    ) -> ValueId {
        fn alloc_local_value<'db>(
            builder: &mut MirBuilder<'db, '_>,
            ty: TyId<'db>,
            local: LocalId,
        ) -> ValueId {
            let space = builder.builder.body.local(local).address_space;
            builder.builder.body.alloc_value(ValueData {
                ty,
                origin: ValueOrigin::Local(local),
                repr: builder.value_repr_for_ty(ty, space),
            })
        }

        fn emit_load_to_temp<'db>(
            builder: &mut MirBuilder<'db, '_>,
            ty: TyId<'db>,
            place: Place<'db>,
        ) -> ValueId {
            let dest = builder.alloc_temp_local(ty, false, "load");
            builder.push_inst_here(MirInst::Assign {
                stmt: None,
                dest: Some(dest),
                rvalue: crate::ir::Rvalue::Load { place },
            });
            alloc_local_value(builder, ty, dest)
        }

        // Helper to check if a type is an enum
        fn is_enum_type(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
            let (base_ty, _) = ty.decompose_ty_app(db);
            if let TyData::TyBase(TyBase::Adt(adt_def)) = base_ty.data(db) {
                matches!(adt_def.adt_ref(db), AdtRef::Enum(_))
            } else {
                false
            }
        }

        // Empty path means access the scrutinee directly
        // But we still need to extract discriminant for enums
        if path.is_empty() {
            if !is_enum_type(self.db, scrutinee_ty) {
                return scrutinee_value;
            }
            let place = Place::new(
                scrutinee_value,
                MirProjectionPath::from_projection(MirProjection::Discriminant),
            );
            return emit_load_to_temp(self, self.u256_ty(), place);
        }

        // Compute the result type of the projection
        let result_ty = self.compute_projection_result_type(scrutinee_ty, path);
        if let Some(value) = self.try_lower_non_ref_scrutinee_projection_as_transparent_cast(
            "match projection path",
            scrutinee_value,
            scrutinee_ty,
            path,
            result_ty,
        ) {
            return value;
        }
        let addr_space = self.value_address_space(scrutinee_value);

        // Build Place with the full projection path
        let place = Place::new(
            scrutinee_value,
            self.mir_projection_from_decision_path(path),
        );

        // Use PlaceRef for by-ref values (pointer), explicit load for word-like values.
        let current_value = if self.is_by_ref_ty(result_ty) {
            self.builder.body.alloc_value(ValueData {
                ty: result_ty,
                origin: ValueOrigin::PlaceRef(place),
                repr: ValueRepr::Ref(addr_space),
            })
        } else {
            emit_load_to_temp(self, result_ty, place)
        };

        // For enums, extract the discriminant for switching
        if is_enum_type(self.db, result_ty) {
            let mut discr_path = self.mir_projection_from_decision_path(path);
            discr_path.push(MirProjection::Discriminant);
            let place = Place::new(scrutinee_value, discr_path);
            return emit_load_to_temp(self, self.u256_ty(), place);
        }

        current_value
    }

    /// Converts a constructor to a switch value for MIR.
    fn constructor_to_switch_value(&self, ctor: &ConstructorKind<'db>) -> Option<SwitchValue> {
        match ctor {
            ConstructorKind::Variant(variant, _) => Some(SwitchValue::Enum(variant.idx as u64)),
            ConstructorKind::Literal(lit, _) => match lit {
                LitKind::Int(value) => Some(SwitchValue::Int(value.data(self.db).clone())),
                LitKind::Bool(value) => Some(SwitchValue::Bool(*value)),
                _ => None,
            },
            ConstructorKind::Type(_) => None,
        }
    }

    /// Extracts a value from the scrutinee for binding purposes.
    ///
    /// Returns both:
    /// - a `PlaceRef` value that references the bound location, and
    /// - the binding's "value" (a `PlaceLoad` for scalars, `PlaceRef` for aggregates).
    fn lower_projection_path_for_binding(
        &mut self,
        path: &ProjectionPath<'db>,
        scrutinee_value: ValueId,
        scrutinee_ty: TyId<'db>,
    ) -> (ValueId, ValueId) {
        fn alloc_local_value<'db>(
            builder: &mut MirBuilder<'db, '_>,
            ty: TyId<'db>,
            local: LocalId,
        ) -> ValueId {
            let space = builder.builder.body.local(local).address_space;
            builder.builder.body.alloc_value(ValueData {
                ty,
                origin: ValueOrigin::Local(local),
                repr: builder.value_repr_for_ty(ty, space),
            })
        }

        // Empty path means we bind to the scrutinee itself
        if path.is_empty() {
            return (scrutinee_value, scrutinee_value);
        }

        // Compute the final type by walking the projection path
        let final_ty = self.compute_projection_result_type(scrutinee_ty, path);
        let is_by_ref = self.is_by_ref_ty(final_ty);

        if let Some(cast) = self.try_lower_non_ref_scrutinee_projection_as_transparent_cast(
            "match binding projection path",
            scrutinee_value,
            scrutinee_ty,
            path,
            final_ty,
        ) {
            return (cast, cast);
        }
        let addr_space = self.value_address_space(scrutinee_value);

        // Create the Place
        let place = Place::new(
            scrutinee_value,
            self.mir_projection_from_decision_path(path),
        );

        let place_ref_id = self.builder.body.alloc_value(ValueData {
            ty: final_ty,
            origin: ValueOrigin::PlaceRef(place.clone()),
            repr: ValueRepr::Ref(addr_space),
        });

        // Use PlaceRef for by-ref values (pointer only), explicit load for word-like values.
        // When by-ref, re-use the place ref as the "value".
        let value_id = if is_by_ref {
            place_ref_id
        } else {
            let dest = self.alloc_temp_local(final_ty, false, "load");
            self.push_inst_here(MirInst::Assign {
                stmt: None,
                dest: Some(dest),
                rvalue: crate::ir::Rvalue::Load { place },
            });
            alloc_local_value(self, final_ty, dest)
        };

        (place_ref_id, value_id)
    }

    /// Computes the result type of applying a projection path to a type.
    ///
    /// Returns an invalid type if any projection step is out of bounds,
    /// which will cause downstream type errors rather than silent bugs.
    fn compute_projection_result_type(
        &self,
        base_ty: TyId<'db>,
        path: &ProjectionPath<'db>,
    ) -> TyId<'db> {
        let mut current_ty = base_ty;

        for proj in path.iter() {
            match proj {
                Projection::Field(field_idx) => {
                    let field_types = current_ty.field_types(self.db);
                    if let Some(&field_ty) = field_types.get(*field_idx) {
                        current_ty = field_ty;
                    } else {
                        // Out of bounds field access - return invalid type
                        return TyId::invalid(self.db, InvalidCause::Other);
                    }
                }
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => {
                    let ctor = ConstructorKind::Variant(*variant, *enum_ty);
                    let field_types = ctor.field_types(self.db);
                    if let Some(&field_ty) = field_types.get(*field_idx) {
                        current_ty = field_ty;
                    } else {
                        // Out of bounds variant field access - return invalid type
                        return TyId::invalid(self.db, InvalidCause::Other);
                    }
                }

                Projection::Discriminant => {
                    current_ty = self.u256_ty();
                }
                Projection::Index(idx_source) => {
                    let (base, args) = current_ty.decompose_ty_app(self.db);
                    if !base.is_array(self.db) || args.is_empty() {
                        return TyId::invalid(self.db, InvalidCause::Other);
                    }
                    match idx_source {
                        hir::projection::IndexSource::Constant(_) => {
                            current_ty = args[0];
                        }
                        hir::projection::IndexSource::Dynamic(infallible) => match *infallible {},
                    }
                }
                Projection::Deref => {
                    return TyId::invalid(self.db, InvalidCause::Other);
                }
            }
        }

        current_ty
    }

    fn mir_projection_from_decision_path(
        &self,
        path: &ProjectionPath<'db>,
    ) -> MirProjectionPath<'db> {
        let mut projection = MirProjectionPath::new();
        for proj in path.iter() {
            match proj {
                Projection::Field(idx) => projection.push(MirProjection::Field(*idx)),
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => projection.push(MirProjection::VariantField {
                    variant: *variant,
                    enum_ty: *enum_ty,
                    field_idx: *field_idx,
                }),
                Projection::Discriminant => projection.push(MirProjection::Discriminant),
                Projection::Index(idx_source) => match idx_source {
                    hir::projection::IndexSource::Constant(idx) => {
                        projection.push(MirProjection::Index(
                            hir::projection::IndexSource::Constant(*idx),
                        ));
                    }
                    hir::projection::IndexSource::Dynamic(infallible) => match *infallible {},
                },
                Projection::Deref => projection.push(MirProjection::Deref),
            }
        }
        projection
    }

    /// Collects all bindings from decision tree leaves, grouped by arm index.
    ///
    /// Returns a map from arm_index to a list of (variable_name, projection_path) pairs.
    fn collect_leaf_bindings(
        &self,
        tree: &DecisionTree<'db>,
    ) -> FxHashMap<usize, Vec<(String, ProjectionPath<'db>)>> {
        let mut bindings_by_arm: FxHashMap<usize, Vec<(String, ProjectionPath<'db>)>> =
            FxHashMap::default();
        self.collect_leaf_bindings_recursive(tree, &mut bindings_by_arm);
        bindings_by_arm
    }

    fn collect_leaf_bindings_recursive(
        &self,
        tree: &DecisionTree<'db>,
        bindings_by_arm: &mut FxHashMap<usize, Vec<(String, ProjectionPath<'db>)>>,
    ) {
        match tree {
            DecisionTree::Leaf(leaf) => {
                let arm_bindings = bindings_by_arm.entry(leaf.arm_index).or_default();
                for ((ident_id, _binding_idx), path) in &leaf.bindings {
                    let name = ident_id.data(self.db).to_string();
                    // Deduplicate by name. The binding_idx in the key distinguishes
                    // different binding sites in the decision tree, but within a single
                    // arm all occurrences of a variable name should resolve to the same
                    // binding. Taking the first occurrence is correct because all paths
                    // to this leaf will produce the same binding for that name.
                    if !arm_bindings.iter().any(|(n, _)| n == &name) {
                        arm_bindings.push((name, path.clone()));
                    }
                }
            }
            DecisionTree::Switch(switch_node) => {
                for (_, subtree) in &switch_node.arms {
                    self.collect_leaf_bindings_recursive(subtree, bindings_by_arm);
                }
            }
        }
    }
}
