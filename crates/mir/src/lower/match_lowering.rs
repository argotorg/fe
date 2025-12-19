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
    match_expr: ExprId,
    /// Block for the wildcard arm (if any), used as default fallback.
    wildcard_arm_block: Option<BasicBlockId>,
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Extracts a literal `SwitchValue` from a pattern when possible.
    ///
    /// # Parameters
    /// - `pat`: Pattern id to inspect.
    ///
    /// # Returns
    /// Literal switch value or `None` when not supported.
    pub(super) fn literal_pat_value(&self, pat: PatId) -> Option<SwitchValue> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return None;
        };

        match pat_data {
            Pat::Lit(lit) => {
                let Partial::Present(lit) = lit else {
                    return None;
                };
                match lit {
                    LitKind::Int(value) => {
                        let ty = self.typed_body.pat_ty(self.db, pat);
                        let bits = self.int_type_bits(ty)?;
                        if bits > 256 {
                            return None;
                        }
                        let literal = value.data(self.db).clone();
                        let literal_bits = literal.bits();
                        if literal_bits > bits as u64 {
                            return None;
                        }
                        Some(SwitchValue::Int(literal))
                    }
                    LitKind::Bool(value) => {
                        if !self.typed_body.pat_ty(self.db, pat).is_bool(self.db) {
                            return None;
                        }
                        Some(SwitchValue::Bool(*value))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Resolves an enum variant path within a scope.
    ///
    /// # Parameters
    /// - `path`: Path to resolve.
    /// - `scope`: Scope to use for resolution.
    ///
    /// # Returns
    /// Resolved variant metadata or `None` on failure.
    pub(super) fn resolve_enum_variant(
        &self,
        path: PathId<'db>,
        scope: ScopeId<'db>,
    ) -> Option<ResolvedVariant<'db>> {
        let res = resolve_path(
            self.db,
            path,
            scope,
            PredicateListId::empty_list(self.db),
            false,
        )
        .ok()?;
        match res {
            PathRes::EnumVariant(variant) => Some(variant),
            _ => None,
        }
    }

    /// Converts enum patterns into `MatchArmPattern` when resolvable.
    ///
    /// # Parameters
    /// - `pat`: Pattern id to inspect.
    ///
    /// # Returns
    /// Enum pattern info or `None` when not an enum pattern.
    pub(super) fn enum_pat_value(&self, pat: PatId) -> Option<MatchArmPattern> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return None;
        };
        let body = self.typed_body.body()?;
        let scope = body.scope();

        if let Pat::Path(path, ..) = pat_data
            && let Some(path) = path.to_opt()
            && let Some(variant) = self.resolve_enum_variant(path, scope)
        {
            let enum_name = variant
                .enum_(self.db)
                .name(self.db)
                .to_opt()
                .unwrap()
                .data(self.db)
                .to_string();
            return Some(MatchArmPattern::Enum {
                variant_index: variant.variant.idx as u64,
                enum_name,
            });
        }

        if let Pat::PathTuple(path, _elem_pats) = pat_data
            && let Some(path) = path.to_opt()
            && let Some(variant) = self.resolve_enum_variant(path, scope)
        {
            let enum_name = variant
                .enum_(self.db)
                .name(self.db)
                .to_opt()
                .unwrap()
                .data(self.db)
                .to_string();

            return Some(MatchArmPattern::Enum {
                variant_index: variant.variant.idx as u64,
                enum_name,
            });
        }

        None
    }

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

        // Pre-lower each arm body to determine termination status and create blocks
        let mut merge_block: Option<BasicBlockId> = None;
        let mut arm_blocks = Vec::with_capacity(arms.len());
        let mut arm_values = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm_entry = self.alloc_block();
            self.move_to_block(arm_entry);
            let arm_value = self.lower_expr(arm.body);
            let arm_end = self.current_block();
            let terminates = arm_end.is_none();
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
                self.goto(merge);
            }
            arm_values.push(if terminates { None } else { Some(arm_value) });
            arm_blocks.push((arm_entry, terminates));
        }

        // Find the wildcard arm block (if any) for use as default fallback.
        // This ensures that even for complete matches, the default routes to the
        // wildcard arm rather than unreachable.
        let wildcard_arm_block = arms
            .iter()
            .zip(arm_blocks.iter())
            .find(|(arm, _)| self.is_wildcard_pat(arm.pat))
            .map(|(_, (block_id, _))| *block_id);

        // Collect arm info for codegen (needed for match_info)
        let mut arms_info: Vec<MatchArmLowering> = arms
            .iter()
            .zip(arm_blocks.iter())
            .zip(arm_values.iter())
            .map(|((arm, (block_id, terminates)), value)| {
                // Determine the pattern type for codegen
                let pattern = if self.is_wildcard_pat(arm.pat) {
                    MatchArmPattern::Wildcard
                } else if let Some(lit_val) = self.literal_pat_value(arm.pat) {
                    MatchArmPattern::Literal(lit_val)
                } else if let Some(enum_pat) = self.enum_pat_value(arm.pat) {
                    enum_pat
                } else {
                    // Fallback to wildcard for unsupported patterns
                    MatchArmPattern::Wildcard
                };

                MatchArmLowering {
                    pattern,
                    body: arm.body,
                    block: *block_id,
                    terminates: *terminates,
                    decision_tree_bindings: Vec::new(),
                    value: *value,
                }
            })
            .collect();

        // Populate decision tree bindings for tuple/struct/enum patterns.
        // Decision tree bindings handle all patterns uniformly, including nested enums.
        let leaf_bindings = self.collect_leaf_bindings(&tree);
        for (arm_idx, arm_info) in arms_info.iter_mut().enumerate() {
            if let Some(bindings) = leaf_bindings.get(&arm_idx) {
                for (name, path) in bindings {
                    let (place, value) = self.lower_projection_path_for_binding(
                        path,
                        scrutinee_value,
                        scrutinee_ty,
                        match_expr,
                    );
                    arm_info.decision_tree_bindings.push(DecisionTreeBinding {
                        name: name.clone(),
                        place,
                        value,
                    });
                }
            }
        }

        // Store match info for codegen
        self.builder.body.match_info.insert(
            match_expr,
            MatchLoweringInfo {
                scrutinee: scrutinee_value,
                merge_block,
                arms: arms_info,
            },
        );

        let ctx = MatchLoweringCtx {
            scrutinee_value,
            scrutinee_ty,
            match_expr,
            wildcard_arm_block,
        };
        let tree_entry = self.lower_decision_tree(&tree, &arm_blocks, &ctx);

        // Set scrut_block to jump to the tree entry
        self.move_to_block(scrut_block);
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
        arm_blocks: &[(BasicBlockId, bool)],
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
        arm_blocks: &[(BasicBlockId, bool)],
    ) -> BasicBlockId {
        // Return the pre-created block for this arm
        // The arm body was already lowered during the pre-lowering phase
        let (arm_block, _terminates) = arm_blocks[leaf.arm_index];
        arm_block
    }

    /// Lowers a switch node (test and branch) to MIR basic blocks.
    fn lower_switch_node(
        &mut self,
        switch_node: &SwitchNode<'db>,
        arm_blocks: &[(BasicBlockId, bool)],
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

        // Extract the value to test based on the occurrence path
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

        // Always use MatchExpr origin for all switches in a decision tree match.
        // This ensures emit_match_switch is called, which reuses the same match temp
        // via match_values.entry(expr_id). All switches in the same match share
        // the same temp variable for collecting results.
        let origin = SwitchOrigin::MatchExpr(ctx.match_expr);

        self.move_to_block(test_block);
        self.switch(test_value, targets, default, origin);

        test_block
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
            let addr_space = self.value_address_space(scrutinee_value);
            let place = Place::new(
                scrutinee_value,
                MirProjectionPath::from_projection(MirProjection::Discriminant),
                addr_space,
            );
            return self.builder.body.alloc_value(ValueData {
                ty: self.u256_ty(),
                origin: ValueOrigin::PlaceLoad(place),
            });
        }

        // Compute the result type of the projection
        let result_ty = self.compute_projection_result_type(scrutinee_ty, path);
        let addr_space = self.value_address_space(scrutinee_value);
        let is_aggregate = result_ty.field_count(self.db) > 0
            || result_ty.is_array(self.db)
            || result_ty
                .adt_ref(self.db)
                .is_some_and(|adt| matches!(adt, AdtRef::Enum(_)));

        // Build Place with the full projection path
        let place = Place::new(
            scrutinee_value,
            self.mir_projection_from_decision_path(path),
            addr_space,
        );

        // Use PlaceRef for aggregates (returns pointer), PlaceLoad for scalars (loads value)
        let origin = if is_aggregate {
            ValueOrigin::PlaceRef(place)
        } else {
            ValueOrigin::PlaceLoad(place)
        };

        let current_value = self.builder.body.alloc_value(ValueData {
            ty: result_ty,
            origin,
        });
        self.value_address_space.insert(current_value, addr_space);

        // For enums, extract the discriminant for switching
        if is_enum_type(self.db, result_ty) {
            let mut discr_path = self.mir_projection_from_decision_path(path);
            discr_path.push(MirProjection::Discriminant);
            let place = Place::new(scrutinee_value, discr_path, addr_space);
            return self.builder.body.alloc_value(ValueData {
                ty: self.u256_ty(),
                origin: ValueOrigin::PlaceLoad(place),
            });
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
        _match_expr: ExprId,
    ) -> (ValueId, ValueId) {
        // Empty path means we bind to the scrutinee itself
        if path.is_empty() {
            return (scrutinee_value, scrutinee_value);
        }

        // Compute the final type by walking the projection path
        let final_ty = self.compute_projection_result_type(scrutinee_ty, path);
        let is_aggregate = final_ty.field_count(self.db) > 0
            || final_ty.is_array(self.db)
            || final_ty
                .adt_ref(self.db)
                .is_some_and(|adt| matches!(adt, AdtRef::Enum(_)));

        // Track address space for the result
        let addr_space = self.value_address_space(scrutinee_value);

        // Create the Place
        let place = Place::new(
            scrutinee_value,
            self.mir_projection_from_decision_path(path),
            addr_space,
        );

        let place_ref_id = self.builder.body.alloc_value(ValueData {
            ty: final_ty,
            origin: ValueOrigin::PlaceRef(place.clone()),
        });
        self.value_address_space.insert(place_ref_id, addr_space);

        // Use PlaceRef for aggregates (pointer only), PlaceLoad for scalars (load value).
        // When aggregate, re-use the place ref as the "value".
        let value_id = if is_aggregate {
            place_ref_id
        } else {
            let loaded_id = self.builder.body.alloc_value(ValueData {
                ty: final_ty,
                origin: ValueOrigin::PlaceLoad(place),
            });
            self.value_address_space.insert(loaded_id, addr_space);
            loaded_id
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
