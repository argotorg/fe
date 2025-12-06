//! Match lowering for MIR: converts supported `match` expressions into switches and prepares
//! enum pattern bindings using decision trees for optimized codegen.

use hir::analysis::ty::{
    decision_tree::{Case, DecisionTree, LeafNode, Occurrence, SwitchNode, build_decision_tree},
    pattern_analysis::PatternMatrix,
    simplified_pattern::ConstructorKind,
};

use super::*;

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
        let scope = self.typed_body.body().unwrap().scope();

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
                bindings: Vec::new(),
            });
        }

        if let Pat::PathTuple(path, elem_pats) = pat_data
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

            let mut bindings = Vec::new();
            let mut offset: u64 = 0;
            for elem_pat in elem_pats {
                let elem_ty = self.typed_body.pat_ty(self.db, *elem_pat);
                let elem_size = self.ty_size_bytes(elem_ty).unwrap_or(32);

                if let Partial::Present(Pat::Path(elem_path, _)) = elem_pat.data(self.db, self.body)
                    && let Some(elem_path) = elem_path.to_opt()
                    && elem_path.is_bare_ident(self.db)
                {
                    bindings.push(PatternBinding {
                        pat_id: *elem_pat,
                        field_offset: offset,
                        value: None,
                    });
                }

                offset += elem_size;
            }

            return Some(MatchArmPattern::Enum {
                variant_index: variant.variant.idx as u64,
                enum_name,
                bindings,
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
    /// - `block`: Entry block for the match.
    /// - `match_expr`: Expression id of the match.
    /// - `scrutinee`: Scrutinee expression id.
    /// - `arms`: Match arms to lower.
    ///
    /// # Returns
    /// Successor merge block (if any) and the value representing the match expression.
    pub(super) fn lower_match_with_decision_tree(
        &mut self,
        block: BasicBlockId,
        match_expr: ExprId,
        scrutinee: ExprId,
        arms: &[MatchArm],
    ) -> (Option<BasicBlockId>, ValueId) {
        // Lower the scrutinee to get its value
        let (scrut_block_opt, scrutinee_value) = self.lower_expr_in(block, scrutinee);
        let Some(scrut_block) = scrut_block_opt else {
            let value = self.ensure_value(match_expr);
            return (None, value);
        };

        // Build pattern matrix from match arms
        let scrutinee_ty = self.typed_body.expr_ty(self.db, scrutinee);
        let scope = self.typed_body.body().unwrap().scope();

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
            // Some patterns couldn't be resolved, fall back to old behavior
            let value = self.ensure_value(match_expr);
            return (Some(scrut_block), value);
        }

        let matrix =
            PatternMatrix::from_hir_patterns(self.db, &patterns, self.body, scope, scrutinee_ty);

        // Build decision tree from pattern matrix
        let tree = build_decision_tree(self.db, &matrix);

        // Lower the decision tree to MIR
        let merge_block = self.alloc_block();

        // Collect arm info for codegen (needed for match_info)
        let mut arms_info: Vec<MatchArmLowering> = arms
            .iter()
            .map(|arm| {
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
                }
            })
            .collect();

        // Populate enum bindings with get_variant_field calls
        let arms_info_mut: &mut [MatchArmLowering] = &mut arms_info;
        let patterns_mut: Vec<&mut MatchArmPattern> = arms_info_mut
            .iter_mut()
            .map(|arm| &mut arm.pattern)
            .collect();
        for pattern in patterns_mut {
            if let MatchArmPattern::Enum { bindings, .. } = pattern {
                for binding in bindings.iter_mut() {
                    if binding.value.is_some() {
                        continue;
                    }
                    let binding_ty = self.typed_body.pat_ty(self.db, binding.pat_id);
                    let callable = self.core.make_callable(
                        match_expr,
                        CoreHelper::GetVariantField,
                        &[binding_ty],
                    );
                    let space_value = self.synthetic_address_space_memory();
                    let offset_value = self.synthetic_u256(BigUint::from(binding.field_offset));
                    let load_value = self.mir_body.alloc_value(ValueData {
                        ty: binding_ty,
                        origin: ValueOrigin::Call(CallOrigin {
                            expr: match_expr,
                            callable,
                            args: vec![scrutinee_value, space_value, offset_value],
                            resolved_name: None,
                        }),
                    });
                    binding.value = Some(load_value);
                }
            }
        }

        // Store match info for codegen
        self.mir_body.match_info.insert(
            match_expr,
            MatchLoweringInfo {
                scrutinee: scrutinee_value,
                arms: arms_info,
            },
        );

        let tree_entry = self.lower_decision_tree(
            &tree,
            scrutinee_value,
            scrutinee_ty,
            arms,
            match_expr,
            Some(merge_block),
        );

        // Set scrut_block to jump to the tree entry
        self.set_terminator(scrut_block, Terminator::Goto { target: tree_entry });

        let value_id = self.ensure_value(match_expr);
        (Some(merge_block), value_id)
    }

    /// Recursively lowers a decision tree to MIR basic blocks.
    ///
    /// # Parameters
    /// - `tree`: Decision tree node to lower.
    /// - `scrutinee_value`: Value representing the root scrutinee.
    /// - `scrutinee_ty`: Type of the scrutinee.
    /// - `arms`: Original match arms.
    /// - `match_expr`: The match expression id.
    /// - `merge_block`: Optional merge block for match results.
    ///
    /// # Returns
    /// The entry basic block for this tree node.
    fn lower_decision_tree(
        &mut self,
        tree: &DecisionTree<'db>,
        scrutinee_value: ValueId,
        scrutinee_ty: TyId<'db>,
        arms: &[MatchArm],
        match_expr: ExprId,
        merge_block: Option<BasicBlockId>,
    ) -> BasicBlockId {
        match tree {
            DecisionTree::Leaf(leaf) => self.lower_leaf_node(leaf, arms, match_expr, merge_block),
            DecisionTree::Switch(switch_node) => self.lower_switch_node(
                switch_node,
                scrutinee_value,
                scrutinee_ty,
                arms,
                match_expr,
                merge_block,
            ),
        }
    }

    /// Lowers a leaf node (match arm execution) to a basic block.
    fn lower_leaf_node(
        &mut self,
        leaf: &LeafNode<'db>,
        arms: &[MatchArm],
        _match_expr: ExprId,
        merge_block: Option<BasicBlockId>,
    ) -> BasicBlockId {
        let arm = &arms[leaf.arm_index];
        let arm_block = self.alloc_block();

        // TODO: Handle bindings from leaf.bindings using Occurrence paths

        let (arm_end, _) = self.lower_expr_in(arm_block, arm.body);

        if let Some(end_block) = arm_end
            && let Some(merge) = merge_block
        {
            self.set_terminator(end_block, Terminator::Goto { target: merge });
        }

        arm_block
    }

    /// Lowers a switch node (test and branch) to MIR basic blocks.
    fn lower_switch_node(
        &mut self,
        switch_node: &SwitchNode<'db>,
        scrutinee_value: ValueId,
        scrutinee_ty: TyId<'db>,
        arms: &[MatchArm],
        match_expr: ExprId,
        merge_block: Option<BasicBlockId>,
    ) -> BasicBlockId {
        let test_block = self.alloc_block();

        // Extract the value to test based on the occurrence path
        let test_value = self.lower_occurrence(
            &switch_node.occurrence,
            scrutinee_value,
            scrutinee_ty,
            match_expr,
        );

        // Recursively lower each case
        let mut targets = vec![];
        let mut default_block = None;

        for (case, subtree) in &switch_node.arms {
            let subtree_entry = self.lower_decision_tree(
                subtree,
                scrutinee_value,
                scrutinee_ty,
                arms,
                match_expr,
                merge_block,
            );

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

        let default = default_block.unwrap_or_else(|| {
            let unreachable = self.alloc_block();
            self.set_terminator(unreachable, Terminator::Unreachable);
            unreachable
        });

        // Use MatchExpr origin only for the root switch (when occurrence is empty)
        let origin = if switch_node.occurrence.0.is_empty() {
            SwitchOrigin::MatchExpr(match_expr)
        } else {
            SwitchOrigin::None
        };

        self.set_terminator(
            test_block,
            Terminator::Switch {
                discr: test_value,
                targets,
                default,
                origin,
            },
        );

        test_block
    }

    /// Extracts a value from the scrutinee based on an occurrence path.
    ///
    /// An Occurrence like [0, 1] means "get field 0, then field 1" from the root scrutinee.
    fn lower_occurrence(
        &mut self,
        occurrence: &Occurrence,
        scrutinee_value: ValueId,
        scrutinee_ty: TyId<'db>,
        match_expr: ExprId,
    ) -> ValueId {
        if occurrence.0.is_empty() {
            // Root occurrence - check if we need to extract discriminant for enums
            if let TyData::TyBase(TyBase::Adt(adt_def)) = scrutinee_ty.data(self.db)
                && matches!(adt_def.adt_ref(self.db), AdtRef::Enum(_))
            {
                let has_payload = adt_def
                    .fields(self.db)
                    .iter()
                    .any(|variant| variant.num_types() > 0);
                if has_payload {
                    // For enums with payloads, extract the discriminant
                    let callable =
                        self.core
                            .make_callable(match_expr, CoreHelper::GetDiscriminant, &[]);
                    let space_value = self.synthetic_address_space_memory();
                    let ty = callable.ret_ty(self.db);
                    return self.mir_body.alloc_value(ValueData {
                        ty,
                        origin: ValueOrigin::Call(CallOrigin {
                            expr: match_expr,
                            callable,
                            args: vec![scrutinee_value, space_value],
                            resolved_name: None,
                        }),
                    });
                }
            }
            return scrutinee_value;
        }

        // Traverse the occurrence path, extracting fields at each step
        let mut current_value = scrutinee_value;
        let mut current_ty = scrutinee_ty;

        for &field_idx in &occurrence.0 {
            // Get field type and offset
            let field_types = current_ty.field_types(self.db);
            if field_idx >= field_types.len() {
                // Invalid index, return current value as fallback
                return current_value;
            }

            let field_ty = field_types[field_idx];

            // Calculate byte offset to this field
            let mut offset_bytes = 0u64;
            for field_ty in field_types.iter().take(field_idx) {
                if let Some(size) = self.ty_size_bytes(*field_ty) {
                    offset_bytes += size;
                }
            }

            // Generate GetField call
            let callable =
                self.core
                    .make_callable(match_expr, CoreHelper::GetField, &[current_ty, field_ty]);
            let space_value = self.synthetic_address_space_memory();
            let offset_value = self.synthetic_u256(BigUint::from(offset_bytes));

            current_value = self.mir_body.alloc_value(ValueData {
                ty: field_ty,
                origin: ValueOrigin::Call(CallOrigin {
                    expr: match_expr,
                    callable,
                    args: vec![current_value, space_value, offset_value],
                    resolved_name: None,
                }),
            });

            current_ty = field_ty;
        }

        // At the end, check if we need discriminant extraction for the final type
        if let TyData::TyBase(TyBase::Adt(adt_def)) = current_ty.data(self.db)
            && matches!(adt_def.adt_ref(self.db), AdtRef::Enum(_))
        {
            let has_payload = adt_def
                .fields(self.db)
                .iter()
                .any(|variant| variant.num_types() > 0);
            if has_payload {
                let callable =
                    self.core
                        .make_callable(match_expr, CoreHelper::GetDiscriminant, &[]);
                let space_value = self.synthetic_address_space_memory();
                let ty = callable.ret_ty(self.db);
                return self.mir_body.alloc_value(ValueData {
                    ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr: match_expr,
                        callable,
                        args: vec![current_value, space_value],
                        resolved_name: None,
                    }),
                });
            }
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
}
