//! Match lowering for MIR: converts supported `match` expressions into switches and prepares
//! enum pattern bindings.

use super::*;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Lowers a `match` expression into a MIR `Switch`.
    ///
    /// # Parameters
    /// - `block`: Entry block for the match.
    /// - `match_expr`: Expression id of the match.
    /// - `scrutinee`: Scrutinee expression id.
    /// - `arms`: Match arms to lower.
    /// - `patterns`: Precomputed arm patterns.
    ///
    /// # Returns
    /// Successor merge block (if any) and the value representing the match expression.
    pub(super) fn lower_match_expr(
        &mut self,
        block: BasicBlockId,
        match_expr: ExprId,
        scrutinee: ExprId,
        arms: &[MatchArm],
        patterns: &mut [MatchArmPattern],
    ) -> (Option<BasicBlockId>, ValueId) {
        debug_assert_eq!(arms.len(), patterns.len());
        let (scrut_block_opt, discr_value) = self.lower_expr_in(block, scrutinee);
        let scrutinee_value = discr_value;
        let Some(scrut_block) = scrut_block_opt else {
            let value = self.ensure_value(match_expr);
            return (None, value);
        };

        let mut merge_block: Option<BasicBlockId> = None;
        let mut arm_blocks = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm_entry = self.alloc_block();
            let (arm_end, _) = self.lower_expr_in(arm_entry, arm.body);
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
                self.set_terminator(end_block, Terminator::Goto { target: merge });
            }
            arm_blocks.push((arm_entry, terminates));
        }

        let mut targets = Vec::new();
        let mut default_block = None;
        for (idx, pattern) in patterns.iter().enumerate() {
            let (block_id, _) = arm_blocks[idx];
            match pattern {
                MatchArmPattern::Literal(value) => targets.push(SwitchTarget {
                    value: value.clone(),
                    block: block_id,
                }),
                MatchArmPattern::Enum { variant_index, .. } => targets.push(SwitchTarget {
                    value: SwitchValue::Enum(*variant_index),
                    block: block_id,
                }),
                MatchArmPattern::Wildcard => default_block = Some(block_id),
            }
        }

        let default_block = default_block.unwrap_or_else(|| {
            let unreachable_block = self.alloc_block();
            self.set_terminator(unreachable_block, Terminator::Unreachable);
            unreachable_block
        });

        self.populate_enum_binding_values(patterns, scrutinee_value, match_expr);

        // For enum scrutinees, always switch on the in-memory discriminant instead of the heap
        // pointer, even for payload-less enums.
        let discr_value = if patterns
            .iter()
            .any(|pat| matches!(pat, MatchArmPattern::Enum { .. }))
        {
            let scrut_ty = self.typed_body.expr_ty(self.db, scrutinee);
            let (base_ty, _) = scrut_ty.decompose_ty_app(self.db);
            let is_address_space = base_ty == self.core.helper_ty(CoreHelperTy::AddressSpace);
            if let TyData::TyBase(TyBase::Adt(adt_def)) = base_ty.data(self.db)
                && matches!(adt_def.adt_ref(self.db), AdtRef::Enum(_))
                && !is_address_space
            {
                let callable =
                    self.core
                        .make_callable(match_expr, CoreHelper::GetDiscriminant, &[]);
                let space_value = self.synthetic_address_space_memory();
                let ty = callable.ret_ty(self.db);
                self.mir_body.alloc_value(ValueData {
                    ty,
                    origin: ValueOrigin::Call(CallOrigin {
                        expr: match_expr,
                        callable,
                        args: vec![scrutinee_value, space_value],
                        resolved_name: None,
                    }),
                })
            } else {
                discr_value
            }
        } else {
            discr_value
        };

        self.set_terminator(
            scrut_block,
            Terminator::Switch {
                discr: discr_value,
                targets,
                default: default_block,
                origin: SwitchOrigin::MatchExpr(match_expr),
            },
        );

        let arms_info = arms
            .iter()
            .zip(patterns.iter())
            .zip(arm_blocks.iter())
            .map(
                |((arm, pattern), (block_id, terminates))| MatchArmLowering {
                    pattern: pattern.clone(),
                    body: arm.body,
                    block: *block_id,
                    terminates: *terminates,
                },
            )
            .collect();

        self.mir_body.match_info.insert(
            match_expr,
            MatchLoweringInfo {
                scrutinee: scrutinee_value,
                merge_block,
                arms: arms_info,
            },
        );

        let value_id = self.ensure_value(match_expr);
        (merge_block, value_id)
    }

    /// Collects supported match arm patterns, rejecting unsupported combinations.
    ///
    /// # Parameters
    /// - `arms`: Match arms to analyze.
    ///
    /// # Returns
    /// Vector of patterns if supported, otherwise `None`.
    pub(super) fn match_arm_patterns(&self, arms: &[MatchArm]) -> Option<Vec<MatchArmPattern>> {
        if arms.is_empty() {
            return None;
        }

        let mut seen_values: FxHashSet<SwitchValue> = FxHashSet::default();
        let mut has_wildcard = false;
        let mut patterns = Vec::with_capacity(arms.len());

        for arm in arms {
            if self.is_wildcard_pat(arm.pat) {
                if has_wildcard {
                    return None;
                }
                has_wildcard = true;
                patterns.push(MatchArmPattern::Wildcard);
                continue;
            }

            if let Some(value) = self.literal_pat_value(arm.pat) {
                if !seen_values.insert(value.clone()) {
                    return None;
                }
                patterns.push(MatchArmPattern::Literal(value));
                continue;
            }

            if let Some(pattern) = self.enum_pat_value(arm.pat) {
                if let MatchArmPattern::Enum { variant_index, .. } = pattern
                    && !seen_values.insert(SwitchValue::Enum(variant_index))
                {
                    return None;
                }
                patterns.push(pattern);
                continue;
            }

            return None;
        }

        Some(patterns)
    }

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

    /// Populates enum pattern bindings with lowered `get_variant_field` calls.
    ///
    /// # Parameters
    /// - `patterns`: Patterns to update.
    /// - `scrutinee_value`: Value representing the enum scrutinee.
    /// - `match_expr`: Match expression id for context.
    ///
    /// # Returns
    /// Nothing; updates bindings in place.
    pub(super) fn populate_enum_binding_values(
        &mut self,
        patterns: &mut [MatchArmPattern],
        scrutinee_value: ValueId,
        match_expr: ExprId,
    ) {
        for pattern in patterns.iter_mut() {
            let MatchArmPattern::Enum { bindings, .. } = pattern else {
                continue;
            };
            for binding in bindings.iter_mut() {
                if binding.value.is_some() {
                    continue;
                }
                let binding_ty = self.typed_body.pat_ty(self.db, binding.pat_id);
                let callable =
                    self.core
                        .make_callable(match_expr, CoreHelper::GetVariantField, &[binding_ty]);
                let space_value =
                    self.address_space_literal(self.value_address_space(scrutinee_value));
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
}
