//! Expression and statement lowering for MIR: handles blocks, control flow, calls, and dispatches
//! to specialized lowering helpers.

use hir::{
    analysis::ty::ty_check::ResolvedEffectArg,
    projection::{IndexSource, Projection},
};

use hir::analysis::ty::{
    binder::Binder,
    fold::{AssocTySubst, TyFoldable},
    normalize::normalize_ty,
};

use crate::{
    ir::{Place, Rvalue},
    layout::{self, ty_storage_slots},
};

use super::*;
use hir::analysis::{
    place::PlaceBase,
    ty::ty_check::{EffectArg, EffectPassMode},
};
use hir::hir_def::expr::BinOp;

enum RootLvalue<'db> {
    Place(Place<'db>),
    Local(LocalId),
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Try to lower a `size_of<T>()` or `encoded_size<T>()` call to a constant.
    fn try_lower_size_intrinsic_call(&mut self, expr: ExprId) -> Option<ValueId> {
        let callable = self.typed_body.callable_expr(expr)?;
        let ingot_kind = callable.callable_def.ingot(self.db).kind(self.db);
        let name = callable.callable_def.name(self.db)?;

        // Get the type argument from the callable's generic args
        let ty = *callable.generic_args().first()?;

        let size_bytes = match (ingot_kind, name.data(self.db).as_str()) {
            (IngotKind::Core, "size_of") => layout::ty_size_bytes(self.db, ty)?,
            (IngotKind::Std, "encoded_size") => self.abi_static_size_bytes(ty)?,
            _ => return None,
        };

        let value_id = self.ensure_value(expr);
        self.builder.body.values[value_id.index()].origin =
            ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(size_bytes)));
        Some(value_id)
    }

    fn u256_lit_from_expr(&self, expr: ExprId) -> Option<BigUint> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Lit(LitKind::Int(int_id))) => Some(int_id.data(self.db).clone()),
            _ => None,
        }
    }

    fn contract_field_slot_offset(&self, contract_name: &str, field_idx: usize) -> Option<usize> {
        let top_mod = self.body.top_mod(self.db);
        let contract = top_mod
            .all_contracts(self.db)
            .iter()
            .copied()
            .find(|contract| {
                contract
                    .name(self.db)
                    .to_opt()
                    .is_some_and(|id| id.data(self.db) == contract_name)
            })?;

        let fields = contract.fields(self.db);
        let field = fields.get_index(field_idx)?.1;
        let desired_space = if field.is_provider {
            self.effect_provider_space_for_provider_ty(field.declared_ty)?
        } else {
            AddressSpaceKind::Storage
        };

        let mut offset = 0;
        for field in fields.values().take(field_idx) {
            let space = if field.is_provider {
                self.effect_provider_space_for_provider_ty(field.declared_ty)?
            } else {
                AddressSpaceKind::Storage
            };
            if space != desired_space {
                continue;
            }
            offset += ty_storage_slots(self.db, field.target_ty)?;
        }
        Some(offset)
    }

    /// Lowers the body root expression, starting from the current block.
    ///
    /// # Parameters
    /// - `expr`: Root expression id of the body.
    pub(super) fn lower_root(&mut self, expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };

        self.move_to_block(block);
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => self.lower_block_expr(stmts),
            _ => {
                let value = self.lower_expr(expr);
                self.builder.body.expr_values.insert(expr, value);
            }
        }
    }

    /// Lowers a block expression by sequentially lowering its statements.
    ///
    /// # Parameters
    /// - `stmts`: Statements contained in the block.
    pub(super) fn lower_block(&mut self, stmts: &[StmtId]) {
        for &stmt_id in stmts {
            if self.current_block().is_none() {
                break;
            }
            self.lower_stmt(stmt_id);
        }
    }

    fn lower_block_expr(&mut self, stmts: &[StmtId]) {
        if stmts.is_empty() {
            return;
        }
        let (head, last) = stmts.split_at(stmts.len() - 1);
        self.lower_block(head);
        if self.current_block().is_none() {
            return;
        }
        let stmt_id = last[0];
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return;
        };
        if let Stmt::Expr(expr) = stmt {
            let ty = self.typed_body.expr_ty(self.db, *expr);
            if self.is_unit_ty(ty) {
                self.lower_expr_stmt(stmt_id, *expr);
            } else {
                let _ = self.lower_expr(*expr);
            }
        } else {
            self.lower_stmt(stmt_id);
        }
    }

    /// Lowers an expression, emitting any required control flow and side effects.
    ///
    /// # Parameters
    /// - `expr`: Expression id to lower.
    ///
    /// # Returns
    /// The value representing the expression.
    pub(super) fn lower_expr(&mut self, expr: ExprId) -> ValueId {
        if self.current_block().is_none() {
            return self.ensure_value(expr);
        }

        if let Some(value) = self.try_lower_variant_ctor(expr, None) {
            return value;
        }
        if let Some(value) = self.try_lower_unit_variant(expr, None) {
            return value;
        }

        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                self.lower_block_expr(stmts);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::With(bindings, body_expr)) => {
                for binding in bindings {
                    if self.current_block().is_none() {
                        break;
                    }
                    let value = self.lower_expr(binding.value);
                    if self.current_block().is_some() {
                        let ty = self.typed_body.expr_ty(self.db, binding.value);
                        if self.is_unit_ty(ty) {
                            self.assign(None, None, Rvalue::Value(value));
                        } else {
                            self.push_inst_here(MirInst::BindValue { value });
                        }
                    }
                }

                let value = self.lower_expr(*body_expr);
                self.builder.body.expr_values.insert(expr, value);
                value
            }
            Partial::Present(Expr::RecordInit(_, fields)) => self.try_lower_record(expr, fields),
            Partial::Present(Expr::Tuple(elems)) => self.try_lower_tuple(expr, elems),
            Partial::Present(Expr::Array(elems)) => self.try_lower_array(expr, elems),
            Partial::Present(Expr::ArrayRep(elem, len)) => {
                self.try_lower_array_rep(expr, *elem, *len)
            }
            Partial::Present(Expr::Match(scrutinee, arms)) => {
                if let Partial::Present(arms) = arms {
                    // Try decision tree lowering first
                    return self.lower_match_with_decision_tree(expr, *scrutinee, arms);
                }
                self.ensure_value(expr)
            }
            Partial::Present(Expr::If(cond, then_expr, else_expr)) => {
                self.lower_if(expr, *cond, *then_expr, *else_expr)
            }
            Partial::Present(Expr::Call(callee, call_args)) => {
                let _ = callee;
                let _ = call_args;
                self.lower_call_expr(expr)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                let _ = receiver;
                let _ = call_args;
                self.lower_call_expr(expr)
            }
            Partial::Present(Expr::Un(inner, _)) => {
                let _ = self.lower_expr(*inner);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::Cast(inner, _)) => {
                let _ = self.lower_expr(*inner);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                self.lower_index_expr(expr, *lhs, *rhs)
            }
            Partial::Present(Expr::Bin(lhs, rhs, _)) => {
                let _ = self.lower_expr(*lhs);
                let _ = self.lower_expr(*rhs);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                self.lower_field_expr(expr, *lhs, *field_index)
            }
            Partial::Present(Expr::Path(_)) => self.lower_path_expr(expr),
            Partial::Present(Expr::Assign(_, _) | Expr::AugAssign(_, _, _)) => {
                // Assignment expressions are expected to be lowered in statement position.
                self.ensure_value(expr)
            }
            _ => self.ensure_value(expr),
        }
    }

    fn lower_call_expr(&mut self, expr: ExprId) -> ValueId {
        self.lower_call_expr_inner(expr, None, None)
    }

    fn lower_call_expr_inner(
        &mut self,
        expr: ExprId,
        dest_override: Option<LocalId>,
        stmt: Option<StmtId>,
    ) -> ValueId {
        if let Some(value_id) = self.try_lower_intrinsic_stmt(expr) {
            return value_id;
        }

        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let ty = self.typed_body.expr_ty(self.db, expr);
        let returns_value = !self.is_unit_ty(ty) && !ty.is_never(self.db);

        if let Some(value_id) = self.try_lower_size_intrinsic_call(expr) {
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }
        let Some(mut callable) = self.typed_body.callable_expr(expr).cloned() else {
            return value_id;
        };
        let callable_def = callable.callable_def;

        let Some((args, arg_exprs)) = self.collect_call_args(expr) else {
            return value_id;
        };

        let provider_space = self.effect_provider_space_for_provider_ty(ty);
        let result_space = provider_space.unwrap_or_else(|| self.expr_address_space(expr));

        if matches!(
            callable_def.ingot(self.db).kind(self.db),
            IngotKind::Core | IngotKind::Std
        ) && callable_def
            .name(self.db)
            .is_some_and(|name| name.data(self.db) == "contract_field_slot")
            && let Some(func) = self.hir_func
            && let Some(contract_fn) = extract_contract_function(self.db, func)
            && let Some(arg_expr) = arg_exprs.first().copied()
            && let Some(field_idx) = self.u256_lit_from_expr(arg_expr)
            && let Some(field_idx) = field_idx.to_usize()
            && let Some(offset) =
                self.contract_field_slot_offset(&contract_fn.contract_name, field_idx)
        {
            self.builder.body.values[value_id.index()].origin =
                ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(offset)));
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        if self.is_cast_intrinsic(callable_def) {
            let mut cast_args = args.clone();
            if self.is_method_call(expr) && !cast_args.is_empty() {
                cast_args.remove(0);
            }
            if cast_args.len() != 1 {
                return value_id;
            }
            let arg_value = cast_args[0];
            self.builder.body.values[value_id.index()].origin =
                ValueOrigin::TransparentCast { value: arg_value };
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        if let Some(op) = self.intrinsic_kind(callable_def) {
            let mut intrinsic_args = args.clone();
            if self.is_method_call(expr) && !intrinsic_args.is_empty() {
                intrinsic_args.remove(0);
            }
            if op.returns_value() {
                // Intrinsics are word-producing operations, but some std/core APIs wrap them
                // in single-field structs (e.g. `Address { inner: caller() }`).
                //
                // When such a wrapper is lowered as an intrinsic (by name), materialize the
                // returned word into the destination aggregate so downstream code sees the
                // expected by-ref representation.
                if self.is_by_ref_ty(ty) {
                    let field_tys = ty.field_types(self.db);
                    let size_bytes = layout::ty_size_bytes(self.db, ty).unwrap_or(0);
                    if field_tys.len() != 1 || size_bytes != layout::WORD_SIZE_BYTES {
                        panic!(
                            "intrinsic `{:?}` used with unsupported by-ref type `{}`",
                            op,
                            ty.pretty_print(self.db)
                        );
                    }

                    let field_ty = field_tys[0];
                    if self.is_by_ref_ty(field_ty) {
                        panic!(
                            "intrinsic `{:?}` used with nested by-ref field type `{}`",
                            op,
                            field_ty.pretty_print(self.db)
                        );
                    }

                    let dest =
                        dest_override.unwrap_or_else(|| self.alloc_temp_local(ty, false, "intr"));
                    self.builder.body.locals[dest.index()].address_space = AddressSpaceKind::Memory;
                    self.assign(
                        stmt,
                        Some(dest),
                        Rvalue::Alloc {
                            address_space: AddressSpaceKind::Memory,
                        },
                    );

                    // Compute the intrinsic word into a temp local.
                    let word_local = self.alloc_temp_local(field_ty, false, "intrw");
                    self.assign(
                        stmt,
                        Some(word_local),
                        Rvalue::Intrinsic {
                            op,
                            args: intrinsic_args,
                        },
                    );
                    let word_value =
                        self.alloc_value(field_ty, ValueOrigin::Local(word_local), ValueRepr::Word);

                    // Store the word into the single field at offset 0.
                    self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                    self.builder.body.values[value_id.index()].repr =
                        ValueRepr::Ref(AddressSpaceKind::Memory);
                    let place = Place::new(
                        value_id,
                        MirProjectionPath::from_projection(Projection::Field(0)),
                    );
                    self.push_inst_here(MirInst::Store {
                        place,
                        value: word_value,
                    });
                    return value_id;
                }

                let dest =
                    dest_override.unwrap_or_else(|| self.alloc_temp_local(ty, false, "intr"));
                self.builder.body.locals[dest.index()].address_space = result_space;
                self.assign(
                    stmt,
                    Some(dest),
                    Rvalue::Intrinsic {
                        op,
                        args: intrinsic_args,
                    },
                );
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                return value_id;
            }

            // Statement-only intrinsics are handled via `try_lower_intrinsic_stmt` above.
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Unit;
            return value_id;
        }

        let mut receiver_space = None;
        if self.is_method_call(expr) && !args.is_empty() {
            let needs_space = callable
                .callable_def
                .receiver_ty(self.db)
                .is_some_and(|binder| {
                    let ty = binder.instantiate_identity();
                    self.value_repr_for_ty(ty, AddressSpaceKind::Memory)
                        .address_space()
                        .is_some()
                });
            if needs_space {
                receiver_space = Some(self.value_address_space(args[0]));
            }
        }

        let mut effect_args = Vec::new();
        let mut effect_writebacks: Vec<(LocalId, Place<'db>)> = Vec::new();
        if let CallableDef::Func(func_def) = callable.callable_def
            && func_def.has_effects(self.db)
            && extract_contract_function(self.db, func_def).is_none()
            && let Some(resolved) = self.typed_body.call_effect_args(expr)
        {
            self.finalize_place_effect_provider_args_for_call(func_def, &mut callable, resolved);
            for resolved_arg in resolved {
                let value = self.lower_effect_arg(resolved_arg, &mut effect_writebacks);
                effect_args.push(value);
            }
        }

        let dest = if returns_value {
            dest_override.or_else(|| Some(self.alloc_temp_local(ty, false, "call")))
        } else {
            None
        };
        if let Some(dest) = dest {
            self.builder.body.locals[dest.index()].address_space = result_space;
        }
        let hir_target = crate::ir::HirCallTarget {
            callable_def: callable.callable_def,
            generic_args: callable.generic_args().to_vec(),
            trait_inst: callable.trait_inst(),
        };
        let call_origin = CallOrigin {
            expr: Some(expr),
            hir_target: Some(hir_target),
            args,
            effect_args,
            resolved_name: None,
            receiver_space,
        };
        if ty.is_never(self.db) {
            self.set_current_terminator(Terminator::TerminatingCall(
                crate::ir::TerminatingCall::Call(call_origin),
            ));
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Unit;
            return value_id;
        }
        self.assign(stmt, dest, Rvalue::Call(call_origin));
        for (dest, place) in effect_writebacks {
            self.assign(None, Some(dest), Rvalue::Load { place });
        }
        self.builder.body.values[value_id.index()].origin =
            dest.map(ValueOrigin::Local).unwrap_or(ValueOrigin::Unit);
        value_id
    }

    fn finalize_place_effect_provider_args_for_call(
        &mut self,
        callee: Func<'db>,
        callable: &mut hir::analysis::ty::ty_check::Callable<'db>,
        resolved: &[ResolvedEffectArg<'db>],
    ) {
        let assumptions = PredicateListId::empty_list(self.db);
        let provider_arg_idx_by_effect =
            hir::analysis::ty::effects::place_effect_provider_param_index_map(self.db, callee);
        let caller_provider_arg_idx_by_effect = self.hir_func.map(|func| {
            hir::analysis::ty::effects::place_effect_provider_param_index_map(self.db, func)
        });

        for resolved_arg in resolved {
            let Some(effect_view) = callee.effect_params(self.db).nth(resolved_arg.param_idx)
            else {
                continue;
            };
            let effect_idx = effect_view.index();
            let Some(provider_arg_idx) = provider_arg_idx_by_effect
                .get(effect_idx)
                .copied()
                .flatten()
            else {
                continue;
            };

            // Don't stomp explicit provider arguments (HIR unifies those already).
            if let Some(existing) = callable.generic_args().get(provider_arg_idx).copied()
                && !matches!(existing.data(self.db), TyData::TyVar(_))
            {
                continue;
            }

            let Some(key_path) = effect_view.key_path(self.db) else {
                continue;
            };
            let Ok(path_res) = resolve_path(self.db, key_path, callee.scope(), assumptions, false)
            else {
                continue;
            };
            let base_target_ty = match path_res {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) if ty.is_star_kind(self.db) => ty,
                _ => continue,
            };

            let mut target_ty =
                Binder::bind(base_target_ty).instantiate(self.db, callable.generic_args());
            if let Some(inst) = callable.trait_inst() {
                let mut subst = AssocTySubst::new(inst);
                target_ty = target_ty.fold_with(self.db, &mut subst);
            }
            target_ty = normalize_ty(self.db, target_ty, callee.scope(), assumptions);

            let inferred_provider_ty = match resolved_arg.pass_mode {
                EffectPassMode::ByTempPlace => {
                    TyId::app(self.db, self.core.mem_ptr_ctor, target_ty)
                }
                EffectPassMode::ByPlace => {
                    let provider_for_effect_param_binding =
                        |this: &Self, binding: LocalBinding<'db>| -> Option<TyId<'db>> {
                            let LocalBinding::EffectParam { site, idx, .. } = binding else {
                                return None;
                            };
                            let current_func = this.hir_func?;
                            let EffectParamSite::Func(binding_func) = site else {
                                return None;
                            };
                            if binding_func != current_func {
                                return None;
                            }
                            let caller_provider_arg_idx_by_effect =
                                caller_provider_arg_idx_by_effect?;
                            let provider_idx = caller_provider_arg_idx_by_effect
                                .get(idx)
                                .copied()
                                .flatten()?;
                            if let Some(concrete) = this.generic_args.get(provider_idx).copied() {
                                return Some(concrete);
                            }
                            CallableDef::Func(current_func)
                                .params(this.db)
                                .get(provider_idx)
                                .copied()
                        };

                    let EffectArg::Place(place) = &resolved_arg.arg else {
                        continue;
                    };
                    let PlaceBase::Binding(binding) = place.base;
                    match binding {
                        binding @ LocalBinding::EffectParam { .. } => {
                            provider_for_effect_param_binding(self, binding).unwrap_or_else(|| {
                                TyId::app(self.db, self.core.mem_ptr_ctor, target_ty)
                            })
                        }
                        LocalBinding::Param {
                            site: ParamSite::EffectField(effect_site),
                            idx,
                            ..
                        } => self
                            .contract_field_provider_ty_for_effect_site(effect_site, idx)
                            .unwrap_or_else(|| {
                                TyId::app(self.db, self.core.stor_ptr_ctor, target_ty)
                            }),
                        _ => TyId::app(self.db, self.core.mem_ptr_ctor, target_ty),
                    }
                }
                _ => continue,
            };

            if let Some(slot) = callable.generic_args_mut().get_mut(provider_arg_idx) {
                *slot = inferred_provider_ty;
            }
        }
    }

    fn materialize_value_in_temp_place(
        &mut self,
        ty: TyId<'db>,
        value: ValueId,
        addr_space: AddressSpaceKind,
        hint: &str,
    ) -> (ValueId, Place<'db>) {
        let addr_local = self.alloc_temp_local(ty, false, hint);
        self.builder.body.locals[addr_local.index()].address_space = addr_space;
        self.assign(
            None,
            Some(addr_local),
            Rvalue::Alloc {
                address_space: addr_space,
            },
        );

        let addr_value = self.alloc_value(
            ty,
            ValueOrigin::Local(addr_local),
            ValueRepr::Ref(addr_space),
        );
        let place = Place::new(addr_value, crate::ir::MirProjectionPath::new());
        self.push_inst_here(MirInst::Store {
            place: place.clone(),
            value,
        });
        (addr_value, place)
    }

    fn lower_effect_arg(
        &mut self,
        resolved_arg: &ResolvedEffectArg<'db>,
        effect_writebacks: &mut Vec<(LocalId, Place<'db>)>,
    ) -> ValueId {
        // Handle ByPlace: resolve binding and materialize as needed
        if resolved_arg.pass_mode == EffectPassMode::ByPlace {
            let EffectArg::Place(place) = &resolved_arg.arg else {
                return self.default_effect_arg();
            };
            let PlaceBase::Binding(binding) = place.base;

            let addr_space = self.address_space_for_binding(&binding);
            let is_non_memory = !matches!(addr_space, AddressSpaceKind::Memory);

            // EffectParam: just get the binding value
            if matches!(binding, LocalBinding::EffectParam { .. }) {
                let value = self
                    .binding_value(binding)
                    .unwrap_or_else(|| self.synthetic_u256(BigUint::from(0u8)));
                return value;
            }

            let binding_ty = match binding {
                LocalBinding::Local { pat, .. } => self.typed_body.pat_ty(self.db, pat),
                LocalBinding::Param { ty, .. } => ty,
                LocalBinding::EffectParam { .. } => self.u256_ty(),
            };

            let Some(local) = self.local_for_binding(binding) else {
                return self.default_effect_arg();
            };

            let value_repr = self.value_repr_for_ty(binding_ty, addr_space);

            // Storage providers are addressable as handles even when their logical type is
            // word-represented (e.g. transparent newtypes around `u256`).
            if value_repr.address_space().is_some() || is_non_memory {
                let value = self.alloc_value(binding_ty, ValueOrigin::Local(local), value_repr);
                return value;
            }

            // Memory provider: materialize in temp place
            if matches!(addr_space, AddressSpaceKind::Memory) {
                let initial =
                    self.alloc_value(binding_ty, ValueOrigin::Local(local), ValueRepr::Word);
                let (addr_value, addr_place) = self.materialize_value_in_temp_place(
                    binding_ty,
                    initial,
                    AddressSpaceKind::Memory,
                    "eff",
                );
                if binding.is_mut() {
                    effect_writebacks.push((local, addr_place));
                }
                return addr_value;
            }

            return self.default_effect_arg();
        }

        // Handle ByTempPlace: lower value and materialize if needed
        if resolved_arg.pass_mode == EffectPassMode::ByTempPlace {
            let EffectArg::Value(expr_id) = &resolved_arg.arg else {
                return self.default_effect_arg();
            };

            let value = self.lower_expr(*expr_id);

            if self.builder.body.value(value).repr.is_ref() {
                return value;
            }

            let ty = self.typed_body.expr_ty(self.db, *expr_id);
            let (addr_value, _) = self.materialize_value_in_temp_place(
                ty,
                value,
                AddressSpaceKind::Memory,
                "eff_tmp",
            );
            return addr_value;
        }

        // Handle ByValue: lower the value directly
        if resolved_arg.pass_mode == EffectPassMode::ByValue {
            let value = match &resolved_arg.arg {
                EffectArg::Value(expr_id) => self.lower_expr(*expr_id),
                EffectArg::Binding(binding) => self
                    .binding_value(*binding)
                    .unwrap_or_else(|| self.synthetic_u256(BigUint::from(0u8))),
                EffectArg::Unknown | EffectArg::Place(_) => self.synthetic_u256(BigUint::from(0u8)),
            };

            return value;
        }

        // Unknown or any other case
        self.default_effect_arg()
    }

    fn default_effect_arg(&mut self) -> ValueId {
        self.synthetic_u256(BigUint::from(0u8))
    }

    fn lower_expr_into_local(&mut self, stmt: StmtId, expr: ExprId, dest: LocalId) -> ValueId {
        if self.current_block().is_none() {
            return self.ensure_value(expr);
        }

        if let Some(value_id) = self.try_lower_variant_ctor(expr, Some(dest)) {
            return value_id;
        }
        if let Some(value_id) = self.try_lower_unit_variant(expr, Some(dest)) {
            return value_id;
        }

        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(..) | Expr::MethodCall(..)) => {
                self.lower_call_expr_inner(expr, Some(dest), Some(stmt))
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let value_id = self.ensure_value(expr);
                let Some(field_index) = field_index.to_opt() else {
                    return value_id;
                };
                let base_value = self.lower_expr(*lhs);
                if self.current_block().is_none() {
                    return value_id;
                }
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let Some(info) = self.field_access_info(lhs_ty, field_index) else {
                    return value_id;
                };

                // Transparent newtype access: field 0 is a representation-preserving cast.
                if info.field_idx == 0
                    && crate::repr::transparent_newtype_field_ty(self.db, lhs_ty).is_some()
                {
                    let base_repr = self.builder.body.value(base_value).repr;
                    if !base_repr.is_ref() {
                        let space = base_repr
                            .address_space()
                            .unwrap_or(AddressSpaceKind::Memory);
                        let field_repr = self.value_repr_for_ty(info.field_ty, space);
                        if field_repr.address_space().is_some() {
                            self.builder.body.locals[dest.index()].address_space =
                                self.value_address_space(base_value);
                        } else {
                            self.builder.body.locals[dest.index()].address_space =
                                self.expr_address_space(expr);
                        }
                        self.assign(Some(stmt), Some(dest), Rvalue::Value(base_value));
                        self.builder.body.values[value_id.index()].origin =
                            ValueOrigin::Local(dest);
                        self.builder.body.values[value_id.index()].repr = field_repr;
                        return value_id;
                    }
                }

                let addr_space = self.value_address_space(base_value);
                let place = Place::new(
                    base_value,
                    MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                );

                if self.is_by_ref_ty(info.field_ty) {
                    let place_value = self.alloc_value(
                        info.field_ty,
                        ValueOrigin::PlaceRef(place),
                        ValueRepr::Ref(addr_space),
                    );
                    self.builder.body.locals[dest.index()].address_space = addr_space;
                    self.assign(Some(stmt), Some(dest), Rvalue::Value(place_value));
                    self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                    self.builder.body.values[value_id.index()].repr = ValueRepr::Ref(addr_space);
                    return value_id;
                }

                self.builder.body.locals[dest.index()].address_space =
                    self.expr_address_space(expr);
                self.assign(Some(stmt), Some(dest), Rvalue::Load { place });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                value_id
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let value_id = self.ensure_value(expr);
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                if !lhs_ty.is_array(self.db) {
                    return value_id;
                }
                let Some(elem_ty) = lhs_ty.generic_args(self.db).first().copied() else {
                    return value_id;
                };
                let base_value = self.lower_expr(*lhs);
                let index_value = self.lower_expr(*rhs);
                if self.current_block().is_none() {
                    return value_id;
                }
                let addr_space = self.value_address_space(base_value);
                let place = Place::new(
                    base_value,
                    MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                        index_value,
                    ))),
                );

                if self.is_by_ref_ty(elem_ty) {
                    let place_value = self.alloc_value(
                        elem_ty,
                        ValueOrigin::PlaceRef(place),
                        ValueRepr::Ref(addr_space),
                    );
                    self.builder.body.locals[dest.index()].address_space = addr_space;
                    self.assign(Some(stmt), Some(dest), Rvalue::Value(place_value));
                    self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                    self.builder.body.values[value_id.index()].repr = ValueRepr::Ref(addr_space);
                    return value_id;
                }

                self.builder.body.locals[dest.index()].address_space =
                    self.expr_address_space(expr);
                self.assign(Some(stmt), Some(dest), Rvalue::Load { place });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                value_id
            }
            _ => {
                let value_id = self.lower_expr(expr);
                if self.current_block().is_some() {
                    self.assign(Some(stmt), Some(dest), Rvalue::Value(value_id));
                }
                value_id
            }
        }
    }

    fn effect_param_key_is_trait(&self, binding: LocalBinding<'db>) -> bool {
        let LocalBinding::EffectParam { key_path, .. } = binding else {
            return false;
        };
        let Ok(res) = hir::analysis::name_resolution::path_resolver::resolve_path(
            self.db,
            key_path,
            self.owner.scope(),
            PredicateListId::empty_list(self.db),
            false,
        ) else {
            return false;
        };
        matches!(
            res,
            hir::analysis::name_resolution::path_resolver::PathRes::Trait(_)
                | hir::analysis::name_resolution::path_resolver::PathRes::TraitMethod(..)
        )
    }

    fn lower_path_expr(&mut self, expr: ExprId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }
        let Partial::Present(Expr::Path(_)) = expr.data(self.db, self.body) else {
            return value_id;
        };

        let ty = self.typed_body.expr_ty(self.db, expr);
        let prop = self.typed_body.expr_prop(self.db, expr);
        let Some(binding) = prop.binding else {
            return value_id;
        };
        let is_effect_binding = matches!(binding, LocalBinding::EffectParam { .. })
            || matches!(
                binding,
                LocalBinding::Param {
                    site: ParamSite::EffectField(_),
                    ..
                }
            );
        if !is_effect_binding {
            return value_id;
        }
        if matches!(binding, LocalBinding::EffectParam { .. })
            && self.effect_param_key_is_trait(binding)
        {
            return value_id;
        }

        // Effect params are passed as provider addresses/slots. When their logical type is
        // word-represented (including transparent-newtype wrappers), path expressions should load
        // the value from that place (rather than treating the provider address as the value).
        if !matches!(
            crate::repr::repr_kind_for_ty(self.db, &self.core, ty),
            crate::repr::ReprKind::Word
        ) {
            return value_id;
        }

        let Some(binding_local) = self.local_for_binding(binding) else {
            return value_id;
        };
        if !matches!(
            self.builder.body.value(value_id).origin,
            ValueOrigin::Local(local) if local == binding_local
        ) {
            return value_id;
        }

        let Some(place) = self.place_for_expr(expr) else {
            return value_id;
        };
        let dest = self.alloc_temp_local(ty, false, "load");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        self.builder.body.values[value_id.index()].repr = ValueRepr::Word;
        value_id
    }

    fn lower_field_expr(
        &mut self,
        expr: ExprId,
        lhs: ExprId,
        field_index: Partial<FieldIndex<'db>>,
    ) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }
        let Some(field_index) = field_index.to_opt() else {
            return value_id;
        };

        let base_value = self.lower_expr(lhs);
        if self.current_block().is_none() {
            return value_id;
        }

        let lhs_ty = self.typed_body.expr_ty(self.db, lhs);
        let Some(info) = self.field_access_info(lhs_ty, field_index) else {
            return value_id;
        };

        // Transparent newtype access: field 0 is a representation-preserving cast.
        if info.field_idx == 0
            && crate::repr::transparent_newtype_field_ty(self.db, lhs_ty).is_some()
        {
            let base_repr = self.builder.body.value(base_value).repr;
            if !base_repr.is_ref() {
                let space = base_repr
                    .address_space()
                    .unwrap_or(AddressSpaceKind::Memory);
                self.builder.body.values[value_id.index()].origin =
                    ValueOrigin::TransparentCast { value: base_value };
                self.builder.body.values[value_id.index()].repr =
                    self.value_repr_for_ty(info.field_ty, space);
                return value_id;
            }
        }

        let addr_space = self.value_address_space(base_value);
        let place = Place::new(
            base_value,
            MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
        );

        if self.is_by_ref_ty(info.field_ty) {
            self.builder.body.values[value_id.index()].origin = ValueOrigin::PlaceRef(place);
            self.builder.body.values[value_id.index()].repr = ValueRepr::Ref(addr_space);
            return value_id;
        }

        let dest = self.alloc_temp_local(info.field_ty, false, "load");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        value_id
    }

    fn lower_index_expr(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let lhs_ty = self.typed_body.expr_ty(self.db, lhs);
        if !lhs_ty.is_array(self.db) {
            return value_id;
        }
        let Some(elem_ty) = lhs_ty.generic_args(self.db).first().copied() else {
            return value_id;
        };

        let base_value = self.lower_expr(lhs);
        let index_value = self.lower_expr(rhs);
        if self.current_block().is_none() {
            return value_id;
        }

        let addr_space = self.value_address_space(base_value);
        let place = Place::new(
            base_value,
            MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                index_value,
            ))),
        );

        if self.is_by_ref_ty(elem_ty) {
            self.builder.body.values[value_id.index()].origin = ValueOrigin::PlaceRef(place);
            self.builder.body.values[value_id.index()].repr = ValueRepr::Ref(addr_space);
            return value_id;
        }

        let dest = self.alloc_temp_local(elem_ty, false, "load");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        value_id
    }

    /// Returns true if the expression is a method call (as opposed to a regular function call).
    fn is_method_call(&self, expr: ExprId) -> bool {
        let exprs = self.body.exprs(self.db);
        matches!(&exprs[expr], Partial::Present(Expr::MethodCall(..)))
    }

    // NOTE: field expressions are lowered via `lower_field_expr` so scalar loads become
    // explicit `MirInst::Load` instructions.

    // NOTE: array index expressions are lowered via `lower_index_expr` so scalar loads become
    // explicit `MirInst::Load` instructions.

    fn place_for_expr(&mut self, expr: ExprId) -> Option<Place<'db>> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Path(_)) => {
                let binding = self.typed_body.expr_prop(self.db, expr).binding?;
                match binding {
                    LocalBinding::EffectParam { .. } => {
                        if self.effect_param_key_is_trait(binding) {
                            return None;
                        }
                    }
                    LocalBinding::Param {
                        site: ParamSite::EffectField(_),
                        ..
                    } => {}
                    _ => return None,
                }

                let ty = self.typed_body.expr_ty(self.db, expr);
                match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
                    crate::repr::ReprKind::Zst | crate::repr::ReprKind::Ptr(_) => return None,
                    crate::repr::ReprKind::Word | crate::repr::ReprKind::Ref => {}
                }

                let local = self.local_for_binding(binding)?;
                let addr_space = self.address_space_for_binding(&binding);
                let base_value =
                    self.alloc_value(ty, ValueOrigin::Local(local), ValueRepr::Ref(addr_space));
                Some(Place::new(base_value, MirProjectionPath::new()))
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let field_index = field_index.to_opt()?;
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let info = self.field_access_info(lhs_ty, field_index)?;

                // Transparent newtypes: treat field 0 as the same place when the base is
                // already addressable, otherwise fall back to scalar newtype semantics.
                if info.field_idx == 0
                    && crate::repr::transparent_newtype_field_ty(self.db, lhs_ty).is_some()
                {
                    if let Some(place) = self.place_for_expr(*lhs) {
                        return Some(place);
                    }
                    let base_value = self.lower_expr(*lhs);
                    if self.builder.body.value(base_value).repr.is_ref() {
                        return Some(Place::new(base_value, MirProjectionPath::new()));
                    }
                    return None;
                }

                let addr_value = self.lower_expr(*lhs);
                Some(Place::new(
                    addr_value,
                    MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                ))
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                if !lhs_ty.is_array(self.db) {
                    return None;
                }
                let addr_value = self.lower_expr(*lhs);
                let index_value = self.lower_expr(*rhs);
                Some(Place::new(
                    addr_value,
                    MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                        index_value,
                    ))),
                ))
            }
            _ => None,
        }
    }

    fn peel_transparent_newtype_field0_lvalue(&self, mut expr: ExprId) -> ExprId {
        loop {
            let Partial::Present(Expr::Field(base, field_index)) = expr.data(self.db, self.body)
            else {
                return expr;
            };
            let Some(field_index) = field_index.to_opt() else {
                return expr;
            };
            let base_ty = self.typed_body.expr_ty(self.db, *base);
            let Some(info) = self.field_access_info(base_ty, field_index) else {
                return expr;
            };
            if info.field_idx == 0
                && crate::repr::transparent_newtype_field_ty(self.db, base_ty).is_some()
            {
                expr = *base;
                continue;
            }
            return expr;
        }
    }

    fn root_lvalue_for_expr(&mut self, expr: ExprId) -> Option<RootLvalue<'db>> {
        if let Some(place) = self.place_for_expr(expr) {
            return Some(RootLvalue::Place(place));
        }
        let binding = self.typed_body.expr_prop(self.db, expr).binding?;
        self.local_for_binding(binding).map(RootLvalue::Local)
    }

    fn store_to_root_lvalue(
        &mut self,
        stmt: Option<StmtId>,
        lvalue: RootLvalue<'db>,
        value: ValueId,
    ) {
        match lvalue {
            RootLvalue::Place(place) => {
                self.push_inst_here(MirInst::Store { place, value });
            }
            RootLvalue::Local(local) => {
                self.assign(stmt, Some(local), Rvalue::Value(value));
            }
        }
    }

    fn lower_assign_to_lvalue(&mut self, stmt_id: StmtId, target: ExprId, value: ValueId) {
        let peeled_target = self.peel_transparent_newtype_field0_lvalue(target);
        if peeled_target != target {
            let root_ty = self.typed_body.expr_ty(self.db, peeled_target);
            let wrapped = self.alloc_value(
                root_ty,
                ValueOrigin::TransparentCast { value },
                self.value_repr_for_expr(peeled_target, root_ty),
            );
            if let Some(lvalue) = self.root_lvalue_for_expr(peeled_target) {
                self.store_to_root_lvalue(Some(stmt_id), lvalue, wrapped);
                return;
            }
        }

        if let Some(lvalue) = self.root_lvalue_for_expr(target) {
            self.store_to_root_lvalue(Some(stmt_id), lvalue, value);
        }
    }

    fn lower_aug_assign_to_lvalue(
        &mut self,
        stmt_id: StmtId,
        target: ExprId,
        rhs_value: ValueId,
        op: hir::hir_def::expr::ArithBinOp,
    ) {
        let peeled_target = self.peel_transparent_newtype_field0_lvalue(target);
        let is_peeled = peeled_target != target;

        let root_expr = if is_peeled { peeled_target } else { target };
        let root_ty = self.typed_body.expr_ty(self.db, root_expr);
        let lhs_ty = self.typed_body.expr_ty(self.db, target);

        let Some(root_lvalue) = self.root_lvalue_for_expr(root_expr) else {
            return;
        };

        let lhs_value = match &root_lvalue {
            RootLvalue::Place(place) => {
                let loaded_local = self.alloc_temp_local(lhs_ty, false, "load");
                self.builder.body.locals[loaded_local.index()].address_space =
                    self.expr_address_space(target);
                self.assign(
                    None,
                    Some(loaded_local),
                    Rvalue::Load {
                        place: place.clone(),
                    },
                );
                self.alloc_value(lhs_ty, ValueOrigin::Local(loaded_local), ValueRepr::Word)
            }
            RootLvalue::Local(local) => {
                if is_peeled {
                    let base_value = self.alloc_value(
                        root_ty,
                        ValueOrigin::Local(*local),
                        self.value_repr_for_ty(
                            root_ty,
                            self.builder.body.local(*local).address_space,
                        ),
                    );
                    self.alloc_value(
                        lhs_ty,
                        ValueOrigin::TransparentCast { value: base_value },
                        ValueRepr::Word,
                    )
                } else {
                    self.alloc_value(lhs_ty, ValueOrigin::Local(*local), ValueRepr::Word)
                }
            }
        };

        let updated = self.alloc_value(
            lhs_ty,
            ValueOrigin::Binary {
                op: BinOp::Arith(op),
                lhs: lhs_value,
                rhs: rhs_value,
            },
            ValueRepr::Word,
        );

        let stored = if is_peeled {
            self.alloc_value(
                root_ty,
                ValueOrigin::TransparentCast { value: updated },
                self.value_repr_for_expr(root_expr, root_ty),
            )
        } else {
            updated
        };

        self.store_to_root_lvalue(Some(stmt_id), root_lvalue, stored);
    }

    /// Lowers a statement in the current block.
    ///
    /// # Parameters
    /// - `stmt_id`: Statement to lower.
    pub(super) fn lower_stmt(&mut self, stmt_id: StmtId) {
        let Some(block) = self.current_block() else {
            return;
        };
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return;
        };
        match stmt {
            Stmt::Let(pat, _ty, value) => {
                self.move_to_block(block);
                let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
                    return;
                };
                if self.current_block().is_none() {
                    return;
                }

                match pat_data {
                    Pat::Path(..) => {
                        let binding =
                            self.typed_body
                                .pat_binding(*pat)
                                .unwrap_or(LocalBinding::Local {
                                    pat: *pat,
                                    is_mut: matches!(pat_data, Pat::Path(_, true)),
                                });
                        let Some(local) = self.local_for_binding(binding) else {
                            return;
                        };
                        if let Some(expr) = value {
                            let value_id = self.lower_expr_into_local(stmt_id, *expr, local);
                            if self.current_block().is_none() {
                                return;
                            }
                            let pat_ty = self.typed_body.pat_ty(self.db, *pat);
                            if self
                                .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                                .address_space()
                                .is_some()
                            {
                                let space = self.value_address_space(value_id);
                                self.set_pat_address_space(*pat, space);
                            }
                        } else {
                            self.assign(Some(stmt_id), Some(local), Rvalue::ZeroInit);
                        }
                    }
                    Pat::WildCard | Pat::Rest => {
                        if let Some(expr) = value {
                            let value_id = self.lower_expr(*expr);
                            if self.current_block().is_some() {
                                self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                            }
                        }
                    }
                    Pat::Tuple(_) | Pat::PathTuple(_, _) | Pat::Record(_, _) => {
                        let Some(expr) = value else {
                            return;
                        };
                        let value_id = self.lower_expr(*expr);
                        if self.current_block().is_some() {
                            self.bind_pat_value(*pat, value_id);
                        }
                    }
                    _ => {
                        if let Some(expr) = value {
                            let value_id = self.lower_expr(*expr);
                            if self.current_block().is_some() {
                                self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                            }
                        }
                    }
                }
            }
            Stmt::For(_, _, _) => {
                panic!("for loops are not supported in MIR lowering yet");
            }
            Stmt::While(cond, body_expr) => self.lower_while(*cond, *body_expr),
            Stmt::Continue => {
                let scope = self.loop_stack.last().expect("continue outside of loop");
                self.goto(scope.continue_target);
            }
            Stmt::Break => {
                let scope = self.loop_stack.last().expect("break outside of loop");
                self.goto(scope.break_target);
            }
            Stmt::Return(value) => {
                self.move_to_block(block);
                if let Some(expr) = value {
                    let ret_ty = self.return_ty;
                    let returns_value = !self.is_unit_ty(ret_ty) && !ret_ty.is_never(self.db);
                    if returns_value {
                        let ret_value = Some(self.lower_expr(*expr));
                        if self.current_block().is_some() {
                            self.set_current_terminator(Terminator::Return(ret_value));
                        }
                    } else {
                        self.lower_expr_stmt(stmt_id, *expr);
                        if self.current_block().is_some() {
                            self.set_current_terminator(Terminator::Return(None));
                        }
                    }
                } else if self.current_block().is_some() {
                    self.set_current_terminator(Terminator::Return(None));
                }
            }
            Stmt::Expr(expr) => self.lower_expr_stmt(stmt_id, *expr),
        }
    }

    /// Lowers a `while` loop statement and wires its control-flow edges.
    ///
    /// # Parameters
    /// - `cond_expr`: Condition expression id.
    /// - `body_expr`: Loop body expression id.
    ///
    pub(super) fn lower_while(&mut self, cond_expr: ExprId, body_expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let exit_block = self.alloc_block();

        self.move_to_block(block);
        self.goto(cond_entry);

        self.move_to_block(cond_entry);
        let cond_val = self.lower_expr(cond_expr);
        let Some(cond_header) = self.current_block() else {
            return;
        };

        self.loop_stack.push(LoopScope {
            continue_target: cond_entry,
            break_target: exit_block,
        });

        self.move_to_block(body_block);
        let _ = self.lower_expr(body_expr);
        let body_end = self.current_block();

        self.loop_stack.pop();

        let mut backedge = None;
        if let Some(body_end_block) = body_end {
            self.move_to_block(body_end_block);
            self.goto(cond_entry);
            backedge = Some(body_end_block);
        }

        self.move_to_block(cond_header);
        self.branch(cond_val, body_block, exit_block);

        self.builder.body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge,
            },
        );

        self.move_to_block(exit_block);
    }

    fn lower_if(
        &mut self,
        if_expr: ExprId,
        cond: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> ValueId {
        let value = self.ensure_value(if_expr);
        let Some(block) = self.current_block() else {
            return value;
        };

        let if_ty = self.typed_body.expr_ty(self.db, if_expr);
        let produces_value = !self.is_unit_ty(if_ty) && !if_ty.is_never(self.db);

        self.move_to_block(block);
        let cond_val = self.lower_expr(cond);
        let Some(cond_block) = self.current_block() else {
            return value;
        };

        let then_block = self.alloc_block();
        if produces_value {
            let Some(else_expr) = else_expr else {
                debug_assert!(
                    false,
                    "value-producing if expressions must have an else branch"
                );
                self.builder.body.values[value.index()].origin = ValueOrigin::Unit;
                return value;
            };

            let result_local = self.alloc_temp_local(if_ty, true, "if");
            self.builder.body.values[value.index()].origin = ValueOrigin::Local(result_local);

            self.move_to_block(cond_block);
            self.assign(None, Some(result_local), Rvalue::ZeroInit);

            let else_block = self.alloc_block();

            self.move_to_block(then_block);
            let then_value = self.lower_expr(then_expr);
            let then_end = self.current_block();

            self.move_to_block(else_block);
            let else_value = self.lower_expr(else_expr);
            let else_end = self.current_block();

            let merge_block = if then_end.is_some() || else_end.is_some() {
                Some(self.alloc_block())
            } else {
                None
            };

            if let Some(merge) = merge_block {
                if let Some(end_block) = then_end {
                    self.move_to_block(end_block);
                    self.assign(None, Some(result_local), Rvalue::Value(then_value));
                    self.goto(merge);
                }
                if let Some(end_block) = else_end {
                    self.move_to_block(end_block);
                    self.assign(None, Some(result_local), Rvalue::Value(else_value));
                    self.goto(merge);
                }
            }

            self.move_to_block(cond_block);
            self.switch(
                cond_val,
                vec![
                    SwitchTarget {
                        value: SwitchValue::Bool(true),
                        block: then_block,
                    },
                    SwitchTarget {
                        value: SwitchValue::Bool(false),
                        block: else_block,
                    },
                ],
                else_block,
            );

            if let Some(merge) = merge_block {
                self.move_to_block(merge);
            }
        } else {
            self.builder.body.values[value.index()].origin = ValueOrigin::Unit;
            let merge_block = self.alloc_block();
            let else_block = else_expr.map(|_| self.alloc_block());

            self.move_to_block(cond_block);
            self.branch(cond_val, then_block, else_block.unwrap_or(merge_block));

            self.move_to_block(then_block);
            let _ = self.lower_expr(then_expr);
            let then_end = self.current_block();

            let else_end = if let Some(else_expr) = else_expr {
                let else_block = else_block.expect("else_block allocated");
                self.move_to_block(else_block);
                let _ = self.lower_expr(else_expr);
                self.current_block()
            } else {
                Some(merge_block)
            };

            if then_end.is_none() && else_end.is_none() {
                return value;
            }

            if let Some(end_block) = then_end {
                self.move_to_block(end_block);
                self.goto(merge_block);
            }
            if let Some(end_block) = else_end
                && end_block != merge_block
            {
                self.move_to_block(end_block);
                self.goto(merge_block);
            }

            self.move_to_block(merge_block);
        }

        value
    }

    /// Returns whether the given type is the unit tuple type.
    ///
    /// # Parameters
    /// - `ty`: Type to inspect.
    ///
    /// # Returns
    /// `true` if the type is unit.
    pub(super) fn is_unit_ty(&self, ty: TyId<'db>) -> bool {
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
    }

    /// Lowers an expression statement, emitting side-effecting instructions as needed.
    ///
    /// # Parameters
    /// - `stmt_id`: Statement id for context.
    /// - `expr`: Expression id to lower.
    pub(super) fn lower_expr_stmt(&mut self, stmt_id: StmtId, expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };
        if self.try_lower_intrinsic_stmt(expr).is_some() {
            return;
        }
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return;
        };

        match expr_data {
            Expr::With(_, _) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(expr);
                let ty = self.typed_body.expr_ty(self.db, expr);
                if self.current_block().is_some() && !self.is_unit_ty(ty) && !ty.is_never(self.db) {
                    self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                }
            }
            Expr::Block(stmts) => {
                self.move_to_block(block);
                if stmts.is_empty() {
                    return;
                }
                let (head, last) = stmts.split_at(stmts.len() - 1);
                self.lower_block(head);
                if self.current_block().is_none() {
                    return;
                }
                let stmt_id = last[0];
                let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
                    return;
                };
                if let Stmt::Expr(expr) = stmt {
                    self.lower_expr_stmt(stmt_id, *expr);
                } else {
                    self.lower_stmt(stmt_id);
                }
            }
            Expr::Assign(target, value) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(*value);
                if let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                    && let LocalBinding::Local { pat, .. } = binding
                {
                    let pat_ty = self.typed_body.pat_ty(self.db, pat);
                    if self
                        .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                        .address_space()
                        .is_some()
                    {
                        let space = self.value_address_space(value_id);
                        self.set_pat_address_space(pat, space);
                    }
                }

                self.lower_assign_to_lvalue(stmt_id, *target, value_id);
            }
            Expr::AugAssign(target, value, op) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(*value);
                if let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                    && let LocalBinding::Local { pat, .. } = binding
                {
                    let pat_ty = self.typed_body.pat_ty(self.db, pat);
                    if self
                        .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                        .address_space()
                        .is_some()
                    {
                        let space = self.value_address_space(value_id);
                        self.set_pat_address_space(pat, space);
                    }
                }

                self.lower_aug_assign_to_lvalue(stmt_id, *target, value_id, *op);
            }
            _ => {
                self.move_to_block(block);
                let value_id = self.lower_expr(expr);
                if self.current_block().is_some() {
                    self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                }
            }
        }
    }
}
