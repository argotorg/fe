use cranelift_entity::EntityRef;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use rustc_hash::FxHashMap;

use crate::{
    analysis::semantic::instance::{
        CallSiteLowering, ForLoopCallSites, SemanticInstance, resolve_semantic_const_ref,
    },
    analysis::{
        HirAnalysisDb,
        semantic::{
            BorrowActivation, CallSiteId, FieldIndex, LayoutBackingPlace, LayoutBackingProjection,
            LayoutBackingSource, Mutability, SBlock, SBlockId, SCallReturnProjectionStep,
            SCallReturnSource, SConst, SEffectArg, SEffectArgValue, SExpr, SLocal, SLocalId,
            SOperand, SOperandIntent, SPlace, SStmt, SStmtId, SStmtKind, STerminator,
            STerminatorKind, SValueId, SemConstValue, SemOrigin, SemanticBody,
            SemanticCodeRegionTarget, SemanticLocalRole, VariantIndex, bool_const, bytes_const,
            eval_const_ref, int_const, reify_runtime_const_for_ty, return_borrow_results_in_ty,
            return_source_borrow_input_reaches_capability, runtime_size_bytes, sem_const_from_ty,
            unit_const,
        },
        ty::{
            adt_def::instantiate_adt_field_shape,
            const_ty::{
                CallableInputLayoutHoleOrigin, ConstTyData, EvaluatedConstTy,
                const_ty_or_abstract_from_assoc_const_use,
                const_ty_or_abstract_from_inherent_const_use, try_eval_const_int_expr,
            },
            normalize::normalize_ty,
            pattern_ir::{
                KnownPatternScrutinee, PatternBranchReachability,
                known_pattern_scrutinee_from_const, known_scrutinee_arm_reachability,
                single_pattern_branch_reachability,
            },
            ty_check::{
                BodyOwner, ClosureCaptureConstruction, CodeRegionIntrinsicKind, ConstIntrinsicKind,
                ConstRef, LocalBinding, ParamSite, PathReadSemantics, RecordInitLowering,
                RecordLike, ReturnIndexSource, ReturnProjectionStep, ReturnProvenance,
                SemanticExprLowering, TypedBody, TypedCallableBody, ValueAccess, ValuePathRef,
            },
            ty_def::{BorrowKind, CapabilityKind, PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        ArithBinOp, Body, CallArg, CallableDef, Cond, CondId, Expr, ExprId, Field as HirField,
        IntegerId, ItemKind, LitKind, MatchArm, Partial, Pat, PatId, PathId, Stmt, StmtId,
        expr::{BinOp, CompBinOp, LogicalBinOp, UnOp},
    },
    projection::{IndexSource, Projection},
    span::{expr::LazyExprSpan, item::LazyItemSpan, pat::LazyPatSpan},
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};

use super::{
    effects::{owner_effect_bindings, provisional_owner_effect_bindings},
    local_facts::{initial_snapshot_source, ordinary_direct_value_role},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum BindingRoleMode {
    Final,
    Provisional,
}

#[derive(Clone, Copy, Debug, Default)]
struct BranchReachability {
    then_branch: bool,
    else_branch: bool,
}

fn closure_body_local_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    typed_body: &'db TypedBody<'db>,
    body: Body<'db>,
    root: ExprId,
) -> Vec<LocalBinding<'db>> {
    struct Collector<'a, 'db> {
        typed_body: &'a TypedBody<'db>,
        bindings: Vec<LocalBinding<'db>>,
        seen: rustc_hash::FxHashSet<LocalBinding<'db>>,
    }

    impl<'db> Visitor<'db> for Collector<'_, 'db> {
        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
            expr: ExprId,
            expr_data: &Expr<'db>,
        ) {
            // Nested closures own their parameters and body-local patterns.
            // Their enclosing closure only lowers the construction expression.
            if !matches!(expr_data, Expr::Closure { .. }) {
                walk_expr(self, ctxt, expr);
            }
        }

        fn visit_pat(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
            pat: PatId,
            _pat_data: &Pat<'db>,
        ) {
            if let Some(binding @ LocalBinding::Local { .. }) = self.typed_body.pat_binding(pat)
                && self.seen.insert(binding)
            {
                self.bindings.push(binding);
            }
            walk_pat(self, ctxt, pat);
        }

        fn visit_item(
            &mut self,
            _ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>,
            _item: ItemKind<'db>,
        ) {
            // Block-local items have independent callable bodies.
        }
    }

    let Partial::Present(root_data) = root.data(db, body) else {
        return Vec::new();
    };
    let mut collector = Collector {
        typed_body,
        bindings: Vec::new(),
        seen: rustc_hash::FxHashSet::default(),
    };
    let mut ctxt = VisitorCtxt::with_expr(db, body.scope(), body, root);
    collector.visit_expr(&mut ctxt, root, root_data);
    collector.bindings
}

fn return_source_result_projection_overlaps_borrow(
    source: &[ReturnProjectionStep],
    borrow: &[LayoutBackingProjection],
) -> bool {
    source.len() <= borrow.len()
        && source
            .iter()
            .zip(borrow)
            .all(|(source, borrow)| match (source, borrow) {
                (ReturnProjectionStep::Field(source), LayoutBackingProjection::Field(borrow)) => {
                    *source == borrow.0
                }
                (
                    ReturnProjectionStep::VariantField {
                        variant: source_variant,
                        field: source_field,
                    },
                    LayoutBackingProjection::VariantField {
                        variant: borrow_variant,
                        field: borrow_field,
                    },
                ) => *source_variant == borrow_variant.0 && *source_field == borrow_field.0,
                (
                    ReturnProjectionStep::ConstantIndex(source),
                    LayoutBackingProjection::Index(Some(borrow)),
                ) => source == borrow,
                (
                    ReturnProjectionStep::ConstantIndex(_)
                    | ReturnProjectionStep::DynamicIndex(_)
                    | ReturnProjectionStep::AnyIndex,
                    LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_),
                )
                | (
                    ReturnProjectionStep::DynamicIndex(_) | ReturnProjectionStep::AnyIndex,
                    LayoutBackingProjection::Index(Some(_)),
                ) => true,
                _ => false,
            })
}

pub fn lower_to_smir<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    template_owner: BodyOwner<'db>,
    typed_body: &'db TypedBody<'db>,
) -> SemanticBody<'db> {
    let call_sites = instance.call_sites(db);
    let for_loop_call_sites = instance.for_loop_call_sites(db);
    lower_to_smir_with_call_sites(
        db,
        instance,
        template_owner,
        typed_body,
        call_sites,
        for_loop_call_sites,
        BindingRoleMode::Final,
    )
}

pub(crate) fn lower_to_smir_with_call_sites<'a, 'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    template_owner: BodyOwner<'db>,
    typed_body: &'db TypedBody<'db>,
    call_sites: &'a [Option<CallSiteLowering<'db>>],
    for_loop_call_sites: &'a [Option<ForLoopCallSites<'db>>],
    binding_role_mode: BindingRoleMode,
) -> SemanticBody<'db> {
    let Some(body) = typed_body.body() else {
        let mut locals = Vec::new();
        let mut entry_locals = Vec::new();
        let mut push_binding_local = |binding| {
            let local = SLocalId::from_u32(locals.len() as u32);
            let ty = match binding_role_mode {
                BindingRoleMode::Final => instance.binding_ty(db, binding),
                BindingRoleMode::Provisional => instance.provisional_binding_ty(db, binding),
            };
            let role = match binding_role_mode {
                BindingRoleMode::Final => instance.binding_role(db, binding),
                BindingRoleMode::Provisional => instance.provisional_binding_role(db, binding),
            };
            let snapshot_source = initial_snapshot_source(&role);
            let layout_ty = role.layout_ty(ty);
            let layout_backing_sources = snapshot_source
                .clone()
                .map(|source| LayoutBackingSource {
                    target: Vec::new(),
                    source: source.into_layout_backing_place(layout_ty),
                })
                .into_iter()
                .collect();
            locals.push(SLocal {
                ty,
                mutability: if binding.is_mut() {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                },
                source: Some(binding),
                role,
                snapshot_source,
                ownership_sources: Vec::new(),
                layout_backing_sources,
            });
            entry_locals.push(local);
        };
        let mut idx = 0;
        while let Some(binding) = typed_body.param_binding(idx) {
            push_binding_local(binding);
            idx += 1;
        }
        for binding in owner_effect_bindings_for_mode(db, template_owner, binding_role_mode) {
            push_binding_local(binding);
        }
        return SemanticBody {
            owner: instance,
            template_owner,
            entry_locals,
            locals,
            blocks: vec![SBlock {
                stmts: Vec::new(),
                terminator: STerminator {
                    origin: SemOrigin::Body(template_owner),
                    kind: STerminatorKind::Return(None),
                },
            }],
        };
    };

    let mut cx = SmirLowerCtxt::new(
        db,
        instance,
        template_owner,
        typed_body,
        SmirLowerInputs {
            body,
            call_sites,
            for_loop_call_sites,
            binding_role_mode,
        },
    );
    let callable_body = TypedCallableBody::new(template_owner, typed_body);
    let root_expr = callable_body.root_expr(db).unwrap_or_else(|| body.expr(db));
    let result = cx.lower_expr(root_expr);
    if !cx.is_terminated(cx.current) {
        let result = SOperand::expr(result, root_expr);
        cx.set_terminator(
            cx.current,
            SemOrigin::Body(template_owner),
            if cx.expr_ty(root_expr) == TyId::unit(db) {
                STerminatorKind::Return(None)
            } else {
                STerminatorKind::Return(Some(result))
            },
        );
    }
    cx.finish()
}

fn owner_effect_bindings_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    binding_role_mode: BindingRoleMode,
) -> Vec<LocalBinding<'db>> {
    match binding_role_mode {
        BindingRoleMode::Final => owner_effect_bindings(db, owner),
        BindingRoleMode::Provisional => provisional_owner_effect_bindings(db, owner),
    }
}

pub(super) struct SmirLowerCtxt<'a, 'db> {
    pub(super) db: &'db dyn HirAnalysisDb,
    pub(super) instance: SemanticInstance<'db>,
    pub(super) template_owner: BodyOwner<'db>,
    pub(super) typed_body: &'db TypedBody<'db>,
    pub(super) body: Body<'db>,
    pub(super) call_sites: &'a [Option<CallSiteLowering<'db>>],
    pub(super) for_loop_call_sites: &'a [Option<ForLoopCallSites<'db>>],
    pub(super) binding_role_mode: BindingRoleMode,
    pub(super) assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
    pub(super) entry_locals: Vec<SLocalId>,
    pub(super) locals: Vec<SLocal<'db>>,
    pub(super) assigned_snapshots: Vec<bool>,
    pub(super) assigned_ownership_sources: Vec<bool>,
    pub(super) assigned_layout_backing_sources: Vec<bool>,
    pub(super) blocks: Vec<BlockState<'db>>,
    pub(super) binding_locals: FxHashMap<LocalBinding<'db>, SLocalId>,
    pub(super) closure_env_local: Option<SLocalId>,
    pub(super) closure_args_local: Option<SLocalId>,
    pub(super) closure_capture_fields: FxHashMap<LocalBinding<'db>, FieldIndex>,
    pub(super) closure_capture_tys: FxHashMap<LocalBinding<'db>, TyId<'db>>,
    pub(super) with_binding_values: FxHashMap<ExprId, SValueId>,
    pub(super) current: SBlockId,
    pub(super) next_stmt_id: u32,
    pub(super) loop_stack: Vec<LoopScope>,
    capability_rebind_result_ty: Option<TyId<'db>>,
}

pub(super) struct BlockState<'db> {
    pub(super) stmts: Vec<SStmt<'db>>,
    pub(super) terminator: Option<STerminator<'db>>,
}

struct SmirLowerInputs<'a, 'db> {
    body: Body<'db>,
    call_sites: &'a [Option<CallSiteLowering<'db>>],
    for_loop_call_sites: &'a [Option<ForLoopCallSites<'db>>],
    binding_role_mode: BindingRoleMode,
}

#[derive(Clone, Copy)]
pub(super) struct LoopScope {
    pub(super) continue_bb: SBlockId,
    pub(super) break_bb: SBlockId,
    pub(super) has_reachable_break: bool,
}

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn fixed_string_capacity_bytes(&self, ty: TyId<'db>) -> Option<usize> {
        if !ty.is_string(self.db) {
            return None;
        }
        let (_, args) = ty.decompose_ty_app(self.db);
        let len_ty = args.first().copied()?;
        let TyData::ConstTy(const_ty) = len_ty.data(self.db) else {
            return None;
        };
        match const_ty.data(self.db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                int_id.data(self.db).to_usize()
            }
            _ => None,
        }
    }

    fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        template_owner: BodyOwner<'db>,
        typed_body: &'db TypedBody<'db>,
        inputs: SmirLowerInputs<'a, 'db>,
    ) -> Self {
        let mut cx = Self {
            db,
            instance,
            template_owner,
            typed_body,
            body: inputs.body,
            call_sites: inputs.call_sites,
            for_loop_call_sites: inputs.for_loop_call_sites,
            binding_role_mode: inputs.binding_role_mode,
            assumptions: match inputs.binding_role_mode {
                BindingRoleMode::Final => instance.assumptions(db),
                BindingRoleMode::Provisional => {
                    crate::analysis::semantic::semantic_instance_base_assumptions_for_key(
                        db,
                        instance.key(db),
                    )
                }
            },
            entry_locals: Vec::new(),
            locals: Vec::new(),
            assigned_snapshots: Vec::new(),
            assigned_ownership_sources: Vec::new(),
            assigned_layout_backing_sources: Vec::new(),
            blocks: Vec::new(),
            binding_locals: FxHashMap::default(),
            closure_env_local: None,
            closure_args_local: None,
            closure_capture_fields: FxHashMap::default(),
            closure_capture_tys: FxHashMap::default(),
            with_binding_values: FxHashMap::default(),
            current: SBlockId::from_u32(0),
            next_stmt_id: 0,
            loop_stack: Vec::new(),
            capability_rebind_result_ty: None,
        };
        cx.current = cx.new_block();
        cx.collect_binding_locals();
        cx
    }

    fn finish(self) -> SemanticBody<'db> {
        let blocks = self
            .blocks
            .into_iter()
            .map(|block| SBlock {
                stmts: block.stmts,
                terminator: block.terminator.unwrap_or(STerminator {
                    origin: SemOrigin::Body(self.template_owner),
                    kind: STerminatorKind::Return(None),
                }),
            })
            .collect();

        SemanticBody {
            owner: self.instance,
            template_owner: self.template_owner,
            entry_locals: self.entry_locals,
            locals: self.locals,
            blocks,
        }
    }

    fn collect_binding_locals(&mut self) {
        if matches!(self.template_owner, BodyOwner::Closure { .. }) {
            let callable_body = self.instance.key(self.db).callable_body(self.db);
            let (closure_body, receiver_mode) = callable_body
                .owner_closure_body(self.db)
                .expect("closure body must have coherent typed metadata");
            for binding in closure_body.physical_param_bindings(self.db, receiver_mode) {
                let local = self.alloc_entry_binding_local(binding);
                if let LocalBinding::Param {
                    site: ParamSite::ClosureEnv(_),
                    ..
                } = binding
                {
                    self.closure_env_local = Some(local);
                } else if let LocalBinding::Param {
                    site: ParamSite::ClosureArgs(_),
                    ..
                } = binding
                {
                    self.closure_args_local = Some(local);
                }
            }

            let args_local = self
                .closure_args_local
                .expect("closure body must have an argument-pack local");
            for (idx, param) in closure_body.params(self.db).enumerate() {
                let binding = param.binding;
                let local = self.alloc_local(
                    param.ty,
                    if binding.is_mut() {
                        Mutability::Mutable
                    } else {
                        Mutability::Immutable
                    },
                    Some(binding),
                );
                self.binding_locals.insert(binding, local);
                let intent = match binding {
                    LocalBinding::Param {
                        mode: crate::hir_def::params::FuncParamMode::Own,
                        ..
                    } => SOperandIntent::Move,
                    LocalBinding::Param {
                        mode: crate::hir_def::params::FuncParamMode::View,
                        ..
                    } => SOperandIntent::Read,
                    LocalBinding::Local { .. } | LocalBinding::EffectParam { .. } => {
                        unreachable!("closure parameters are parameter bindings")
                    }
                };
                self.push_synthetic_stmt(SStmtKind::Assign {
                    dst: local,
                    expr: SExpr::Field {
                        base: SOperand::synthetic(args_local).with_intent(intent),
                        field: FieldIndex(
                            u16::try_from(idx)
                                .expect("closure argument field index must fit in u16"),
                        ),
                    },
                });
            }
            for (idx, capture) in closure_body.captures(self.db).enumerate() {
                self.closure_capture_fields.insert(
                    capture.binding,
                    FieldIndex(
                        u16::try_from(idx).expect("closure capture field index must fit in u16"),
                    ),
                );
                self.closure_capture_tys.insert(capture.binding, capture.ty);
            }
            let root = self
                .template_owner
                .root_expr(self.db)
                .expect("closure body must have a root expression");
            for binding in closure_body_local_bindings(self.db, self.typed_body, self.body, root) {
                self.alloc_binding_local(binding);
            }
            return;
        }

        let mut param_idx = 0;
        while let Some(binding) = self.typed_body.param_binding(param_idx) {
            self.alloc_entry_binding_local(binding);
            param_idx += 1;
        }
        if let BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } = self.template_owner
        {
            let recv = crate::semantic::RecvView::new(self.db, contract, recv_idx);
            let arm = crate::semantic::RecvArmView::new(self.db, recv, arm_idx);
            for binding in arm.arg_bindings(self.db) {
                if let Some(binding) = self.typed_body.pat_binding(binding.pat) {
                    self.alloc_entry_binding_local(binding);
                }
            }
        }
        for binding in self.owner_effect_bindings() {
            self.alloc_entry_binding_local(binding);
        }

        for (pat, _) in self.body.pats(self.db).iter() {
            if let Some(binding) = self.typed_body.pat_binding(pat) {
                self.alloc_binding_local(binding);
            }
        }
    }

    pub(super) fn alloc_binding_local(&mut self, binding: LocalBinding<'db>) -> SLocalId {
        if let Some(&local) = self.binding_locals.get(&binding) {
            return local;
        }
        let local = self.alloc_local(
            self.binding_ty(binding),
            if binding.is_mut() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            Some(binding),
        );
        self.binding_locals.insert(binding, local);
        local
    }

    fn alloc_entry_binding_local(&mut self, binding: LocalBinding<'db>) -> SLocalId {
        let local = self.alloc_binding_local(binding);
        if !self.entry_locals.contains(&local) {
            self.entry_locals.push(local);
        }
        local
    }

    fn alloc_local(
        &mut self,
        ty: TyId<'db>,
        mutability: Mutability,
        source: Option<LocalBinding<'db>>,
    ) -> SLocalId {
        let id = SLocalId::from_u32(self.locals.len() as u32);
        let role = source.map_or_else(ordinary_direct_value_role, |binding| {
            self.binding_role(binding)
        });
        let snapshot_source = initial_snapshot_source(&role);
        let layout_ty = role.layout_ty(ty);
        let layout_backing_sources = snapshot_source
            .clone()
            .map(|source| LayoutBackingSource {
                target: Vec::new(),
                source: source.into_layout_backing_place(layout_ty),
            })
            .into_iter()
            .collect::<Vec<_>>();
        self.assigned_snapshots.push(snapshot_source.is_some());
        self.assigned_ownership_sources.push(false);
        self.assigned_layout_backing_sources
            .push(!layout_backing_sources.is_empty());
        self.locals.push(SLocal {
            ty,
            mutability,
            source,
            role,
            snapshot_source,
            ownership_sources: Vec::new(),
            layout_backing_sources,
        });
        id
    }

    pub(super) fn binding_ty(&self, binding: LocalBinding<'db>) -> TyId<'db> {
        match self.binding_role_mode {
            BindingRoleMode::Final => self.instance.binding_ty(self.db, binding),
            BindingRoleMode::Provisional => self.instance.provisional_binding_ty(self.db, binding),
        }
    }

    fn owner_effect_bindings(&self) -> Vec<LocalBinding<'db>> {
        owner_effect_bindings_for_mode(self.db, self.template_owner, self.binding_role_mode)
    }

    fn binding_role(&self, binding: LocalBinding<'db>) -> SemanticLocalRole<'db> {
        match self.binding_role_mode {
            BindingRoleMode::Final => self.instance.binding_role(self.db, binding),
            BindingRoleMode::Provisional => {
                self.instance.provisional_binding_role(self.db, binding)
            }
        }
    }

    fn alloc_temp(&mut self, ty: TyId<'db>) -> SLocalId {
        self.alloc_local(ty, Mutability::Immutable, None)
    }

    pub(super) fn new_block(&mut self) -> SBlockId {
        let id = SBlockId::from_u32(self.blocks.len() as u32);
        self.blocks.push(BlockState {
            stmts: Vec::new(),
            terminator: None,
        });
        id
    }

    pub(super) fn switch_to(&mut self, block: SBlockId) {
        self.current = block;
    }

    pub(super) fn is_terminated(&self, block: SBlockId) -> bool {
        self.blocks[block.index()].terminator.is_some()
    }

    pub(super) fn push_stmt(&mut self, origin: SemOrigin<'db>, kind: SStmtKind<'db>) {
        if !self.is_terminated(self.current) {
            self.update_stmt_local_facts(&kind);
            let id = SStmtId::from_u32(self.next_stmt_id);
            self.next_stmt_id = self
                .next_stmt_id
                .checked_add(1)
                .expect("semantic statement id overflow");
            self.blocks[self.current.index()]
                .stmts
                .push(SStmt { id, origin, kind });
        }
    }

    pub(super) fn set_terminator(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        kind: STerminatorKind<'db>,
    ) {
        if self.blocks[block.index()].terminator.is_none() {
            self.blocks[block.index()].terminator = Some(STerminator { origin, kind });
        }
    }

    pub(super) fn emit_expr_with_origin(
        &mut self,
        origin: SemOrigin<'db>,
        ty: TyId<'db>,
        expr: SExpr<'db>,
    ) -> SValueId {
        let expr = self.materialize_enum_view_operands(origin, expr);
        let dst = self.alloc_temp(ty);
        self.push_stmt(origin, SStmtKind::Assign { dst, expr });
        dst
    }

    fn materialize_enum_view_operands(
        &mut self,
        origin: SemOrigin<'db>,
        expr: SExpr<'db>,
    ) -> SExpr<'db> {
        match expr {
            SExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => {
                let adt = enum_ty.adt_def(self.db);
                let fields = fields
                    .into_vec()
                    .into_iter()
                    .enumerate()
                    .map(|(idx, field)| {
                        adt.map(|adt| {
                            instantiate_adt_field_shape(
                                self.db,
                                adt,
                                usize::from(variant.0),
                                idx,
                                enum_ty.generic_args(self.db),
                            )
                        })
                        .map_or(field, |expected| {
                            self.materialize_view_operand(origin, field, expected)
                        })
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                SExpr::EnumMake {
                    enum_ty,
                    variant,
                    fields,
                }
            }
            expr => expr,
        }
    }

    fn materialize_view_operand(
        &mut self,
        origin: SemOrigin<'db>,
        operand: SOperand,
        expected: TyId<'db>,
    ) -> SOperand {
        let expected = normalize_ty(self.db, expected, self.body.scope(), self.assumptions);
        let actual = normalize_ty(
            self.db,
            self.locals[operand.value.index()].ty,
            self.body.scope(),
            self.assumptions,
        );
        if !matches!(
            expected.as_capability(self.db),
            Some((CapabilityKind::View, _))
        ) || actual.as_capability(self.db).is_some()
        {
            return operand;
        }

        let value = self.emit_expr_with_origin(
            operand.sem_origin(origin),
            expected,
            SExpr::ReadPlace {
                place: SPlace::new(operand.value),
                intent: SOperandIntent::Read,
            },
        );
        SOperand {
            value,
            intent: SOperandIntent::Read,
            ..operand
        }
    }

    pub(super) fn emit_expr(&mut self, ty: TyId<'db>, expr: SExpr<'db>) -> SValueId {
        self.emit_expr_with_origin(SemOrigin::Synthetic, ty, expr)
    }

    pub(super) fn lower_expr_operand(&mut self, expr: ExprId) -> SOperand {
        SOperand::expr(self.lower_expr(expr), expr).with_intent(self.expr_operand_intent(expr))
    }

    /// Materializes an index expression at the exact projection site.
    ///
    /// A direct path read can otherwise reuse its mutable binding local. That
    /// would make an earlier projection appear to change when the binding is
    /// reassigned later, defeating program-point constant and alias facts.
    /// Program-point reaching-value facts relate snapshots that are guaranteed
    /// to observe the same source version, without leaking branch-local values
    /// through joins.
    pub(super) fn lower_index_operand(&mut self, expr: ExprId) -> SOperand {
        let source = self.lower_expr(expr);
        let snapshot = self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            self.expr_ty(expr),
            SExpr::UseValue(SOperand::expr(source, expr).with_intent(SOperandIntent::Read)),
        );
        SOperand::expr(snapshot, expr).with_intent(SOperandIntent::Read)
    }

    pub(super) fn expr_operand_intent(&self, expr: ExprId) -> SOperandIntent {
        match self.typed_body.expr_value_access(self.db, expr) {
            ValueAccess::Infer => SOperandIntent::Infer,
            ValueAccess::Read => SOperandIntent::Read,
            ValueAccess::MoveIfNonCopy => {
                unreachable!("conditional value access must be resolved before semantic lowering")
            }
            ValueAccess::Move => SOperandIntent::Move,
        }
    }

    pub(super) fn push_synthetic_stmt(&mut self, kind: SStmtKind<'db>) {
        self.push_stmt(SemOrigin::Synthetic, kind);
    }

    pub(super) fn set_synthetic_terminator(&mut self, block: SBlockId, kind: STerminatorKind<'db>) {
        self.set_terminator(block, SemOrigin::Synthetic, kind);
    }

    pub(super) fn expr_ty(&self, expr: ExprId) -> TyId<'db> {
        self.typed_body.expr_ty(self.db, expr)
    }

    pub(super) fn unit_value(&mut self) -> SValueId {
        self.emit_expr(
            TyId::unit(self.db),
            SExpr::Const(SConst::Value(unit_const(self.db))),
        )
    }

    pub(super) fn lower_expr(&mut self, expr: ExprId) -> SValueId {
        let target_ty = self.expr_ty(expr);
        let source_ty = self
            .typed_body
            .contextual_view_source(expr)
            .filter(|_| self.typed_body.expr_place(expr).is_none())
            .unwrap_or(target_ty);
        let source = self.lower_expr_as(expr, source_ty);
        if source_ty == target_ty {
            return source;
        }
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            target_ty,
            SExpr::ReadPlace {
                place: SPlace::new(source),
                intent: SOperandIntent::Read,
            },
        )
    }

    fn lower_expr_as(&mut self, expr: ExprId, ty: TyId<'db>) -> SValueId {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            panic!("cannot lower absent expression")
        };
        let ty = if ty.as_capability(self.db).is_some()
            && matches!(
                expr_data,
                Expr::Block(..) | Expr::If(..) | Expr::Match(..) | Expr::With(..)
            ) {
            self.capability_rebind_result_ty.unwrap_or(ty)
        } else {
            ty
        };
        let origin = SemOrigin::Expr(expr);

        match expr_data {
            Expr::Lit(lit) => self.lower_leaf_literal(expr, lit, ty),
            Expr::Path(_) => self.lower_path_expr(expr, ty),
            Expr::Closure { .. } => {
                let closure = ty
                    .as_closure(self.db)
                    .unwrap_or_else(|| panic!("closure expression has non-closure type: {ty:?}"));
                let captures = self
                    .instance
                    .key(self.db)
                    .callable_body(self.db)
                    .closure_capture_plan(self.db, closure)
                    .unwrap_or_else(|| panic!("closure capture plan missing for {expr:?}"));
                let fields = captures
                    .into_iter()
                    .map(|capture| {
                        self.lower_binding_capture_operand(
                            capture.binding,
                            capture.ty,
                            capture.construction,
                        )
                    })
                    .collect();
                self.emit_expr_with_origin(origin, ty, SExpr::AggregateMake { ty, fields })
            }
            Expr::Tuple(elems) | Expr::Array(elems) => {
                let fields = elems
                    .iter()
                    .map(|expr| self.lower_expr_operand(*expr))
                    .collect();
                self.emit_expr_with_origin(origin, ty, SExpr::AggregateMake { ty, fields })
            }
            Expr::ArrayRep(elem, _) => {
                let value = self.lower_expr_operand(*elem);
                self.emit_expr_with_origin(origin, ty, SExpr::ArrayRepeat { ty, value })
            }
            Expr::RecordInit(path, fields) => self.lower_record_init(expr, *path, fields, ty),
            Expr::Field(base, _) => {
                if let Some(place) = self.typed_body.expr_place(expr) {
                    let place = self.lower_place_data(place);
                    return self.emit_expr_with_origin(
                        origin,
                        ty,
                        SExpr::ReadPlace {
                            place,
                            intent: self.expr_operand_intent(expr),
                        },
                    );
                }
                let base_expr = *base;
                let base = self.lower_expr(base_expr);
                let field = FieldIndex(
                    self.typed_body
                        .resolved_field_index(expr)
                        .expect("field expression should have a resolved field index"),
                );
                self.emit_expr_with_origin(
                    origin,
                    ty,
                    SExpr::Field {
                        base: SOperand::expr(base, base_expr)
                            .with_intent(self.expr_operand_intent(expr)),
                        field,
                    },
                )
            }
            Expr::Bin(base, index, BinOp::Index) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*base), &[*index]);
                }
                if let Some(place) = self.typed_body.expr_place(expr) {
                    let place = self.lower_place_data(place);
                    return self.emit_expr_with_origin(
                        origin,
                        ty,
                        SExpr::ReadPlace {
                            place,
                            intent: self.expr_operand_intent(expr),
                        },
                    );
                }
                let base = self
                    .lower_expr_operand(*base)
                    .with_intent(self.expr_operand_intent(expr));
                let index = self.lower_index_operand(*index);
                self.emit_expr_with_origin(origin, ty, SExpr::Index { base, index })
            }
            Expr::Un(inner, UnOp::Mut | UnOp::Ref) => {
                let kind = match expr_data {
                    Expr::Un(_, UnOp::Mut) => BorrowKind::Mut,
                    Expr::Un(_, UnOp::Ref) => BorrowKind::Ref,
                    _ => unreachable!(),
                };
                let place = self.lower_place(*inner);
                self.emit_expr_with_origin(
                    origin,
                    ty,
                    SExpr::Borrow {
                        place,
                        kind,
                        provider: self.typed_body.expr_prop(self.db, expr).borrow_provider,
                        activation: BorrowActivation::Immediate,
                    },
                )
            }
            Expr::Un(inner, op) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*inner), &[]);
                }
                if *op == UnOp::Minus
                    && let Some(value) = self.lower_negated_int_literal(expr, *inner, ty)
                {
                    return value;
                }
                let value = self.lower_expr_operand(*inner);
                self.emit_expr_with_origin(origin, ty, SExpr::Unary { op: *op, value })
            }
            Expr::Bin(lhs, rhs, BinOp::Arith(ArithBinOp::Range)) => {
                let lhs = self.lower_expr_operand(*lhs);
                let rhs = self.lower_expr_operand(*rhs);
                let unit = SOperand::synthetic(self.unit_value());
                let fields = ty
                    .field_types(self.db)
                    .into_iter()
                    .enumerate()
                    .map(|(idx, field_ty)| {
                        let field_ty =
                            normalize_ty(self.db, field_ty, self.body.scope(), self.assumptions);
                        if field_ty == TyId::unit(self.db) || field_ty.is_zero_sized(self.db) {
                            unit
                        } else if idx == 0 {
                            lhs
                        } else {
                            rhs
                        }
                    })
                    .collect();
                self.emit_expr_with_origin(origin, ty, SExpr::AggregateMake { ty, fields })
            }
            Expr::Bin(lhs, rhs, op) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*lhs), &[*rhs]);
                }
                if matches!(op, BinOp::Logical(_)) {
                    return self.lower_logical_expr(expr);
                }
                let lhs = self.lower_expr_operand(*lhs);
                let rhs = self.lower_expr_operand(*rhs);
                self.emit_expr_with_origin(origin, ty, SExpr::Binary { op: *op, lhs, rhs })
            }
            Expr::Cast(value, to) => {
                let value_ty = self.expr_ty(*value);
                let value = self.lower_expr_operand(*value);
                let value_ty = normalize_ty(self.db, value_ty, self.body.scope(), self.assumptions);
                let ty = normalize_ty(self.db, ty, self.body.scope(), self.assumptions);
                if value_ty == ty {
                    return self.emit_expr_with_origin(origin, ty, SExpr::UseValue(value));
                }
                self.emit_expr_with_origin(
                    origin,
                    ty,
                    SExpr::Cast {
                        value,
                        to: to.to_opt().map_or(ty, |_| ty),
                    },
                )
            }
            Expr::Call(callee, args) => {
                let receiver = matches!(
                    self.typed_body.semantic_expr_lowering(expr),
                    Some(SemanticExprLowering::Call {
                        callee_is_receiver: true,
                        ..
                    })
                )
                .then_some(*callee);
                self.lower_call(expr, receiver, args, ty)
            }
            Expr::Assert(args) => self.lower_assert(expr, args),
            Expr::MethodCall(receiver, _, _, args) => {
                self.lower_call(expr, Some(*receiver), args, ty)
            }
            Expr::Assign(dst, src) => {
                let rebinds_capability = self.typed_body.assignment_rebinds_capability(expr);
                let dst_place = self.lower_assignment_place(*dst, rebinds_capability);
                let previous_result_ty = self.capability_rebind_result_ty;
                if rebinds_capability {
                    self.capability_rebind_result_ty = Some(self.expr_ty(*dst));
                }
                let src = self.lower_expr_operand(*src);
                if rebinds_capability {
                    self.capability_rebind_result_ty = previous_result_ty;
                }
                self.push_place_write(origin, dst_place, src, rebinds_capability);
                self.unit_value()
            }
            Expr::AugAssign(dst, src, op) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*dst), &[*src]);
                }
                let dst_expr_ty = self.expr_ty(*dst);
                let dst_place = self.lower_assignment_place(*dst, false);
                let lhs =
                    if dst_expr_ty.as_capability(self.db).is_some() || !dst_place.path.is_empty() {
                        SOperand::expr(
                            self.emit_expr_with_origin(
                                SemOrigin::Expr(*dst),
                                self.projectable_place_ty(dst_expr_ty),
                                SExpr::ReadPlace {
                                    place: dst_place.clone(),
                                    intent: SOperandIntent::Read,
                                },
                            ),
                            *dst,
                        )
                    } else {
                        self.lower_expr_operand(*dst)
                    };
                let rhs = self.lower_expr_operand(*src);
                let dst_ty = self.projectable_place_ty(dst_expr_ty);
                let sum = self.emit_expr_with_origin(
                    origin,
                    dst_ty,
                    SExpr::Binary {
                        op: BinOp::Arith(*op),
                        lhs,
                        rhs,
                    },
                );
                self.push_place_write(origin, dst_place, SOperand::inherited(sum), false);
                self.unit_value()
            }
            Expr::Block(stmts) => self.lower_block_expr(stmts),
            Expr::If(cond, then_expr, else_expr) => {
                self.lower_if_expr(*cond, *then_expr, *else_expr, ty)
            }
            Expr::Match(scrutinee, arms) => self.lower_match_expr(*scrutinee, arms, ty),
            Expr::With(bindings, body) => self.lower_with_expr(bindings, *body),
        }
    }

    fn lower_assert(&mut self, expr: ExprId, args: &[crate::hir_def::CallArg<'db>]) -> SValueId {
        let Some(cond_arg) = args.first() else {
            return self.unit_value();
        };
        let message = if let Some(message_arg) = args.get(1) {
            let Partial::Present(Expr::Lit(LitKind::String(message))) =
                message_arg.expr.data(self.db, self.body)
            else {
                return self.unit_value();
            };
            Some(*message)
        } else {
            None
        };

        let success_bb = self.new_block();
        let failure_bb = self.new_block();
        let join_bb = self.new_block();
        let reachable = self.lower_expr_branch(cond_arg.expr, success_bb, failure_bb);

        self.switch_to(success_bb);
        if reachable.then_branch {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        } else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }

        self.switch_to(failure_bb);
        if reachable.else_branch {
            self.set_terminator(
                self.current,
                SemOrigin::Expr(expr),
                STerminatorKind::Assert { message },
            );
        } else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }

        if !reachable.then_branch {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
        self.unit_value()
    }

    fn lower_leaf_literal(&mut self, expr: ExprId, lit: &LitKind<'db>, ty: TyId<'db>) -> SValueId {
        let value = match lit {
            LitKind::Int(int_id) => int_const(self.db, ty, int_id.data(self.db).clone().into()),
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
            LitKind::Bool(value) => bool_const(self.db, *value),
        };
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::Const(SConst::Value(value)),
        )
    }

    fn lower_negated_int_literal(
        &mut self,
        expr: ExprId,
        inner: ExprId,
        ty: TyId<'db>,
    ) -> Option<SValueId> {
        let Partial::Present(Expr::Lit(LitKind::Int(int_id))) = inner.data(self.db, self.body)
        else {
            return None;
        };
        let value = int_const(self.db, ty, -BigInt::from(int_id.data(self.db).clone()));
        Some(self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::Const(SConst::Value(value)),
        ))
    }

    fn lower_const_ref(
        &mut self,
        expr: ExprId,
        const_ref: ConstRef<'db>,
        ty: TyId<'db>,
    ) -> SValueId {
        let mut type_level_fallback = None;
        let symbolic_const_ty = match const_ref {
            ConstRef::TraitConst(assoc) => {
                const_ty_or_abstract_from_assoc_const_use(self.db, assoc, ty)
            }
            ConstRef::InherentConst(use_) => {
                const_ty_or_abstract_from_inherent_const_use(self.db, use_, ty)
            }
            ConstRef::Const(_) => None,
        };
        if let Some(const_ty) = symbolic_const_ty.map(|const_ty| TyId::const_ty(self.db, const_ty))
            && let Some(mut symbolic) = sem_const_from_ty(self.db, const_ty)
        {
            if matches!(symbolic.value(self.db), SemConstValue::TypeLevel { .. })
                && let Some(runtime) =
                    reify_runtime_const_for_ty(self.db, self.instance, ty, symbolic)
            {
                symbolic = runtime;
            }

            let instance_has_generic_args = self
                .instance
                .key(self.db)
                .subst(self.db)
                .generic_args(self.db)
                .iter()
                .any(|arg| arg.has_param(self.db) || arg.has_var(self.db));
            if !matches!(symbolic.value(self.db), SemConstValue::TypeLevel { .. })
                || instance_has_generic_args
            {
                return self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::Const(SConst::Value(symbolic)),
                );
            }
            type_level_fallback = Some(symbolic);
        }

        if let Some(const_ref) =
            resolve_semantic_const_ref(self.db, const_ref, ty, SemOrigin::Expr(expr))
        {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::Const(SConst::Ref(const_ref)),
            );
        }

        // The associated const cannot be resolved to a concrete instance in
        // this context (e.g. a const on a still-generic `Self` inside a
        // CTFE-evaluated anon const body). Emit the type-level symbolic value
        // so const evaluation yields the abstract form instead of panicking.
        if let Some(symbolic) = type_level_fallback {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::Const(SConst::Value(symbolic)),
            );
        }

        panic!("const ref should resolve to a semantic instance: {const_ref:?}");
    }

    fn lower_path_expr(&mut self, expr: ExprId, ty: TyId<'db>) -> SValueId {
        if let Some(binding) = self.typed_body.expr_binding(expr) {
            if let Some(value) = self.lower_captured_binding_read(expr, binding, ty) {
                return value;
            }
            let local = *self
                .binding_locals
                .get(&binding)
                .expect("binding local should be allocated");
            return match self.binding_path_read_semantics(
                binding,
                self.typed_body
                    .path_expr_read_semantics(expr)
                    .expect("binding path should have typed read semantics"),
                ty,
            ) {
                PathReadSemantics::ReuseLocal => local,
                PathReadSemantics::ForwardInterface => self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::Forward(
                        SOperand::inherited(local).with_intent(self.expr_operand_intent(expr)),
                    ),
                ),
                PathReadSemantics::MaterializeValue => self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::UseValue(
                        SOperand::inherited(local).with_intent(self.expr_operand_intent(expr)),
                    ),
                ),
            };
        }
        if let Some(const_ref) = self.typed_body.expr_const_ref(expr) {
            return self.lower_const_ref(expr, const_ref, ty);
        }
        if let Some(region) = self.typed_body.expr_code_region_ref(self.db, expr) {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::CodeRegionRef { region },
            );
        }

        match self.typed_body.value_path_ref(expr) {
            Some(ValuePathRef::UnitVariant(variant)) => self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::EnumMake {
                    enum_ty: variant.ty,
                    variant: VariantIndex(variant.variant.idx),
                    fields: Box::new([]),
                },
            ),
            Some(ValuePathRef::TypeConst(ty)) => {
                if let Some(value) = sem_const_from_ty(self.db, ty) {
                    let value = reify_runtime_const_for_ty(self.db, self.instance, ty, value)
                        .unwrap_or(value);
                    self.emit_expr_with_origin(
                        SemOrigin::Expr(expr),
                        ty,
                        SExpr::Const(SConst::Value(value)),
                    )
                } else {
                    panic!(
                        "typed const value path is not lowerable in semantic MIR: expr={expr:?} ty={} data={:?}",
                        ty.pretty_print(self.db),
                        ty.data(self.db),
                    )
                }
            }
            Some(ValuePathRef::FunctionItem) => {
                debug_assert!(
                    ty.is_func(self.db),
                    "function-item path has non-function type: {}",
                    ty.pretty_print(self.db),
                );
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::AggregateMake {
                        ty,
                        fields: Box::new([]),
                    },
                )
            }
            None => panic!(
                "typed path expression is missing semantic value-path classification: owner={:?} expr={expr:?} data={:?} ty={} ty_data={:?} binding={:?} const_ref={:?} code_region_ref={:?}",
                self.template_owner,
                self.body.exprs(self.db)[expr],
                self.expr_ty(expr).pretty_print(self.db),
                self.expr_ty(expr).data(self.db),
                self.typed_body.expr_binding(expr),
                self.typed_body.expr_const_ref(expr),
                self.typed_body.expr_code_region_ref(self.db, expr),
            ),
        }
    }

    fn lower_captured_binding_read(
        &mut self,
        expr: ExprId,
        binding: LocalBinding<'db>,
        ty: TyId<'db>,
    ) -> Option<SValueId> {
        let field = *self.closure_capture_fields.get(&binding)?;
        let field_ty = *self.closure_capture_tys.get(&binding)?;
        let env = self.closure_env_local?;
        if field_ty != ty && field_ty.as_capability(self.db).is_some() {
            let place = SPlace::new(self.lower_effect_binding_value(binding));
            return Some(self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::ReadPlace {
                    place,
                    intent: self.expr_operand_intent(expr),
                },
            ));
        }
        Some(self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::Field {
                base: SOperand::inherited(env).with_intent(self.expr_operand_intent(expr)),
                field,
            },
        ))
    }

    pub(super) fn lower_effect_binding_value(&mut self, binding: LocalBinding<'db>) -> SValueId {
        if let Some(local) = self.binding_locals.get(&binding).copied() {
            return local;
        }
        let field = *self
            .closure_capture_fields
            .get(&binding)
            .unwrap_or_else(|| panic!("effect binding local should be allocated: {binding:?}"));
        let ty = *self
            .closure_capture_tys
            .get(&binding)
            .expect("closure effect capture must retain its field type");
        let env = self
            .closure_env_local
            .expect("closure environment local missing for effect capture");
        self.emit_expr_with_origin(
            SemOrigin::Synthetic,
            ty,
            SExpr::Field {
                base: SOperand::inherited(env),
                field,
            },
        )
    }

    fn lower_binding_capture_operand(
        &mut self,
        binding: LocalBinding<'db>,
        ty: TyId<'db>,
        construction: ClosureCaptureConstruction,
    ) -> SOperand {
        let intent = match construction {
            ClosureCaptureConstruction::Copy => SOperandIntent::Read,
            ClosureCaptureConstruction::Deferred => {
                unreachable!("closure capture construction must be resolved before lowering")
            }
            ClosureCaptureConstruction::Move => SOperandIntent::Move,
        };
        let (source_ty, source_place, source_value, source_field) =
            if let Some(local) = self.binding_locals.get(&binding).copied() {
                (
                    self.locals[local.index()].ty,
                    SPlace::new(local),
                    Some(local),
                    None,
                )
            } else {
                let field = *self
                    .closure_capture_fields
                    .get(&binding)
                    .unwrap_or_else(|| {
                        panic!("capture binding local should be allocated: {binding:?}")
                    });
                let env = self.closure_env_local.unwrap_or_else(|| {
                    panic!("closure environment local missing for capture: {binding:?}")
                });
                let mut place = SPlace::new(env);
                place.push_field(field);
                (
                    *self
                        .closure_capture_tys
                        .get(&binding)
                        .expect("nested closure capture must retain its field type"),
                    place,
                    None,
                    Some((env, field)),
                )
            };
        if let Some((kind, inner)) = ty.as_capability(self.db)
            && source_ty.as_capability(self.db).is_none()
            && inner == source_ty
        {
            let kind = match kind {
                CapabilityKind::Mut => BorrowKind::Mut,
                CapabilityKind::View | CapabilityKind::Ref => BorrowKind::Ref,
            };
            let provider = self.binding_provider_space(binding).or_else(|| {
                matches!(
                    binding,
                    LocalBinding::Local { .. } | LocalBinding::Param { .. }
                )
                .then_some(crate::analysis::ty::ProviderAddressSpace::Memory)
            });
            let value = self.emit_expr_with_origin(
                SemOrigin::Synthetic,
                ty,
                SExpr::Borrow {
                    place: source_place,
                    kind,
                    provider,
                    activation: BorrowActivation::Immediate,
                },
            );
            return SOperand::synthetic(value).with_intent(intent);
        }
        if let Some(value) = source_value {
            return SOperand::inherited(value).with_intent(intent);
        }
        let (env, field) = source_field.expect("nested closure capture field");
        let value = self.emit_expr_with_origin(
            SemOrigin::Synthetic,
            ty,
            SExpr::Field {
                base: SOperand::inherited(env).with_intent(intent),
                field,
            },
        );
        SOperand::synthetic(value).with_intent(intent)
    }

    fn binding_path_read_semantics(
        &self,
        binding: LocalBinding<'db>,
        typed_semantics: PathReadSemantics,
        expr_ty: TyId<'db>,
    ) -> PathReadSemantics {
        let scope = self.body.scope();
        if normalize_ty(self.db, expr_ty, scope, self.assumptions)
            == normalize_ty(self.db, self.binding_ty(binding), scope, self.assumptions)
        {
            return PathReadSemantics::ReuseLocal;
        }

        match self.binding_role(binding) {
            SemanticLocalRole::DirectValue {
                provenance: crate::analysis::semantic::ValueProvenance::RootProvider(_),
            }
            | SemanticLocalRole::PlaceBoundValue { .. }
            | SemanticLocalRole::DirectCarrier { .. } => PathReadSemantics::ForwardInterface,
            SemanticLocalRole::PlaceCarrier { .. }
                if normalize_ty(self.db, expr_ty, scope, self.assumptions)
                    .as_capability(self.db)
                    .is_some() =>
            {
                PathReadSemantics::ForwardInterface
            }
            SemanticLocalRole::Erased
            | SemanticLocalRole::DirectValue { .. }
            | SemanticLocalRole::PlaceCarrier { .. } => match typed_semantics {
                PathReadSemantics::ReuseLocal => PathReadSemantics::MaterializeValue,
                PathReadSemantics::ForwardInterface | PathReadSemantics::MaterializeValue => {
                    typed_semantics
                }
            },
        }
    }

    fn lower_record_init(
        &mut self,
        expr: ExprId,
        _: Partial<PathId<'db>>,
        fields: &[HirField<'db>],
        ty: TyId<'db>,
    ) -> SValueId {
        match self
            .typed_body
            .record_init_lowering(expr)
            .unwrap_or_else(|| panic!("record init lowering missing for {expr:?}"))
        {
            RecordInitLowering::EnumVariant(variant) => {
                let mut values = vec![None; fields.len()];
                for field in fields {
                    let Some(label) = field.label_eagerly(self.db, self.body) else {
                        panic!("record variant init field label missing")
                    };
                    let idx = RecordLike::from_variant(variant)
                        .record_field_idx(self.db, label)
                        .expect("record variant field should resolve");
                    values[idx] = Some(self.lower_expr_operand(field.expr));
                }
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::EnumMake {
                        enum_ty: variant.ty,
                        variant: VariantIndex(variant.variant.idx),
                        fields: values
                            .into_iter()
                            .map(|value| value.expect("missing enum field"))
                            .collect(),
                    },
                )
            }
            RecordInitLowering::Struct => {
                let mut values = vec![None; fields.len()];
                for field in fields {
                    let Some(label) = field.label_eagerly(self.db, self.body) else {
                        panic!("record init field label missing")
                    };
                    let idx = RecordLike::Type(ty)
                        .record_field_idx(self.db, label)
                        .expect("record field should resolve");
                    values[idx] = Some(self.lower_expr_operand(field.expr));
                }
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::AggregateMake {
                        ty,
                        fields: values
                            .into_iter()
                            .map(|value| value.expect("missing record field"))
                            .collect(),
                    },
                )
            }
        }
    }

    fn lower_call(
        &mut self,
        expr: ExprId,
        receiver: Option<ExprId>,
        args: &[CallArg<'db>],
        ty: TyId<'db>,
    ) -> SValueId {
        let arg_exprs = args.iter().map(|arg| arg.expr).collect::<Vec<_>>();
        self.lower_call_like_expr(expr, ty, receiver, &arg_exprs)
    }

    fn lower_call_like_expr(
        &mut self,
        expr: ExprId,
        ty: TyId<'db>,
        receiver: Option<ExprId>,
        args: &[ExprId],
    ) -> SValueId {
        let lowering = self
            .typed_body
            .semantic_expr_lowering(expr)
            .unwrap_or_else(|| {
                panic!("semantic lowering missing for call-like expression {expr:?}")
            });
        match lowering {
            SemanticExprLowering::Call { callable, .. } => {
                self.lower_callable_expr(expr, ty, receiver, args, callable)
            }
            SemanticExprLowering::CodeRegionIntrinsic {
                region_arg, kind, ..
            } => {
                let target = self.lower_code_region_target(expr, *region_arg);
                let lowered = match kind {
                    CodeRegionIntrinsicKind::Offset => SExpr::CodeRegionOffset { target },
                    CodeRegionIntrinsicKind::Len => SExpr::CodeRegionLen { target },
                };
                self.emit_expr_with_origin(SemOrigin::Expr(expr), ty, lowered)
            }
            SemanticExprLowering::ConstIntrinsic { callable, kind } => {
                self.lower_const_intrinsic(expr, callable, *kind, ty)
            }
        }
    }

    fn lower_code_region_target(
        &self,
        call_expr: ExprId,
        region_arg: ExprId,
    ) -> SemanticCodeRegionTarget<'db> {
        self.typed_body
            .expr_code_region_ref(self.db, region_arg)
            .map(SemanticCodeRegionTarget::Resolved)
            .unwrap_or_else(|| {
                let ty = self.expr_ty(region_arg);
                if ty.has_param(self.db) || ty.has_var(self.db) {
                    SemanticCodeRegionTarget::Deferred {
                        arg: region_arg, ty
                    }
                } else {
                    panic!(
                        "typed code-region intrinsic is missing instantiated code-region ref: call={call_expr:?} arg={region_arg:?} ty={ty:?}"
                    )
                }
            })
    }

    fn lower_callable_expr(
        &mut self,
        expr: ExprId,
        ty: TyId<'db>,
        receiver: Option<ExprId>,
        args: &[ExprId],
        callable: &crate::analysis::ty::ty_check::Callable<'db>,
    ) -> SValueId {
        let call_args_pack = callable
            .call_trait_args_pack_ty(self.db, self.body.scope())
            .map(|ty| self.instance.normalized_ty(self.db, ty));
        let mut values = Vec::with_capacity(if call_args_pack.is_some() {
            2
        } else {
            args.len() + usize::from(receiver.is_some())
        });
        if let Some(pack_ty) = call_args_pack {
            let logical_args = if let Some(receiver) = receiver {
                values.push(SOperand::expr(
                    self.lower_callable_receiver(expr, receiver),
                    receiver,
                ));
                args
            } else {
                let (receiver, logical_args) = args
                    .split_first()
                    .expect("call trait invocation must include a receiver");
                values.push(self.lower_expr_operand(*receiver));
                logical_args
            };
            let field_tys = pack_ty.field_types(self.db);
            debug_assert_eq!(logical_args.len(), field_tys.len());
            let fields = logical_args
                .iter()
                .zip(field_tys)
                .map(|(arg, ty)| {
                    self.lower_expr_operand(*arg).with_intent(
                        if ty.as_capability(self.db).is_some() {
                            SOperandIntent::Read
                        } else {
                            SOperandIntent::Move
                        },
                    )
                })
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let pack = self.emit_expr(
                pack_ty,
                SExpr::AggregateMake {
                    ty: pack_ty,
                    fields,
                },
            );
            values.push(SOperand::synthetic(pack).with_intent(SOperandIntent::Move));
        } else {
            if let Some(receiver) = receiver {
                values.push(SOperand::expr(
                    self.lower_callable_receiver(expr, receiver),
                    receiver,
                ));
            }
            for arg in args {
                values.push(self.lower_expr_operand(*arg));
            }
        }

        match callable.callable_def() {
            CallableDef::VariantCtor(variant) => self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::EnumMake {
                    enum_ty: ty,
                    variant: VariantIndex(variant.idx),
                    fields: values.into_boxed_slice(),
                },
            ),
            CallableDef::Func(_) => {
                let call_site = self
                    .call_sites
                    .get(expr.index())
                    .and_then(|site| site.as_ref())
                    .unwrap_or_else(|| {
                        panic!("call lowering plan missing semantic callee for {expr:?}")
                    });
                let callee = call_site
                    .callee
                    .unwrap_or_else(|| panic!("call lowering plan missing callee for {expr:?}"));
                let effect_args = self.lower_effect_arg_slice(&call_site.effect_args);
                let (return_sources, return_sources_complete) =
                    self.lower_call_return_sources(callee, &values, &effect_args);
                let value = self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::Call {
                        call_site: CallSiteId::Expr(expr),
                        callee,
                        args: values.into_boxed_slice(),
                        effect_args,
                        return_sources,
                        return_sources_complete,
                    },
                );
                if SemanticInstance::new(self.db, callee.key)
                    .normalized_result_ty(self.db)
                    .is_never(self.db)
                {
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Goto(self.current),
                    );
                }
                value
            }
        }
    }

    fn lower_call_return_sources(
        &mut self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        args: &[SOperand],
        effect_args: &[SEffectArg<'db>],
    ) -> (Box<[SCallReturnSource]>, bool) {
        let callable_body = callee.key.callable_body(self.db);
        let mut sources = match callable_body.return_provenance(self.db) {
            ReturnProvenance::Forwarded(sources) => sources,
            ReturnProvenance::Fresh | ReturnProvenance::Unknown => Vec::new(),
        };
        let (forwarded_sources, mut sources_complete) =
            callable_body.forwarded_return_sources_with_completeness(self.db);
        // Exact carrier provenance stays unchanged, while borrowed result
        // slots also retain every conservative input source. Callsite
        // instantiation is needed for ordinary functions as well as closures:
        // it replaces type-level dynamic-index descriptors with caller-local
        // snapshot values before normalized analyses consume the call.
        let result_ty = SemanticInstance::new(self.db, callee.key).normalized_result_ty(self.db);
        let borrow_results = return_borrow_results_in_ty(self.db, result_ty);
        sources.extend(forwarded_sources.into_iter().filter(|source| {
            borrow_results.iter().any(|result| {
                return_source_result_projection_overlaps_borrow(
                    &source.result_projection,
                    &result.projection,
                )
            })
        }));
        sources.sort_unstable();
        sources.dedup();
        sources.retain(|source| {
            let relevant_results = borrow_results
                .iter()
                .filter(|result| {
                    return_source_result_projection_overlaps_borrow(
                        &source.result_projection,
                        &result.projection,
                    )
                })
                .collect::<Vec<_>>();
            if relevant_results.is_empty() {
                return true;
            }
            let Some(input_ty) = self.call_return_source_input_ty(callee, source) else {
                sources_complete = false;
                return true;
            };
            let mut reaches_capability = false;
            let mut unclassified = false;
            for result in relevant_results {
                let Some(reaches) = return_source_borrow_input_reaches_capability(
                    self.db,
                    input_ty,
                    source,
                    &result.projection,
                ) else {
                    unclassified = true;
                    continue;
                };
                reaches_capability |= reaches;
            }
            if unclassified {
                sources_complete = false;
            }
            reaches_capability || unclassified
        });
        let mut index_values = FxHashMap::default();
        let sources = sources
            .into_iter()
            .map(|source| {
                let result_projection = self.lower_call_return_projection(
                    &source.result_projection,
                    args,
                    effect_args,
                    &mut index_values,
                );
                let projection = self.lower_call_return_projection(
                    &source.projection,
                    args,
                    effect_args,
                    &mut index_values,
                );
                SCallReturnSource {
                    result_projection,
                    origin: source.origin,
                    projection,
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        (sources, sources_complete)
    }

    fn call_return_source_input_ty(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        source: &crate::analysis::ty::ty_check::ReturnSource,
    ) -> Option<TyId<'db>> {
        let instance = SemanticInstance::new(self.db, callee.key);
        let callable = callee.key.callable_body(self.db);
        let binding = callable
            .param_bindings(self.db)
            .into_iter()
            .find(|binding| binding.callable_input_origin(self.db) == Some(source.origin))?;
        Some(instance.normalized_binding_ty(self.db, binding))
    }

    fn lower_call_return_projection(
        &mut self,
        projection: &[ReturnProjectionStep],
        args: &[SOperand],
        effect_args: &[SEffectArg<'db>],
        index_values: &mut FxHashMap<ReturnIndexSource, SLocalId>,
    ) -> Vec<SCallReturnProjectionStep> {
        projection
            .iter()
            .map(|step| match step {
                ReturnProjectionStep::Field(field) => SCallReturnProjectionStep::Field(*field),
                ReturnProjectionStep::VariantField { variant, field } => {
                    SCallReturnProjectionStep::VariantField {
                        variant: *variant,
                        field: *field,
                    }
                }
                ReturnProjectionStep::ConstantIndex(index) => {
                    SCallReturnProjectionStep::ConstantIndex(*index)
                }
                ReturnProjectionStep::DynamicIndex(source) => self
                    .lower_call_return_index_source(source, args, effect_args, index_values)
                    .map_or(SCallReturnProjectionStep::AnyIndex, |index| {
                        SCallReturnProjectionStep::DynamicIndex(index)
                    }),
                ReturnProjectionStep::AnyIndex => SCallReturnProjectionStep::AnyIndex,
            })
            .collect()
    }

    fn lower_call_return_index_source(
        &mut self,
        source: &ReturnIndexSource,
        args: &[SOperand],
        effect_args: &[SEffectArg<'db>],
        index_values: &mut FxHashMap<ReturnIndexSource, SLocalId>,
    ) -> Option<SLocalId> {
        if let Some(value) = index_values.get(source).copied() {
            return Some(value);
        }
        let (mut ty, mut place) = self.call_return_input_place(source.origin, args, effect_args)?;
        for step in &source.projection {
            ty = ty.as_capability(self.db).map_or(ty, |(_, inner)| inner);
            match step {
                ReturnProjectionStep::Field(field) => {
                    place.path.push(Projection::Field(usize::from(*field)));
                    ty = *ty.field_types(self.db).get(usize::from(*field))?;
                }
                ReturnProjectionStep::ConstantIndex(index) => {
                    place
                        .path
                        .push(Projection::Index(IndexSource::Constant(*index)));
                    ty = *ty.generic_args(self.db).first()?;
                }
                ReturnProjectionStep::VariantField { .. }
                | ReturnProjectionStep::DynamicIndex(_)
                | ReturnProjectionStep::AnyIndex => return None,
            }
        }
        let index_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
        let value = self.emit_expr(
            index_ty,
            SExpr::ReadPlace {
                place,
                intent: SOperandIntent::Read,
            },
        );
        index_values.insert(source.clone(), value);
        Some(value)
    }

    fn call_return_input_place(
        &self,
        origin: CallableInputLayoutHoleOrigin,
        args: &[SOperand],
        effect_args: &[SEffectArg<'db>],
    ) -> Option<(TyId<'db>, SPlace<'db>)> {
        let value = match origin {
            CallableInputLayoutHoleOrigin::Receiver => args.first()?.value,
            CallableInputLayoutHoleOrigin::ValueParam(param_idx) => args.get(param_idx)?.value,
            CallableInputLayoutHoleOrigin::Effect(effect_idx) => {
                let effect_arg = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx as usize == effect_idx)?;
                return match &effect_arg.arg {
                    SEffectArgValue::Place(place) => Some((
                        effect_arg
                            .target_ty
                            .unwrap_or(self.locals[place.local.index()].ty),
                        place.clone(),
                    )),
                    SEffectArgValue::Value(value) => Some((
                        effect_arg
                            .target_ty
                            .unwrap_or(self.locals[value.value.index()].ty),
                        SPlace::new(value.value),
                    )),
                };
            }
        };
        Some((self.locals[value.index()].ty, SPlace::new(value)))
    }

    fn lower_callable_receiver(&mut self, call_expr: ExprId, receiver: ExprId) -> SValueId {
        if let Some(plan) = self
            .call_sites
            .get(call_expr.index())
            .and_then(|site| site.as_ref())
            .and_then(|plan| plan.receiver)
        {
            let receiver_prop = self.typed_body.expr_prop(self.db, receiver);
            let place = if let Some(place) = self.typed_body.expr_place(receiver) {
                self.lower_place_data(place)
            } else {
                let value = self.lower_expr(receiver);
                let local = self.alloc_local(
                    plan.receiver_ty,
                    if matches!(plan.kind, BorrowKind::Mut) {
                        Mutability::Mutable
                    } else {
                        Mutability::Immutable
                    },
                    None,
                );
                self.push_stmt(
                    SemOrigin::Expr(receiver),
                    SStmtKind::Assign {
                        dst: local,
                        expr: SExpr::UseValue(SOperand::inherited(value)),
                    },
                );
                SPlace::new(local)
            };
            return self.emit_expr_with_origin(
                SemOrigin::Expr(call_expr),
                plan.borrowed_ty,
                SExpr::Borrow {
                    place,
                    kind: plan.kind,
                    provider: receiver_prop.borrow_provider,
                    activation: if plan.kind == BorrowKind::Mut {
                        BorrowActivation::AtCall
                    } else {
                        BorrowActivation::Immediate
                    },
                },
            );
        }

        self.lower_expr(receiver)
    }

    fn lower_const_intrinsic(
        &mut self,
        expr: ExprId,
        callable: &crate::analysis::ty::ty_check::Callable<'db>,
        kind: ConstIntrinsicKind,
        result_ty: TyId<'db>,
    ) -> SValueId {
        let ty = match kind {
            ConstIntrinsicKind::SizeOf => normalize_ty(
                self.db,
                *callable
                    .generic_args()
                    .first()
                    .expect("core::size_of lowering requires a concrete generic arg"),
                self.body.scope(),
                self.assumptions,
            ),
        };
        let size = runtime_size_bytes(self.db, ty).unwrap_or_else(|| {
            panic!(
                "core::size_of should resolve for {}",
                ty.pretty_print(self.db)
            )
        });
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            result_ty,
            SExpr::Const(SConst::Value(int_const(
                self.db,
                result_ty,
                BigInt::from(size),
            ))),
        )
    }

    fn lower_block_expr(&mut self, stmts: &[StmtId]) -> SValueId {
        let Some((tail, head)) = stmts.split_last() else {
            return self.unit_value();
        };

        for stmt in head {
            self.lower_stmt(*stmt);
            if self.is_terminated(self.current) {
                return self.unit_value();
            }
        }

        match tail.data(self.db, self.body) {
            Partial::Present(Stmt::Expr(expr)) => self.lower_expr(*expr),
            _ => {
                self.lower_stmt(*tail);
                self.unit_value()
            }
        }
    }

    fn lower_stmt(&mut self, stmt: StmtId) {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            panic!("cannot lower absent statement")
        };
        let origin = SemOrigin::Stmt(stmt);

        match stmt_data {
            Stmt::Let(pat, _, init) => {
                if let Some(init) = init {
                    let value = self.lower_expr(*init);
                    self.bind_pattern(*pat, value, SemOrigin::Expr(*init));
                }
            }
            Stmt::While(cond, body_expr) => self.lower_while(*cond, *body_expr),
            Stmt::For(pat, iter, body_expr, _) => self.lower_for(stmt, *pat, *iter, *body_expr),
            Stmt::Continue => {
                let scope = self
                    .loop_stack
                    .last()
                    .copied()
                    .expect("continue outside loop");
                self.set_terminator(
                    self.current,
                    origin,
                    STerminatorKind::Goto(scope.continue_bb),
                );
            }
            Stmt::Break => {
                let is_reachable = !self.is_terminated(self.current);
                let scope = self.loop_stack.last_mut().expect("break outside loop");
                scope.has_reachable_break |= is_reachable;
                let break_bb = scope.break_bb;
                self.set_terminator(self.current, origin, STerminatorKind::Goto(break_bb));
            }
            Stmt::Return(expr) => {
                let value = expr.map(|expr| self.lower_expr_operand(expr));
                self.set_terminator(
                    self.current,
                    origin,
                    if expr.is_some_and(|expr| self.expr_ty(expr) == TyId::unit(self.db)) {
                        STerminatorKind::Return(None)
                    } else {
                        STerminatorKind::Return(value)
                    },
                );
            }
            Stmt::Expr(expr) => {
                let _ = self.lower_expr(*expr);
            }
        }
    }

    fn lower_while(&mut self, cond: CondId, body_expr: ExprId) {
        let cond_bb = self.new_block();
        let body_bb = self.new_block();
        let exit_bb = self.new_block();
        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(cond_bb));

        self.switch_to(cond_bb);
        let reachable = self.lower_cond_branch(cond, body_bb, exit_bb);

        self.loop_stack.push(LoopScope {
            continue_bb: cond_bb,
            break_bb: exit_bb,
            has_reachable_break: false,
        });
        self.switch_to(body_bb);
        if reachable.then_branch {
            let _ = self.lower_expr(body_expr);
            if !self.is_terminated(self.current) {
                self.set_synthetic_terminator(self.current, STerminatorKind::Goto(cond_bb));
            }
        } else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }
        let scope = self.loop_stack.pop().expect("while loop scope");

        if !reachable.else_branch && !scope.has_reachable_break {
            self.set_synthetic_terminator(exit_bb, STerminatorKind::Goto(exit_bb));
        }
        self.switch_to(exit_bb);
    }

    fn lower_for(&mut self, stmt: StmtId, pat: PatId, iter: ExprId, body_expr: ExprId) {
        let for_loop_call_sites = self
            .for_loop_call_sites
            .get(stmt.index())
            .and_then(|sites| sites.as_ref())
            .unwrap_or_else(|| panic!("missing staged callee refs for for-loop {stmt:?}"));
        let seq = self
            .typed_body
            .for_loop_seq(stmt)
            .unwrap_or_else(|| panic!("missing Seq resolution for for-loop {stmt:?}"));
        let iter_value = self.lower_expr(iter);
        let iter_operand = SOperand::expr(iter_value, iter);
        let usize_ty = seq.len_callable.ret_ty(self.db);
        let elem_ty = seq.elem_ty;
        let idx_local = self.alloc_temp(usize_ty);
        self.push_synthetic_stmt(SStmtKind::Assign {
            dst: idx_local,
            expr: SExpr::Const(SConst::Value(int_const(
                self.db,
                usize_ty,
                BigInt::default(),
            ))),
        });
        let len_effect_args = self.lower_effect_arg_slice(&for_loop_call_sites.len.effect_args);
        let len_callee = for_loop_call_sites
            .len
            .callee
            .expect("Seq::len should lower to a semantic callee");
        let len_args = [iter_operand];
        let (len_return_sources, len_return_sources_complete) =
            self.lower_call_return_sources(len_callee, &len_args, &len_effect_args);
        let len_value = self.emit_expr(
            usize_ty,
            SExpr::Call {
                call_site: CallSiteId::ForLoopLen(stmt),
                callee: len_callee,
                args: Box::new(len_args),
                effect_args: len_effect_args,
                return_sources: len_return_sources,
                return_sources_complete: len_return_sources_complete,
            },
        );

        let cond_bb = self.new_block();
        let body_bb = self.new_block();
        let exit_bb = self.new_block();
        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(cond_bb));

        self.switch_to(cond_bb);
        let cond = self.emit_expr(
            TyId::bool(self.db),
            SExpr::Binary {
                op: BinOp::Comp(CompBinOp::Lt),
                lhs: SOperand::synthetic(idx_local),
                rhs: SOperand::synthetic(len_value),
            },
        );
        self.set_synthetic_terminator(
            self.current,
            STerminatorKind::Branch {
                cond: SOperand::synthetic(cond),
                then_bb: body_bb,
                else_bb: exit_bb,
            },
        );

        self.loop_stack.push(LoopScope {
            continue_bb: cond_bb,
            break_bb: exit_bb,
            has_reachable_break: false,
        });
        self.switch_to(body_bb);
        let get_effect_args = self.lower_effect_arg_slice(&for_loop_call_sites.get.effect_args);
        let get_callee = for_loop_call_sites
            .get
            .callee
            .expect("Seq::get should lower to a semantic callee");
        let get_args = [iter_operand, SOperand::synthetic(idx_local)];
        let (get_return_sources, get_return_sources_complete) =
            self.lower_call_return_sources(get_callee, &get_args, &get_effect_args);
        let elem = self.emit_expr(
            elem_ty,
            SExpr::Call {
                call_site: CallSiteId::ForLoopGet(stmt),
                callee: get_callee,
                args: Box::new(get_args),
                effect_args: get_effect_args,
                return_sources: get_return_sources,
                return_sources_complete: get_return_sources_complete,
            },
        );
        if seq.element_layout_backing_source {
            self.locals[elem.index()].layout_backing_sources = vec![LayoutBackingSource {
                target: Vec::new(),
                source: LayoutBackingPlace::Local(SPlace::dynamic_index(iter_value, idx_local)),
            }];
            self.assigned_layout_backing_sources[elem.index()] = true;
        }
        self.bind_pattern(pat, elem, SemOrigin::Stmt(stmt));
        let _ = self.lower_expr(body_expr);
        if !self.is_terminated(self.current) {
            let one = self.emit_expr(
                usize_ty,
                SExpr::Const(SConst::Value(int_const(
                    self.db,
                    usize_ty,
                    BigInt::from(1u8),
                ))),
            );
            let next = self.emit_expr(
                usize_ty,
                SExpr::Binary {
                    op: BinOp::Arith(ArithBinOp::Add),
                    lhs: SOperand::synthetic(idx_local),
                    rhs: SOperand::synthetic(one),
                },
            );
            self.push_synthetic_stmt(SStmtKind::Assign {
                dst: idx_local,
                expr: SExpr::UseValue(SOperand::synthetic(next)),
            });
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(cond_bb));
        }
        self.loop_stack.pop();
        self.switch_to(exit_bb);
    }

    fn lower_if_expr(
        &mut self,
        cond: CondId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
        result_ty: TyId<'db>,
    ) -> SValueId {
        let result = self.alloc_temp(result_ty);
        let then_bb = self.new_block();
        let else_bb = self.new_block();
        let join_bb = self.new_block();
        let mut join_reachable = false;

        let reachable = self.lower_cond_branch(cond, then_bb, else_bb);
        let known_condition = self.known_cond_bool(cond);
        let then_result_reachable = known_condition != Some(false);
        let else_result_reachable = known_condition != Some(true);

        self.switch_to(then_bb);
        if reachable.then_branch {
            let then_value = self.lower_expr(then_expr);
            if !self.is_terminated(self.current) {
                if then_result_reachable && self.typed_body.expr_can_complete_normally(then_expr) {
                    join_reachable = true;
                    self.push_synthetic_stmt(SStmtKind::Assign {
                        dst: result,
                        expr: SExpr::Forward(
                            SOperand::expr(then_value, then_expr)
                                .with_intent(self.expr_operand_intent(then_expr)),
                        ),
                    });
                    self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
                } else {
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Goto(self.current),
                    );
                }
            }
        } else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }

        self.switch_to(else_bb);
        if reachable.else_branch {
            let else_value = if let Some(expr) = else_expr {
                SOperand::expr(self.lower_expr(expr), expr)
                    .with_intent(self.expr_operand_intent(expr))
            } else {
                SOperand::synthetic(self.unit_value())
            };
            if !self.is_terminated(self.current) {
                let else_completes =
                    else_expr.is_none_or(|expr| self.typed_body.expr_can_complete_normally(expr));
                if else_result_reachable && else_completes {
                    join_reachable = true;
                    self.push_synthetic_stmt(SStmtKind::Assign {
                        dst: result,
                        expr: SExpr::Forward(else_value),
                    });
                    self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
                } else {
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Goto(self.current),
                    );
                }
            }
        } else {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }

        if !join_reachable {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
        result
    }

    fn known_cond_bool(&self, cond: CondId) -> Option<bool> {
        self.typed_body.cond_normal_bool_value(cond)
    }

    fn lower_logical_expr(&mut self, expr: ExprId) -> SValueId {
        // This deliberately lowers expression-context `&&` and `||` through a
        // simple true/false/join CFG, matching condition lowering and keeping
        // RHS evaluation lazy. The pre-opt Sonatina IR has extra constant
        // assignment blocks, but normal Sonatina optimization collapses this
        // shape and can use the LHS branch fact to simplify checked RHS
        // arithmetic. A cleaner Fe-side lowering could instead thread the
        // destination temp through recursive logical lowering and assign
        // directly on each short-circuit edge, e.g. `a || b` writes `true` from
        // the LHS-true edge and only lowers/assigns `b` on the LHS-false edge.
        let result = self.alloc_temp(TyId::bool(self.db));
        let true_bb = self.new_block();
        let false_bb = self.new_block();
        let join_bb = self.new_block();
        let mut join_reachable = false;

        let reachable = self.lower_expr_branch(expr, true_bb, false_bb);

        self.switch_to(true_bb);
        if reachable.then_branch && !self.is_terminated(self.current) {
            join_reachable = true;
            let value = self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                TyId::bool(self.db),
                SExpr::Const(SConst::Value(bool_const(self.db, true))),
            );
            self.push_synthetic_stmt(SStmtKind::Assign {
                dst: result,
                expr: SExpr::Forward(SOperand::synthetic(value)),
            });
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        } else if !reachable.then_branch {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }

        self.switch_to(false_bb);
        if reachable.else_branch && !self.is_terminated(self.current) {
            join_reachable = true;
            let value = self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                TyId::bool(self.db),
                SExpr::Const(SConst::Value(bool_const(self.db, false))),
            );
            self.push_synthetic_stmt(SStmtKind::Assign {
                dst: result,
                expr: SExpr::Forward(SOperand::synthetic(value)),
            });
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        } else if !reachable.else_branch {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(self.current));
        }

        if !join_reachable {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
        result
    }

    fn lower_expr_branch(
        &mut self,
        expr: ExprId,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) -> BranchReachability {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            panic!("cannot lower absent condition expression")
        };

        match expr_data {
            Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::And)) => {
                let rhs_bb = self.new_block();
                let lhs_reachable = self.lower_expr_branch(*lhs, rhs_bb, else_bb);
                self.switch_to(rhs_bb);
                let rhs_reachable = if lhs_reachable.then_branch {
                    self.lower_expr_branch(*rhs, then_bb, else_bb)
                } else {
                    self.set_synthetic_terminator(rhs_bb, STerminatorKind::Goto(rhs_bb));
                    BranchReachability::default()
                };
                BranchReachability {
                    then_branch: lhs_reachable.then_branch && rhs_reachable.then_branch,
                    else_branch: lhs_reachable.else_branch
                        || lhs_reachable.then_branch && rhs_reachable.else_branch,
                }
            }
            Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::Or)) => {
                let rhs_bb = self.new_block();
                let lhs_reachable = self.lower_expr_branch(*lhs, then_bb, rhs_bb);
                self.switch_to(rhs_bb);
                let rhs_reachable = if lhs_reachable.else_branch {
                    self.lower_expr_branch(*rhs, then_bb, else_bb)
                } else {
                    self.set_synthetic_terminator(rhs_bb, STerminatorKind::Goto(rhs_bb));
                    BranchReachability::default()
                };
                BranchReachability {
                    then_branch: lhs_reachable.then_branch
                        || lhs_reachable.else_branch && rhs_reachable.then_branch,
                    else_branch: lhs_reachable.else_branch && rhs_reachable.else_branch,
                }
            }
            _ => {
                let cond = self.lower_expr(expr);
                if self.is_terminated(self.current) {
                    return BranchReachability::default();
                }
                if let Some(value) = self.typed_body.expr_normal_bool_value(expr) {
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Goto(if value { then_bb } else { else_bb }),
                    );
                    return BranchReachability {
                        then_branch: value,
                        else_branch: !value,
                    };
                }
                self.set_synthetic_terminator(
                    self.current,
                    STerminatorKind::Branch {
                        cond: SOperand::expr(cond, expr),
                        then_bb,
                        else_bb,
                    },
                );
                BranchReachability {
                    then_branch: true,
                    else_branch: true,
                }
            }
        }
    }

    fn lower_match_expr(
        &mut self,
        scrutinee: ExprId,
        arms: &Partial<Vec<MatchArm>>,
        result_ty: TyId<'db>,
    ) -> SValueId {
        let Partial::Present(arms) = arms else {
            panic!("match arms missing")
        };
        let result_reachability = self
            .known_pattern_scrutinee(scrutinee)
            .and_then(|scrutinee| {
                known_scrutinee_arm_reachability(
                    self.db,
                    self.typed_body.pattern_store(),
                    arms.iter().map(|arm| arm.pat),
                    &scrutinee,
                )
            });
        let value = self.lower_expr(scrutinee);
        let result = self.alloc_temp(result_ty);
        let join_bb = self.new_block();
        if self.is_terminated(self.current) {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
            self.switch_to(join_bb);
            return result;
        }
        self.lower_match_expr_with_decision_tree(
            value,
            SemOrigin::Expr(scrutinee),
            result,
            join_bb,
            arms,
            result_reachability.as_deref(),
        )
    }

    fn known_pattern_scrutinee(&self, expr: ExprId) -> Option<KnownPatternScrutinee<'db>> {
        if let Some(const_ref) = self.typed_body.expr_const_ref(expr)
            && let Some(const_ref) = resolve_semantic_const_ref(
                self.db,
                const_ref,
                normalize_ty(
                    self.db,
                    self.expr_ty(expr),
                    self.body.scope(),
                    self.assumptions,
                ),
                SemOrigin::Expr(expr),
            )
            && let Ok(value) = eval_const_ref(self.db, const_ref)
        {
            return Some(known_pattern_scrutinee_from_const(self.db, value));
        }
        let ty = normalize_ty(
            self.db,
            self.expr_ty(expr),
            self.body.scope(),
            self.assumptions,
        );
        if ty.is_integral(self.db)
            && let Some(value) = try_eval_const_int_expr(self.db, self.body, expr, ty)
        {
            let (sign, bytes) = value.to_bytes_be();
            if sign != Sign::Minus {
                return Some(KnownPatternScrutinee::Literal(LitKind::Int(
                    IntegerId::new(self.db, BigUint::from_bytes_be(&bytes)),
                )));
            }
        }

        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return None;
        };
        match expr_data {
            Expr::Lit(lit) => Some(KnownPatternScrutinee::Literal(*lit)),
            Expr::Path(_) => match self.typed_body.value_path_ref(expr) {
                Some(ValuePathRef::UnitVariant(variant)) => Some(KnownPatternScrutinee::variant(
                    variant.variant,
                    std::iter::empty(),
                )),
                Some(ValuePathRef::TypeConst(_) | ValuePathRef::FunctionItem) | None => None,
            },
            Expr::Tuple(fields) | Expr::Array(fields) => {
                let ty = normalize_ty(
                    self.db,
                    self.expr_ty(expr),
                    self.body.scope(),
                    self.assumptions,
                );
                Some(KnownPatternScrutinee::type_constructor(
                    ty,
                    fields.iter().map(|field| {
                        self.known_pattern_scrutinee(*field)
                            .unwrap_or(KnownPatternScrutinee::Unknown)
                    }),
                ))
            }
            Expr::RecordInit(_, fields) => {
                let (record_like, constructor) = match self.typed_body.record_init_lowering(expr)? {
                    RecordInitLowering::Struct => {
                        let ty = normalize_ty(
                            self.db,
                            self.expr_ty(expr),
                            self.body.scope(),
                            self.assumptions,
                        );
                        (
                            RecordLike::Type(ty),
                            KnownPatternScrutinee::type_constructor(ty, std::iter::empty()),
                        )
                    }
                    RecordInitLowering::EnumVariant(variant) => (
                        RecordLike::from_variant(variant),
                        KnownPatternScrutinee::variant(variant.variant, std::iter::empty()),
                    ),
                };
                let mut known_fields =
                    vec![KnownPatternScrutinee::Unknown; record_like.record_labels(self.db).len()];
                for field in fields {
                    let Some(label) = field.label_eagerly(self.db, self.body) else {
                        continue;
                    };
                    let Some(field_idx) = record_like.record_field_idx(self.db, label) else {
                        continue;
                    };
                    let Some(slot) = known_fields.get_mut(field_idx) else {
                        continue;
                    };
                    *slot = self
                        .known_pattern_scrutinee(field.expr)
                        .unwrap_or(KnownPatternScrutinee::Unknown);
                }
                Some(match constructor {
                    KnownPatternScrutinee::Variant { variant, .. } => {
                        KnownPatternScrutinee::variant(variant, known_fields)
                    }
                    KnownPatternScrutinee::Type { ty, .. } => {
                        KnownPatternScrutinee::type_constructor(ty, known_fields)
                    }
                    KnownPatternScrutinee::Unknown | KnownPatternScrutinee::Literal(_) => {
                        unreachable!("record constructors have a structural shape")
                    }
                })
            }
            Expr::Call(_, args) => {
                let SemanticExprLowering::Call { callable, .. } =
                    self.typed_body.semantic_expr_lowering(expr)?
                else {
                    return None;
                };
                match callable.callable_def() {
                    CallableDef::VariantCtor(variant) => Some(KnownPatternScrutinee::variant(
                        variant,
                        args.iter().map(|arg| {
                            self.known_pattern_scrutinee(arg.expr)
                                .unwrap_or(KnownPatternScrutinee::Unknown)
                        }),
                    )),
                    CallableDef::Func(_) => None,
                }
            }
            Expr::Block(stmts) => {
                let tail = stmts.last()?;
                match tail.data(self.db, self.body) {
                    Partial::Present(Stmt::Expr(tail)) => self.known_pattern_scrutinee(*tail),
                    _ => None,
                }
            }
            Expr::With(_, body) => self.known_pattern_scrutinee(*body),
            Expr::If(cond, then_expr, Some(else_expr)) => {
                let known_condition = self.typed_body.cond_normal_bool_value(*cond);
                self.merge_known_pattern_scrutinees(
                    [
                        (known_condition != Some(false)).then_some(*then_expr),
                        (known_condition != Some(true)).then_some(*else_expr),
                    ]
                    .into_iter()
                    .flatten(),
                )
            }
            Expr::Match(scrutinee, Partial::Present(arms)) => {
                if !self.typed_body.expr_can_complete_normally(*scrutinee) {
                    return None;
                }
                let reachable = self
                    .known_pattern_scrutinee(*scrutinee)
                    .and_then(|scrutinee| {
                        known_scrutinee_arm_reachability(
                            self.db,
                            self.typed_body.pattern_store(),
                            arms.iter().map(|arm| arm.pat),
                            &scrutinee,
                        )
                    });
                self.merge_known_pattern_scrutinees(
                    arms.iter()
                        .enumerate()
                        .filter(|(idx, _)| {
                            reachable.as_ref().is_none_or(|reachable| reachable[*idx])
                        })
                        .map(|(_, arm)| arm.body),
                )
            }
            Expr::Cast(inner, _) if ty.is_integral(self.db) => {
                match self.known_pattern_scrutinee(*inner)? {
                    KnownPatternScrutinee::Literal(LitKind::Int(value)) => {
                        Some(KnownPatternScrutinee::Literal(LitKind::Int(value)))
                    }
                    KnownPatternScrutinee::Literal(LitKind::Bool(value)) => {
                        Some(KnownPatternScrutinee::Literal(LitKind::Int(
                            IntegerId::new(self.db, BigUint::from(u8::from(value))),
                        )))
                    }
                    KnownPatternScrutinee::Unknown
                    | KnownPatternScrutinee::Variant { .. }
                    | KnownPatternScrutinee::Type { .. }
                    | KnownPatternScrutinee::Literal(_) => None,
                }
            }
            Expr::Closure { .. }
            | Expr::Bin(..)
            | Expr::Un(..)
            | Expr::Cast(..)
            | Expr::Assert(..)
            | Expr::MethodCall(..)
            | Expr::Field(..)
            | Expr::ArrayRep(..)
            | Expr::If(..)
            | Expr::Match(_, Partial::Absent)
            | Expr::Assign(..)
            | Expr::AugAssign(..) => None,
        }
    }

    fn merge_known_pattern_scrutinees(
        &self,
        exprs: impl IntoIterator<Item = ExprId>,
    ) -> Option<KnownPatternScrutinee<'db>> {
        let mut merged = None;
        for expr in exprs {
            if !self.typed_body.expr_can_complete_normally(expr) {
                continue;
            }
            let value = self
                .known_pattern_scrutinee(expr)
                .unwrap_or(KnownPatternScrutinee::Unknown);
            match &merged {
                Some(previous) if previous != &value => return None,
                Some(_) => {}
                None => merged = Some(value),
            }
        }
        merged
    }

    fn lower_cond_branch(
        &mut self,
        cond: CondId,
        then_bb: SBlockId,
        else_bb: SBlockId,
    ) -> BranchReachability {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            panic!("cannot lower absent condition")
        };

        match cond_data {
            Cond::Expr(expr) => {
                let cond = self.lower_expr(*expr);
                if self.is_terminated(self.current) {
                    return BranchReachability::default();
                }
                if let Some(value) = self.typed_body.expr_normal_bool_value(*expr) {
                    self.set_synthetic_terminator(
                        self.current,
                        STerminatorKind::Goto(if value { then_bb } else { else_bb }),
                    );
                    return BranchReachability {
                        then_branch: value,
                        else_branch: !value,
                    };
                }
                self.set_synthetic_terminator(
                    self.current,
                    STerminatorKind::Branch {
                        cond: SOperand::expr(cond, *expr),
                        then_bb,
                        else_bb,
                    },
                );
                BranchReachability {
                    then_branch: true,
                    else_branch: true,
                }
            }
            Cond::Bin(lhs, rhs, LogicalBinOp::And) => {
                let rhs_bb = self.new_block();
                let lhs_reachable = self.lower_cond_branch(*lhs, rhs_bb, else_bb);
                self.switch_to(rhs_bb);
                let rhs_reachable = if lhs_reachable.then_branch {
                    self.lower_cond_branch(*rhs, then_bb, else_bb)
                } else {
                    self.set_synthetic_terminator(rhs_bb, STerminatorKind::Goto(rhs_bb));
                    BranchReachability::default()
                };
                BranchReachability {
                    then_branch: lhs_reachable.then_branch && rhs_reachable.then_branch,
                    else_branch: lhs_reachable.else_branch
                        || lhs_reachable.then_branch && rhs_reachable.else_branch,
                }
            }
            Cond::Bin(lhs, rhs, LogicalBinOp::Or) => {
                let rhs_bb = self.new_block();
                let lhs_reachable = self.lower_cond_branch(*lhs, then_bb, rhs_bb);
                self.switch_to(rhs_bb);
                let rhs_reachable = if lhs_reachable.else_branch {
                    self.lower_cond_branch(*rhs, then_bb, else_bb)
                } else {
                    self.set_synthetic_terminator(rhs_bb, STerminatorKind::Goto(rhs_bb));
                    BranchReachability::default()
                };
                BranchReachability {
                    then_branch: lhs_reachable.then_branch
                        || lhs_reachable.else_branch && rhs_reachable.then_branch,
                    else_branch: lhs_reachable.else_branch && rhs_reachable.else_branch,
                }
            }
            Cond::Let(pat, expr) => {
                let value = self.lower_expr(*expr);
                if self.is_terminated(self.current) {
                    return BranchReachability::default();
                }
                let known_scrutinee = self.known_pattern_scrutinee(*expr);
                let reachable = single_pattern_branch_reachability(
                    self.db,
                    self.typed_body.pattern_store(),
                    *pat,
                    known_scrutinee.as_ref(),
                )
                .unwrap_or(PatternBranchReachability::BOTH);
                match (reachable.can_match, reachable.can_miss) {
                    (true, false) => {
                        self.bind_pattern(*pat, value, SemOrigin::Expr(*expr));
                        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
                    }
                    (false, true) => {
                        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(else_bb));
                    }
                    (true, true) => self.lower_pattern_branch(
                        *pat,
                        value,
                        SemOrigin::Expr(*expr),
                        then_bb,
                        else_bb,
                    ),
                    (false, false) => unreachable!("a pattern must match or miss"),
                }
                BranchReachability {
                    then_branch: reachable.can_match,
                    else_branch: reachable.can_miss,
                }
            }
        }
    }

    /// Lowers the physical destination of an assignment.
    ///
    /// A projected capability field has two distinct locations: the field
    /// slot that stores the handle and the pointee reached through that
    /// handle. Explicit capability construction rebinds the former. Ordinary
    /// assignment materializes the stored handle as a carrier local so the
    /// semantic `Store` is rooted directly at the latter; runtime lowering no
    /// longer has to guess from the destination type.
    fn lower_assignment_place(&mut self, expr: ExprId, rebinds_capability: bool) -> SPlace<'db> {
        let place = self.lower_place(expr);
        let ty = self.expr_ty(expr);
        if rebinds_capability || ty.as_capability(self.db).is_none() || place.path.is_empty() {
            return place;
        }
        let carrier = self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::ReadPlace {
                place,
                intent: SOperandIntent::Read,
            },
        );
        SPlace::new(carrier)
    }

    fn place_needs_indirect_store(&self, place: &SPlace<'db>) -> bool {
        let Some(local) = self.locals.get(place.local.index()) else {
            return false;
        };
        if local.ty.as_capability(self.db).is_some() {
            return true;
        }
        let Some(binding) = local.source else {
            return false;
        };
        if matches!(
            binding,
            LocalBinding::EffectParam { .. }
                | LocalBinding::Param {
                    site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
                    ..
                }
        ) {
            return true;
        }
        self.binding_ty(binding).as_capability(self.db).is_some()
    }

    fn place_can_assign_directly(&self, place: &SPlace<'db>) -> bool {
        place.path.is_empty() && !self.place_needs_indirect_store(place)
    }

    fn push_place_write(
        &mut self,
        origin: SemOrigin<'db>,
        dst: SPlace<'db>,
        src: SOperand,
        rebinds_capability: bool,
    ) {
        let kind = if (rebinds_capability && dst.path.is_empty())
            || self.place_can_assign_directly(&dst)
        {
            SStmtKind::Assign {
                dst: dst.local,
                expr: SExpr::UseValue(src),
            }
        } else {
            SStmtKind::Store { dst, src }
        };
        self.push_stmt(origin, kind);
    }
}
