use cranelift_entity::EntityRef;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use rustc_hash::FxHashMap;

use crate::{
    analysis::semantic::instance::{
        CallSiteLowering, ForLoopCallSites, SemanticInstance, resolve_semantic_const_ref,
    },
    analysis::{
        HirAnalysisDb,
        semantic::{
            CallSiteId, FieldIndex, Mutability, SBlock, SBlockId, SConst, SExpr, SLocal, SLocalId,
            SOperand, SPlace, SStmt, SStmtKind, STerminator, STerminatorKind, SValueId,
            SemConstValue, SemOrigin, SemanticBody, SemanticCodeRegionTarget, SemanticLocalRole,
            VariantIndex, bool_const, bytes_const, int_const, reify_runtime_const_for_ty,
            runtime_size_bytes, sem_const_from_ty, unit_const,
        },
        ty::{
            const_ty::{ConstTyData, EvaluatedConstTy, const_ty_or_abstract_from_assoc_const_use},
            normalize::normalize_ty,
            ty_check::{
                BodyOwner, CodeRegionIntrinsicKind, ConstIntrinsicKind, ConstRef, LocalBinding,
                PathReadSemantics, RecordInitLowering, RecordLike, SemanticExprLowering, TypedBody,
                ValuePathRef,
            },
            ty_def::{BorrowKind, TyData, TyId},
        },
    },
    hir_def::{
        ArithBinOp, Body, CallArg, CallableDef, Cond, CondId, Expr, ExprId, Field as HirField,
        LitKind, MatchArm, Partial, PatId, PathId, Stmt, StmtId,
        expr::{BinOp, CompBinOp, LogicalBinOp, UnOp},
    },
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
        let mut push_binding_local = |binding| {
            let role = match binding_role_mode {
                BindingRoleMode::Final => instance.binding_role(db, binding),
                BindingRoleMode::Provisional => instance.provisional_binding_role(db, binding),
            };
            let snapshot_source = initial_snapshot_source(&role);
            locals.push(SLocal {
                ty: match binding_role_mode {
                    BindingRoleMode::Final => instance.binding_ty(db, binding),
                    BindingRoleMode::Provisional => instance.provisional_binding_ty(db, binding),
                },
                mutability: if binding.is_mut() {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                },
                source: Some(binding),
                role,
                snapshot_source,
            });
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
    let result = cx.lower_expr(body.expr(db));
    if !cx.is_terminated(cx.current) {
        let result = SOperand::expr(result, body.expr(db));
        cx.set_terminator(
            cx.current,
            SemOrigin::Body(template_owner),
            if cx.expr_ty(body.expr(db)) == TyId::unit(db) {
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
    pub(super) locals: Vec<SLocal<'db>>,
    pub(super) assigned_snapshots: Vec<bool>,
    pub(super) blocks: Vec<BlockState<'db>>,
    pub(super) binding_locals: FxHashMap<LocalBinding<'db>, SLocalId>,
    pub(super) with_binding_values: FxHashMap<ExprId, SValueId>,
    pub(super) current: SBlockId,
    pub(super) loop_stack: Vec<LoopScope>,
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
            locals: Vec::new(),
            assigned_snapshots: Vec::new(),
            blocks: Vec::new(),
            binding_locals: FxHashMap::default(),
            with_binding_values: FxHashMap::default(),
            current: SBlockId::from_u32(0),
            loop_stack: Vec::new(),
        };
        cx.collect_binding_locals();
        cx.current = cx.new_block();
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
            locals: self.locals,
            blocks,
        }
    }

    fn collect_binding_locals(&mut self) {
        let mut param_idx = 0;
        while let Some(binding) = self.typed_body.param_binding(param_idx) {
            self.alloc_binding_local(binding);
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
                    self.alloc_binding_local(binding);
                }
            }
        }
        for binding in self.owner_effect_bindings() {
            self.alloc_binding_local(binding);
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
        self.assigned_snapshots.push(snapshot_source.is_some());
        self.locals.push(SLocal {
            ty,
            mutability,
            source,
            role,
            snapshot_source,
        });
        id
    }

    fn binding_ty(&self, binding: LocalBinding<'db>) -> TyId<'db> {
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
            self.blocks[self.current.index()]
                .stmts
                .push(SStmt { origin, kind });
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
        let dst = self.alloc_temp(ty);
        self.push_stmt(origin, SStmtKind::Assign { dst, expr });
        dst
    }

    pub(super) fn emit_expr(&mut self, ty: TyId<'db>, expr: SExpr<'db>) -> SValueId {
        self.emit_expr_with_origin(SemOrigin::Synthetic, ty, expr)
    }

    pub(super) fn lower_expr_operand(&mut self, expr: ExprId) -> SOperand {
        SOperand::expr(self.lower_expr(expr), expr)
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
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            panic!("cannot lower absent expression")
        };
        let origin = SemOrigin::Expr(expr);
        let ty = self.expr_ty(expr);

        match expr_data {
            Expr::Lit(lit) => self.lower_leaf_literal(expr, lit),
            Expr::Path(_) => self.lower_path_expr(expr),
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
            Expr::RecordInit(path, fields) => self.lower_record_init(expr, *path, fields),
            Expr::Field(base, _) => {
                if let Some(place) = self.typed_body.expr_place(expr) {
                    let place = self.lower_place_data(place);
                    return self.emit_expr_with_origin(origin, ty, SExpr::ReadPlace { place });
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
                        base: SOperand::expr(base, base_expr),
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
                    return self.emit_expr_with_origin(origin, ty, SExpr::ReadPlace { place });
                }
                let base = self.lower_expr_operand(*base);
                let index = self.lower_expr_operand(*index);
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
                    },
                )
            }
            Expr::Un(inner, op) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*inner), &[]);
                }
                if *op == UnOp::Minus
                    && let Some(value) = self.lower_negated_int_literal(expr, *inner)
                {
                    return value;
                }
                let value = self.lower_expr_operand(*inner);
                self.emit_expr_with_origin(origin, ty, SExpr::Unary { op: *op, value })
            }
            Expr::Bin(lhs, rhs, BinOp::Arith(ArithBinOp::Range)) => {
                let ty = self.expr_ty(expr);
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
                let value = self.lower_expr_operand(*value);
                self.emit_expr_with_origin(
                    origin,
                    ty,
                    SExpr::Cast {
                        value,
                        to: to.to_opt().map_or(ty, |_| ty),
                    },
                )
            }
            Expr::Call(_, args) => self.lower_call(expr, None, args),
            Expr::Assert(args) => self.lower_assert(expr, args),
            Expr::MethodCall(receiver, _, _, args) => self.lower_call(expr, Some(*receiver), args),
            Expr::Assign(dst, src) => {
                let src = self.lower_expr_operand(*src);
                let dst_place = self.lower_place(*dst);
                self.push_place_write(origin, dst_place, src);
                self.unit_value()
            }
            Expr::AugAssign(dst, src, op) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*dst), &[*src]);
                }
                let lhs = self.lower_expr_operand(*dst);
                let rhs = self.lower_expr_operand(*src);
                let dst_ty = self.projectable_place_ty(self.expr_ty(*dst));
                let sum = self.emit_expr_with_origin(
                    origin,
                    dst_ty,
                    SExpr::Binary {
                        op: BinOp::Arith(*op),
                        lhs,
                        rhs,
                    },
                );
                let dst_place = self.lower_place(*dst);
                self.push_place_write(origin, dst_place, SOperand::inherited(sum));
                self.unit_value()
            }
            Expr::Block(stmts) => self.lower_block_expr(stmts),
            Expr::If(cond, then_expr, else_expr) => {
                self.lower_if_expr(*cond, *then_expr, *else_expr)
            }
            Expr::Match(scrutinee, arms) => self.lower_match_expr(expr, *scrutinee, arms),
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
        self.lower_expr_branch(cond_arg.expr, success_bb, failure_bb);

        self.switch_to(success_bb);
        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));

        self.switch_to(failure_bb);
        self.set_terminator(
            self.current,
            SemOrigin::Expr(expr),
            STerminatorKind::Assert { message },
        );

        self.switch_to(join_bb);
        self.unit_value()
    }

    fn lower_leaf_literal(&mut self, expr: ExprId, lit: &LitKind<'db>) -> SValueId {
        let ty = self.expr_ty(expr);
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

    fn lower_negated_int_literal(&mut self, expr: ExprId, inner: ExprId) -> Option<SValueId> {
        let Partial::Present(Expr::Lit(LitKind::Int(int_id))) = inner.data(self.db, self.body)
        else {
            return None;
        };
        let ty = self.expr_ty(expr);
        let value = int_const(self.db, ty, -BigInt::from(int_id.data(self.db).clone()));
        Some(self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::Const(SConst::Value(value)),
        ))
    }

    fn lower_const_ref(&mut self, expr: ExprId, const_ref: ConstRef<'db>) -> SValueId {
        let ty = self.expr_ty(expr);
        let mut type_level_fallback = None;
        if let ConstRef::TraitConst(assoc) = const_ref
            && let Some(const_ty) = const_ty_or_abstract_from_assoc_const_use(self.db, assoc, ty)
                .map(|const_ty| TyId::const_ty(self.db, const_ty))
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

        // The trait const cannot be resolved to a concrete instance in this
        // context (e.g. a trait const on a still-generic `Self` inside a
        // CTFE-evaluated anon const body of a trait method signature). Emit
        // the type-level symbolic value so const evaluation yields the
        // abstract form instead of panicking.
        if let Some(symbolic) = type_level_fallback {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::Const(SConst::Value(symbolic)),
            );
        }

        panic!("const ref should resolve to a semantic instance: {const_ref:?}");
    }

    fn lower_path_expr(&mut self, expr: ExprId) -> SValueId {
        if let Some(binding) = self.typed_body.expr_binding(expr) {
            let local = *self
                .binding_locals
                .get(&binding)
                .expect("binding local should be allocated");
            return match self.binding_path_read_semantics(
                binding,
                self.typed_body
                    .path_expr_read_semantics(expr)
                    .expect("binding path should have typed read semantics"),
                self.expr_ty(expr),
            ) {
                PathReadSemantics::ReuseLocal => local,
                PathReadSemantics::ForwardInterface => self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    self.expr_ty(expr),
                    SExpr::Forward(SOperand::inherited(local)),
                ),
                PathReadSemantics::MaterializeValue => self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    self.expr_ty(expr),
                    SExpr::UseValue(SOperand::inherited(local)),
                ),
            };
        }
        if let Some(const_ref) = self.typed_body.expr_const_ref(expr) {
            return self.lower_const_ref(expr, const_ref);
        }
        if let Some(region) = self.typed_body.expr_code_region_ref(self.db, expr) {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                self.expr_ty(expr),
                SExpr::CodeRegionRef { region },
            );
        }

        match self.typed_body.value_path_ref(expr) {
            Some(ValuePathRef::UnitVariant(variant)) => self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                self.expr_ty(expr),
                SExpr::EnumMake {
                    enum_ty: variant.ty,
                    variant: VariantIndex(variant.variant.idx),
                    fields: Box::new([]),
                },
            ),
            Some(ValuePathRef::TypeConst(ty)) => {
                if let Some(value) = sem_const_from_ty(self.db, ty) {
                    self.emit_expr_with_origin(
                        SemOrigin::Expr(expr),
                        self.expr_ty(expr),
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
                    self.expr_ty(expr),
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
                let ty = self.expr_ty(expr);
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
    ) -> SValueId {
        let arg_exprs = args.iter().map(|arg| arg.expr).collect::<Vec<_>>();
        self.lower_call_like_expr(expr, self.expr_ty(expr), receiver, &arg_exprs)
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
            SemanticExprLowering::Call { callable } => {
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
                self.lower_const_intrinsic(expr, callable, *kind)
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
        let mut values = Vec::with_capacity(args.len() + usize::from(receiver.is_some()));
        if let Some(receiver) = receiver {
            values.push(SOperand::expr(
                self.lower_callable_receiver(expr, receiver),
                receiver,
            ));
        }
        for arg in args {
            values.push(self.lower_expr_operand(*arg));
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
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::Call {
                        call_site: CallSiteId::Expr(expr),
                        callee,
                        args: values.into_boxed_slice(),
                        effect_args,
                    },
                )
            }
        }
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
            self.expr_ty(expr),
            SExpr::Const(SConst::Value(int_const(
                self.db,
                self.expr_ty(expr),
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
                    self.bind_pattern(*pat, value);
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
                let scope = self.loop_stack.last().copied().expect("break outside loop");
                self.set_terminator(self.current, origin, STerminatorKind::Goto(scope.break_bb));
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
        self.lower_cond_branch(cond, body_bb, exit_bb);

        self.loop_stack.push(LoopScope {
            continue_bb: cond_bb,
            break_bb: exit_bb,
        });
        self.switch_to(body_bb);
        let _ = self.lower_expr(body_expr);
        if !self.is_terminated(self.current) {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(cond_bb));
        }
        self.loop_stack.pop();

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
        let len_value = self.emit_expr(
            usize_ty,
            SExpr::Call {
                call_site: CallSiteId::ForLoopLen(stmt),
                callee: for_loop_call_sites
                    .len
                    .callee
                    .expect("Seq::len should lower to a semantic callee"),
                args: vec![iter_operand].into_boxed_slice(),
                effect_args: len_effect_args,
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
        });
        self.switch_to(body_bb);
        let get_effect_args = self.lower_effect_arg_slice(&for_loop_call_sites.get.effect_args);
        let elem = self.emit_expr(
            elem_ty,
            SExpr::Call {
                call_site: CallSiteId::ForLoopGet(stmt),
                callee: for_loop_call_sites
                    .get
                    .callee
                    .expect("Seq::get should lower to a semantic callee"),
                args: vec![iter_operand, SOperand::synthetic(idx_local)].into_boxed_slice(),
                effect_args: get_effect_args,
            },
        );
        self.bind_pattern(pat, elem);
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
    ) -> SValueId {
        let result_ty = self.expr_ty(then_expr);
        let result = self.alloc_temp(result_ty);
        let then_bb = self.new_block();
        let else_bb = self.new_block();
        let join_bb = self.new_block();
        let mut join_reachable = false;

        self.lower_cond_branch(cond, then_bb, else_bb);

        self.switch_to(then_bb);
        let then_value = self.lower_expr(then_expr);
        if !self.is_terminated(self.current) {
            join_reachable = true;
            self.push_synthetic_stmt(SStmtKind::Assign {
                dst: result,
                expr: SExpr::Forward(SOperand::expr(then_value, then_expr)),
            });
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        }

        self.switch_to(else_bb);
        let else_value = if let Some(expr) = else_expr {
            SOperand::expr(self.lower_expr(expr), expr)
        } else {
            SOperand::synthetic(self.unit_value())
        };
        if !self.is_terminated(self.current) {
            join_reachable = true;
            self.push_synthetic_stmt(SStmtKind::Assign {
                dst: result,
                expr: SExpr::Forward(else_value),
            });
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        }

        if !join_reachable {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
        result
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

        self.lower_expr_branch(expr, true_bb, false_bb);

        self.switch_to(true_bb);
        if !self.is_terminated(self.current) {
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
        }

        self.switch_to(false_bb);
        if !self.is_terminated(self.current) {
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
        }

        if !join_reachable {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
        self.switch_to(join_bb);
        result
    }

    fn lower_expr_branch(&mut self, expr: ExprId, then_bb: SBlockId, else_bb: SBlockId) {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            panic!("cannot lower absent condition expression")
        };

        match expr_data {
            Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::And)) => {
                let rhs_bb = self.new_block();
                self.lower_expr_branch(*lhs, rhs_bb, else_bb);
                self.switch_to(rhs_bb);
                self.lower_expr_branch(*rhs, then_bb, else_bb);
            }
            Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::Or)) => {
                let rhs_bb = self.new_block();
                self.lower_expr_branch(*lhs, then_bb, rhs_bb);
                self.switch_to(rhs_bb);
                self.lower_expr_branch(*rhs, then_bb, else_bb);
            }
            _ => {
                let cond = self.lower_expr(expr);
                self.set_synthetic_terminator(
                    self.current,
                    STerminatorKind::Branch {
                        cond: SOperand::expr(cond, expr),
                        then_bb,
                        else_bb,
                    },
                );
            }
        }
    }

    fn lower_match_expr(
        &mut self,
        expr: ExprId,
        scrutinee: ExprId,
        arms: &Partial<Vec<MatchArm>>,
    ) -> SValueId {
        let Partial::Present(arms) = arms else {
            panic!("match arms missing")
        };
        let value = self.lower_expr(scrutinee);
        let result = self.alloc_temp(self.expr_ty(expr));
        let join_bb = self.new_block();
        self.lower_match_expr_with_decision_tree(value, result, join_bb, arms)
    }

    fn lower_cond_branch(&mut self, cond: CondId, then_bb: SBlockId, else_bb: SBlockId) {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            panic!("cannot lower absent condition")
        };

        match cond_data {
            Cond::Expr(expr) => {
                let cond = self.lower_expr(*expr);
                self.set_synthetic_terminator(
                    self.current,
                    STerminatorKind::Branch {
                        cond: SOperand::expr(cond, *expr),
                        then_bb,
                        else_bb,
                    },
                );
            }
            Cond::Bin(lhs, rhs, LogicalBinOp::And) => {
                let rhs_bb = self.new_block();
                self.lower_cond_branch(*lhs, rhs_bb, else_bb);
                self.switch_to(rhs_bb);
                self.lower_cond_branch(*rhs, then_bb, else_bb);
            }
            Cond::Bin(lhs, rhs, LogicalBinOp::Or) => {
                let rhs_bb = self.new_block();
                self.lower_cond_branch(*lhs, then_bb, rhs_bb);
                self.switch_to(rhs_bb);
                self.lower_cond_branch(*rhs, then_bb, else_bb);
            }
            Cond::Let(pat, expr) => {
                let value = self.lower_expr(*expr);
                if self.pattern_is_irrefutable(*pat) {
                    self.bind_pattern(*pat, value);
                    self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
                } else {
                    self.lower_pattern_branch(*pat, value, then_bb, else_bb);
                    self.switch_to(then_bb);
                    self.bind_pattern(*pat, value);
                }
            }
        }
    }

    fn place_needs_indirect_store(&self, place: &SPlace<'db>) -> bool {
        let Some(local) = self.locals.get(place.local.index()) else {
            return false;
        };
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

    fn push_place_write(&mut self, origin: SemOrigin<'db>, dst: SPlace<'db>, src: SOperand) {
        let kind = if self.place_can_assign_directly(&dst) {
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
