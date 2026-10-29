use cranelift_entity::EntityRef;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use rustc_hash::FxHashMap;

use crate::{
    analysis::semantic::instance::{
        CallSiteLowering, ForLoopCallSites, SemanticInstance, provisional_semantic_callee_key,
        resolve_semantic_const_ref, semantic_callee_key_with_effect_providers,
    },
    analysis::{
        HirAnalysisDb,
        semantic::{
            BinaryOp, CallSiteId, FieldIndex, LayoutBackingSource, Mutability, SBlock, SBlockId,
            SConst, SExpr, SLocal, SLocalId, SOperand, SPlace, SStmt, SStmtId, SStmtKind,
            STerminator, STerminatorKind, SValueId, SemConstId, SemConstValue, SemOrigin,
            SemanticBody, SemanticCodeRegionTarget, SemanticLocalRole, VariantIndex, bool_const,
            bytes_const, consts::instantiate_const_template, int_const, reify_runtime_const_for_ty,
            runtime_size_bytes, sem_const_from_ty, struct_const, unit_const,
        },
        ty::{
            const_expr::{ConstExpr, ConstExprId, ConstInvocation},
            const_ty::{
                ConstTyData, ConstTyId, const_ty_or_abstract_from_assoc_const_use,
                const_ty_or_abstract_from_inherent_const_use,
            },
            normalize::normalize_ty,
            shape::{Shape, SumKind, sum_kind},
            ty_check::{
                BindingAccess, BodyOwner, BuiltinOp, CallParam, Callable, CodeRegionIntrinsicKind,
                ConstIntrinsicKind, ConstRef, ForLoopStep, LocalBinding, PathReadSemantics,
                RecordInitLowering, RecordLike, SemanticExprLowering, TypedBody, ValuePathRef,
                constructed_variant, infer_body, transparent_newtype_field_ty,
            },
            ty_def::{BorrowKind, TyData, TyId},
            ty_is_copy, ty_is_snapshot,
        },
    },
    core::semantic::storage_slot_span,
    hir_def::{
        ArithBinOp, Body, CallArg, CallableDef, Cond, CondId, Expr, ExprId, Field as HirField,
        LitKind, MatchArm, Partial, Pat, PatId, PathId, Stmt, StmtId,
        expr::{BinOp, LogicalBinOp, UnOp},
        params::FuncParamMode,
    },
};

use super::{
    drops::DropScope,
    effects::{WithBindingSource, provisional_owner_effect_bindings},
    elaborate::elaborate_ends,
    local_facts::{initial_snapshot_source, ordinary_direct_value_role},
    pattern::PatternSource,
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
                layout_backing_sources,
            });
            entry_locals.push(local);
        };
        for &binding in typed_body.input_bindings() {
            push_binding_local(binding);
        }
        for binding in
            owner_effect_bindings_for_mode(db, instance, template_owner, binding_role_mode)
        {
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
            unsafe_splits: Vec::new(),
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
    let root = template_owner
        .root_expr(db)
        .unwrap_or_else(|| body.expr(db));
    cx.push_drop_scope(true);
    cx.hold_entry();
    let result = cx.lower_expr(root);
    if !cx.is_terminated(cx.current) {
        let result = (cx.expr_ty(root) != TyId::unit(db)).then(|| SOperand::expr(result, root));
        cx.exit(SemOrigin::Body(template_owner), result);
    }
    let mut body = cx.finish();
    elaborate_ends(db, &mut body);
    body
}

/// The effect bindings of an instance's entry: its owner's declared effects
/// and the components of the rows they name.
fn owner_effect_bindings_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    owner: BodyOwner<'db>,
    binding_role_mode: BindingRoleMode,
) -> Vec<LocalBinding<'db>> {
    match binding_role_mode {
        BindingRoleMode::Final => instance.effect_bindings(db),
        BindingRoleMode::Provisional => {
            let mut bindings = provisional_owner_effect_bindings(db, owner);
            bindings.extend(instance.row_component_bindings(db));
            bindings
        }
    }
}

/// What a `for` loop holds of a base (`lower_loop_base`).
enum LoopBase<'db> {
    Value(SValueId),
    Place(SPlace<'db>, TyId<'db>),
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
    pub(super) assigned_layout_backing_sources: Vec<bool>,
    pub(super) blocks: Vec<BlockState<'db>>,
    pub(super) binding_locals: FxHashMap<LocalBinding<'db>, SLocalId>,
    /// In a closure body, the field of its environment each captured
    /// binding is read from.
    pub(super) capture_places: FxHashMap<LocalBinding<'db>, (SPlace<'db>, TyId<'db>)>,
    pub(super) with_binding_sources: FxHashMap<ExprId, WithBindingSource<'db>>,
    pub(super) current: SBlockId,
    pub(super) next_stmt_id: u32,
    pub(super) loop_stack: Vec<LoopScope>,
    /// Loop bases, evaluated once before the method chain that drives them.
    loop_bases: FxHashMap<ExprId, SValueId>,
    /// Receivers of projection calls in an assignment target or a receiver,
    /// prepared before the value or the arguments (`prepare_place_root`).
    prepared_receivers: FxHashMap<ExprId, Receiver<'db>>,
    /// How many yield sites enclose the expression being lowered.
    pub(super) yield_depth: u32,
    /// The slide of the `yield` statement being lowered.
    slide: Option<SBlockId>,
    /// The components of each unsafe split lowered so far.
    unsafe_splits: Vec<Vec<SValueId>>,
    /// The scopes enclosing the code being lowered, innermost last, with the
    /// owners each destroys where control leaves it (`drops`).
    pub(super) drop_scopes: Vec<DropScope>,
    /// The scope each temporary was allocated in, by local.
    pub(super) temp_scopes: Vec<Option<u32>>,
    pub(super) next_drop_scope: u32,
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

/// A call's receiver between the resolution of its root, before the call's
/// arguments, and the opening of its accesses, after them.
pub(super) enum Receiver<'db> {
    /// A temporary, a loop base or an access carrier, evaluated already.
    Value(SValueId),
    /// A receiver opened after the arguments: a `mut` receiver's place in a
    /// temporary, or `None` for a place rooted at a binding, lowered whole
    /// then.
    Place(Option<SPlace<'db>>),
    /// A projection call in the receiver chain, by its own receiver: its
    /// session opens after the arguments of the call it is the receiver of.
    Chain(ExprId, Box<Receiver<'db>>),
}

#[derive(Clone, Copy)]
pub(super) struct LoopScope {
    pub(super) continue_bb: SBlockId,
    pub(super) break_bb: SBlockId,
    pub(super) has_reachable_continue: bool,
    /// The scopes outside the loop body: `break` and `continue` destroy the
    /// owners of the scopes inside it.
    pub(super) drop_depth: usize,
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
        const_ty.integer_value(self.db)?.to_usize()
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
            assigned_layout_backing_sources: Vec::new(),
            blocks: Vec::new(),
            binding_locals: FxHashMap::default(),
            capture_places: FxHashMap::default(),
            with_binding_sources: FxHashMap::default(),
            current: SBlockId::from_u32(0),
            next_stmt_id: 0,
            loop_stack: Vec::new(),
            loop_bases: FxHashMap::default(),
            prepared_receivers: FxHashMap::default(),
            yield_depth: 0,
            slide: None,
            unsafe_splits: Vec::new(),
            drop_scopes: Vec::new(),
            temp_scopes: Vec::new(),
            next_drop_scope: 0,
        };
        cx.collect_binding_locals();
        cx.current = cx.new_block();
        cx.bind_param_products();
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
            unsafe_splits: self.unsafe_splits,
        }
    }

    fn collect_binding_locals(&mut self) {
        for &binding in self.typed_body.input_bindings() {
            self.alloc_entry_binding_local(binding);
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
        if let BodyOwner::Closure { def, .. } = self.template_owner
            && let Some(env) = self.typed_body.param_binding(0)
            && let Some(info) = self.typed_body.closure_info(def.expr)
        {
            let env = self.binding_locals[&env];
            for (idx, capture) in info.captures.iter().enumerate() {
                let mut place = SPlace::new(env);
                place.push_field(FieldIndex(idx as u16));
                self.capture_places
                    .insert(capture.binding, (place, capture.ty));
            }
        }

        for (pat, _) in self.body.pats(self.db).iter() {
            if let Some(binding) = self.typed_body.pat_binding(pat) {
                self.alloc_binding_local(binding);
            }
        }
    }

    /// Gathers each product parameter's parts, the inputs a call passes, into
    /// the product its body destructures.
    fn bind_param_products(&mut self) {
        for binding in (0..).map_while(|idx| self.typed_body.param_binding(idx)) {
            let Some(product) = self.typed_body.param_product(binding).cloned() else {
                continue;
            };
            let LocalBinding::Param { site, idx, .. } = binding else {
                unreachable!("a product is a parameter's")
            };
            let parts: Vec<_> = self
                .typed_body
                .input_bindings()
                .iter()
                .filter(|input| {
                    matches!(input, LocalBinding::ParamPart { site: part_site, idx: param, .. }
                        if *part_site == site && *param == idx)
                })
                .map(|input| self.binding_locals[input])
                .collect();
            // An opaque parameter's binding is its one input, passed on
            // whole, as is a known one of a single part.
            let expr = match (&product, parts.as_slice()) {
                (Shape::Tuple(_), _) => self.gather_product(&product, &mut parts.into_iter()),
                (_, &[part]) => SExpr::Forward(SOperand::synthetic(part)),
                _ => unreachable!("a parameter's inputs are its shape's parts"),
            };
            let local = self.alloc_binding_local(binding);
            self.push_synthetic_stmt(SStmtKind::Assign { dst: local, expr });
        }
    }

    /// The product `shape` of `parts`, its leaves' carriers in order.
    fn gather_product(
        &mut self,
        shape: &Shape<'db>,
        parts: &mut impl Iterator<Item = SLocalId>,
    ) -> SExpr<'db> {
        let Shape::Tuple(elems) = shape else {
            unreachable!("a product is a tuple of parts")
        };
        let mut fields = Vec::with_capacity(elems.len());
        for elem in elems {
            let value = if matches!(elem, Shape::Tuple(_)) {
                let expr = self.gather_product(elem, parts);
                self.emit_expr(elem.carrier_ty(self.db), expr)
            } else {
                parts.next().expect("an input for each part")
            };
            fields.push(SOperand::synthetic(value));
        }
        let ty = shape.carrier_ty(self.db);
        SExpr::AggregateMake {
            ty,
            fields: fields.into(),
        }
    }

    pub(super) fn alloc_binding_local(&mut self, binding: LocalBinding<'db>) -> SLocalId {
        if let Some(&local) = self.binding_locals.get(&binding) {
            return local;
        }
        // `mut` on an access binding names its access kind: the binding
        // itself is never reassigned.
        let local = self.alloc_local(
            self.binding_ty(binding),
            if binding.is_mut() && self.typed_body.binding_access(binding).is_none() {
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

    pub(super) fn alloc_local(
        &mut self,
        ty: TyId<'db>,
        mutability: Mutability,
        source: Option<LocalBinding<'db>>,
    ) -> SLocalId {
        let id = SLocalId::from_u32(self.locals.len() as u32);
        // Later stages read local types as the instance's: `B::Item` is the
        // element type it selects.
        let ty = normalize_ty(self.db, ty, self.body.scope(), self.assumptions);
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
        self.assigned_layout_backing_sources
            .push(!layout_backing_sources.is_empty());
        let scope = source.map_or_else(|| self.allocation_scope(), |_| None);
        self.temp_scopes.push(scope);
        self.locals.push(SLocal {
            ty,
            mutability,
            source,
            role,
            snapshot_source,
            layout_backing_sources,
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
        owner_effect_bindings_for_mode(
            self.db,
            self.instance,
            self.template_owner,
            self.binding_role_mode,
        )
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
            self.hold_assigned(&kind);
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
        let dst = self.alloc_temp(ty);
        self.push_stmt(origin, SStmtKind::Assign { dst, expr });
        dst
    }

    pub(super) fn emit_expr(&mut self, ty: TyId<'db>, expr: SExpr<'db>) -> SValueId {
        self.emit_expr_with_origin(SemOrigin::Synthetic, ty, expr)
    }

    /// The field type of `ty` when it is a transparent newtype.
    fn newtype_field(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        let scope = self.body.scope();
        transparent_newtype_field_ty(
            self.db,
            scope,
            normalize_ty(self.db, ty, scope, self.assumptions),
        )
    }

    /// A cast of the value of `value`, of type `from`, to `to`. One that
    /// unwraps a newtype projects its operand's place, as a field access
    /// does: it reads the leaf it reaches, not the whole value.
    fn lower_cast_expr(
        &mut self,
        origin: SemOrigin<'db>,
        value: ExprId,
        from: TyId<'db>,
        to: TyId<'db>,
    ) -> SValueId {
        if self.newtype_field(from).is_some()
            && let Some(mut place) = self.try_lower_place(value)
        {
            let mut leaf = from;
            while let Some(field_ty) = self.newtype_field(leaf) {
                place.push_field(FieldIndex(0));
                leaf = field_ty;
            }
            let read = self.emit_expr_with_origin(origin, leaf, SExpr::ReadPlace { place });
            return self.lower_cast(origin, SOperand::expr(read, value), leaf, to);
        }
        let operand = self.lower_expr_operand(value);
        self.lower_cast(origin, operand, from, to)
    }

    /// A cast sees through transparent newtypes (`check_cast`): it unwraps
    /// the source's, converts the leaf and wraps the result in the target's.
    fn lower_cast(
        &mut self,
        origin: SemOrigin<'db>,
        mut value: SOperand,
        from: TyId<'db>,
        to: TyId<'db>,
    ) -> SValueId {
        let mut layers = Vec::new();
        let mut leaf = to;
        while let Some(field_ty) = self.newtype_field(leaf) {
            layers.push(leaf);
            leaf = field_ty;
        }
        let mut from = from;
        let mut unwrapped = false;
        while let Some(field_ty) = self.newtype_field(from) {
            let field = SExpr::Field {
                base: value,
                field: FieldIndex(0),
            };
            value = SOperand::synthetic(self.emit_expr_with_origin(origin, field_ty, field));
            from = field_ty;
            unwrapped = true;
        }
        let mut result = if from == leaf && (unwrapped || !layers.is_empty()) {
            value
        } else {
            let cast = SExpr::Cast { value, to: leaf };
            SOperand::synthetic(self.emit_expr_with_origin(origin, leaf, cast))
        };
        for ty in layers.into_iter().rev() {
            let fields = Box::new([result]);
            result = SOperand::synthetic(self.emit_expr_with_origin(
                origin,
                ty,
                SExpr::AggregateMake { ty, fields },
            ));
        }
        result.value
    }

    pub(super) fn lower_expr_operand(&mut self, expr: ExprId) -> SOperand {
        SOperand::expr(self.lower_expr(expr), expr)
    }

    /// Lowers an operand that other operands follow (`take_operand`).
    fn take_expr_operand(&mut self, expr: ExprId) -> SOperand {
        let value = self.lower_expr(expr);
        SOperand::expr(self.take_operand(expr, value), expr)
    }

    /// Lowers operands evaluated left to right, taking each but the last.
    fn lower_expr_operands(&mut self, exprs: &[ExprId]) -> Vec<SOperand> {
        let Some((&last, init)) = exprs.split_last() else {
            return Vec::new();
        };
        let mut operands: Vec<_> = init
            .iter()
            .map(|&expr| self.take_expr_operand(expr))
            .collect();
        operands.push(self.lower_expr_operand(last));
        operands
    }

    /// Takes a variable's value where it appears as an operand, not where
    /// the operation consuming it runs, after the operands that follow it,
    /// which may write or move it. An immutable `Copy` variable reads the
    /// same anywhere.
    pub(super) fn take_operand(&mut self, expr: ExprId, value: SValueId) -> SValueId {
        let local = &self.locals[value.index()];
        if local.source.is_none()
            || !matches!(
                local.role,
                SemanticLocalRole::DirectValue { .. } | SemanticLocalRole::PlaceBoundValue { .. }
            )
            || local.mutability == Mutability::Immutable
                && ty_is_copy(self.db, self.body.scope(), local.ty, self.assumptions)
        {
            return value;
        }
        let ty = local.ty;
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::UseValue(SOperand::expr(value, expr)),
        )
    }

    pub(super) fn push_synthetic_stmt(&mut self, kind: SStmtKind<'db>) {
        self.push_stmt(SemOrigin::Synthetic, kind);
    }

    /// Ends the body on the current path with `value`. A projection yields
    /// it instead: its session resumes in the slide of the `yield` statement
    /// being lowered, or else returns.
    /// Every scope ends on the path, and the value leaves with it. A
    /// projection's frame outlives its yield: its scopes end where its
    /// session resumes and returns.
    fn exit(&mut self, origin: SemOrigin<'db>, value: Option<SOperand>) {
        let Some(value) = value.filter(|_| self.instance.is_projection(self.db)) else {
            self.destroy_scopes_from(0, value.map(|value| value.value));
            self.set_terminator(self.current, origin, STerminatorKind::Return(value));
            return;
        };
        let resume = self.new_block();
        self.set_terminator(
            self.current,
            origin,
            STerminatorKind::Yield { value, resume },
        );
        self.current = resume;
        match self.slide {
            Some(slide) => self.set_terminator(resume, origin, STerminatorKind::Goto(slide)),
            None => {
                self.destroy_scopes_from(0, None);
                self.set_terminator(resume, origin, STerminatorKind::Return(None));
            }
        }
    }

    pub(super) fn set_synthetic_terminator(&mut self, block: SBlockId, kind: STerminatorKind<'db>) {
        self.set_terminator(block, SemOrigin::Synthetic, kind);
    }

    /// The semantic-IR type of `expr`'s value: a projection's yield sites
    /// carry their grants. A leaf yielding an access to a value is lowered
    /// as that value, of its own type: the grant of the frame temporary
    /// holding it is typed by the shape (`lower_yield_leaf`). A call that
    /// never returns yields nothing, and joins the other leaves' grants.
    pub(super) fn expr_ty(&self, expr: ExprId) -> TyId<'db> {
        match self.typed_body.yield_shape(expr) {
            Some(Shape::Access(..)) if self.is_yield_leaf(expr) && !self.never_returns(expr) => {
                self.typed_body.expr_ty(self.db, expr)
            }
            Some(shape) => shape.carrier_ty(self.db),
            None => self.typed_body.expr_ty(self.db, expr),
        }
    }

    /// Whether `expr` calls something that never returns.
    fn never_returns(&self, expr: ExprId) -> bool {
        self.typed_body
            .callable_expr(expr)
            .is_some_and(|callable| callable.ret_ty(self.db).is_never(self.db))
    }

    /// Whether the yield site `expr` yields itself, rather than through the
    /// branches or the tail of the block it is.
    fn is_yield_leaf(&self, expr: ExprId) -> bool {
        !matches!(
            expr.data(self.db, self.body),
            Partial::Present(Expr::Block(..) | Expr::If(..) | Expr::Match(..) | Expr::With(..))
        )
    }

    pub(super) fn unit_value(&mut self) -> SValueId {
        self.emit_expr(
            TyId::unit(self.db),
            SExpr::Const(SConst::from_trusted_source(self.db, unit_const(self.db))),
        )
    }

    /// Lowers what a pattern destructures: a place its `ref` and `mut`
    /// bindings match is bound by component, and an access is destructured
    /// through its carrier, so the pattern reads only the parts it binds.
    fn lower_scrutinee(&mut self, expr: ExprId) -> PatternSource<'db> {
        if self.typed_body.is_matched_place(expr) {
            return PatternSource::Place {
                place: self.lower_place(expr),
                ty: self.expr_ty(expr),
                provider: self.typed_body.expr_prop(self.db, expr).borrow_provider,
                inspected: self.typed_body.is_inspected_place(expr),
            };
        }
        if matches!(
            expr.data(self.db, self.body),
            Partial::Present(Expr::Path(_))
        ) && let Some(binding) = self.typed_body.expr_binding(expr)
            && !self.capture_places.contains_key(&binding)
            && let Some(&local) = self.binding_locals.get(&binding)
        {
            let ty = self.locals[local.index()].ty;
            if ty.as_capability(self.db).is_some() {
                return self.value_pattern_source(local);
            }
            // A product parameter's binding holds its parts as an `own`
            // binding holds its value, so it is matched in place.
            if self.typed_body.param_product(binding).is_some() {
                return PatternSource::Place {
                    place: SPlace::new(local),
                    ty,
                    provider: None,
                    inspected: true,
                };
            }
        }
        let value = self.lower_source(expr);
        // A value that owns something to destroy is matched in place, so it
        // has one owner: the tests read it and the bindings move its parts
        // out. A value has no `ref` or `mut` binding to borrow it.
        let ty = self.locals[value.index()].ty;
        if self.needs_destroying(ty) {
            let local = self.alloc_temp(ty);
            self.push_stmt(
                SemOrigin::Expr(expr),
                SStmtKind::Assign {
                    dst: local,
                    expr: SExpr::UseValue(SOperand::expr(value, expr)),
                },
            );
            return PatternSource::Place {
                place: SPlace::new(local),
                ty,
                provider: None,
                inspected: true,
            };
        }
        self.value_pattern_source(value)
    }

    /// Lowers what a binding, scrutinee or argument receives from `expr`: the
    /// carrier of the accesses it grants, or its value.
    pub(super) fn lower_source(&mut self, expr: ExprId) -> SValueId {
        if self.typed_body.expr_prop(self.db, expr).shape.is_some() {
            self.lower_access(expr)
        } else {
            self.lower_expr(expr)
        }
    }

    /// Lowers `ref p`, `mut p` or a projection call to the carrier of the
    /// accesses it grants.
    pub(super) fn lower_access(&mut self, expr: ExprId) -> SValueId {
        let prop = self.typed_body.expr_prop(self.db, expr);
        let shape = prop
            .shape
            .unwrap_or_else(|| panic!("access expression without a shape: {expr:?}"));
        let ty = shape.carrier_ty(self.db);
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(_, args)) => {
                let args = args.iter().map(|arg| arg.expr).collect::<Vec<_>>();
                let receiver = self.typed_body.call_receiver(self.db, expr);
                self.lower_call_like_expr(expr, ty, receiver, &args)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, args)) => {
                let args = args.iter().map(|arg| arg.expr).collect::<Vec<_>>();
                self.lower_call_like_expr(expr, ty, Some(*receiver), &args)
            }
            // An `Index` call; a built-in index names a place, as below.
            Partial::Present(Expr::Bin(base, index, BinOp::Index))
                if self.typed_body.semantic_expr_lowering(expr).is_some() =>
            {
                self.lower_call_like_expr(expr, ty, Some(*base), &[*index])
            }
            Partial::Present(Expr::Try(inner)) => self.lower_try(expr, *inner, ty),
            // An unsafe split: one session granting each component.
            Partial::Present(Expr::Tuple(elems)) if self.typed_body.is_unsafe_split(expr) => {
                let fields: Vec<_> = elems
                    .iter()
                    .map(|elem| SOperand::expr(self.lower_source(*elem), *elem))
                    .collect();
                self.unsafe_splits
                    .push(fields.iter().map(|field| field.value).collect());
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::AggregateMake {
                        ty,
                        fields: fields.into(),
                    },
                )
            }
            // The block's bindings belong to the enclosing scope: the access
            // it evaluates to may view them, so they live as long as it may.
            Partial::Present(Expr::Block(stmts, _)) => {
                let (tail, head) = stmts.split_last().expect("an access block has a tail");
                for stmt in head {
                    self.lower_stmt(*stmt);
                }
                let Partial::Present(Stmt::Expr(tail)) = tail.data(self.db, self.body) else {
                    panic!("an access block ends in an expression")
                };
                self.lower_access(*tail)
            }
            // `mut d.at()` with an opaque result is the call at that mode.
            Partial::Present(Expr::Un(inner, UnOp::Mut | UnOp::MaybeMut | UnOp::Ref))
                if matches!(shape, Shape::Opaque(_)) =>
            {
                self.lower_access(*inner)
            }
            // A transport binding of an opaque shape is passed on as it is,
            // moved: it is used at most once.
            Partial::Present(Expr::Path(_))
                if matches!(shape, Shape::Opaque(_))
                    && let Some(binding) = self.typed_body.expr_binding(expr) =>
            {
                let value = self.binding_locals[&binding];
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::UseValue(SOperand::expr(value, expr)),
                )
            }
            // `ref p`, `mut p`, or a place matched through the `mut` access
            // its `mut` pattern bindings open.
            data => {
                let Shape::Access(kind, _) = shape else {
                    panic!("unexpected access expression: {expr:?}")
                };
                let place = match data {
                    Partial::Present(Expr::Un(inner, UnOp::Mut | UnOp::MaybeMut | UnOp::Ref)) => {
                        *inner
                    }
                    _ => expr,
                };
                let place = self.lower_place(place);
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::Borrow {
                        place,
                        kind,
                        provider: prop.borrow_provider,
                    },
                )
            }
        }
    }

    /// Lowers `inner?`: the payload of a `Some` or `Ok` (a payload grant when
    /// `inner` is a sum shape), or else an exit with the empty variant,
    /// carrying an `Err`'s error.
    fn lower_try(&mut self, expr: ExprId, inner: ExprId, ty: TyId<'db>) -> SValueId {
        let origin = SemOrigin::Expr(expr);
        let value = self.lower_source(inner);
        let variant = self.exit_on_empty(origin, value);
        self.emit_expr_with_origin(
            origin,
            ty,
            SExpr::ExtractEnumField {
                value: SOperand::synthetic(value),
                variant,
                field: FieldIndex(0),
            },
        )
    }

    /// Tests the `Option` or `Result` `value`: on its empty variant, exits
    /// with that variant, carrying an `Err`'s error; on its payload variant,
    /// which it returns, continues.
    fn exit_on_empty(&mut self, origin: SemOrigin<'db>, value: SValueId) -> VariantIndex {
        let sum_ty = self.locals[value.index()].ty;
        let kind = sum_kind(self.db, self.body.scope(), sum_ty)
            .expect("`?` and a sum shape's yield test an `Option` or a `Result`");
        let success = kind.payload_variant();
        let failure = 1 - success;
        let is_success = self.emit_expr_with_origin(
            origin,
            TyId::bool(self.db),
            SExpr::IsEnumVariant {
                value: SOperand::synthetic(value),
                variant: VariantIndex(success),
            },
        );
        let success_bb = self.new_block();
        let failure_bb = self.new_block();
        self.set_terminator(
            self.current,
            origin,
            STerminatorKind::Branch {
                cond: SOperand::synthetic(is_success),
                then_bb: success_bb,
                else_bb: failure_bb,
            },
        );

        self.switch_to(failure_bb);
        // The failure path's temporaries exist only on it, which leaves
        // every scope.
        self.push_drop_scope(false);
        let error = (kind == SumKind::Result).then(|| {
            SOperand::synthetic(self.emit_expr_with_origin(
                origin,
                sum_ty.generic_args(self.db)[0],
                SExpr::ExtractEnumField {
                    value: SOperand::synthetic(value),
                    variant: VariantIndex(failure),
                    field: FieldIndex(0),
                },
            ))
        });
        let exit_ty = match self.template_owner {
            BodyOwner::Func(func, mode) if let Some(shape) = func.return_shape(self.db, mode) => {
                shape.carrier_ty(self.db)
            }
            _ => self.typed_body.result_ty(),
        };
        let exit = self.emit_expr_with_origin(
            origin,
            exit_ty,
            SExpr::EnumMake {
                enum_ty: exit_ty,
                variant: VariantIndex(failure),
                fields: error.into_iter().collect(),
            },
        );
        self.exit_empty(origin, SOperand::synthetic(exit));
        self.take_drop_scope();

        self.switch_to(success_bb);
        VariantIndex(success)
    }

    /// Exits with `value`, the empty variant of an `Option` or a `Result`. It
    /// grants nothing, so no slide follows it.
    fn exit_empty(&mut self, origin: SemOrigin<'db>, value: SOperand) {
        let slide = self.slide.take();
        self.exit(origin, Some(value));
        self.slide = slide;
    }

    /// Lowers a projection's yield site to the carrier of what it grants.
    /// Tuples and variant constructors yield through their (yield-site)
    /// elements; an access binding is re-yielded; a `ref` yield of any
    /// other place views it, and a value is held in a frame temporary that
    /// the yield grants.
    fn lower_yield_leaf(&mut self, expr: ExprId, shape: Shape<'db>) -> SValueId {
        if self.typed_body.expr_prop(self.db, expr).shape.is_some() {
            return self.lower_access(expr);
        }
        if self.never_returns(expr) {
            return self.lower_expr_inner(expr);
        }
        let Shape::Access(kind, _) = shape else {
            return self.lower_expr_inner(expr);
        };
        let origin = SemOrigin::Expr(expr);
        // An access binding of the kind the shape names is re-yielded through
        // its carrier. A `mut` one where the shape names `ref`, and a view
        // parameter, are borrowed below like any place: a view's carrier is
        // not the grant the shape names, and a snapshot one is a
        // session-owned copy.
        if let Some(binding) = self.typed_body.expr_binding(expr)
            && !self.capture_places.contains_key(&binding)
            && let Some(&local) = self.binding_locals.get(&binding)
            && self.locals[local.index()]
                .ty
                .as_borrow(self.db)
                .is_some_and(|(carrier, _)| carrier == kind)
        {
            let ty = self.locals[local.index()].ty;
            return self.emit_expr_with_origin(
                origin,
                ty,
                SExpr::Forward(SOperand::expr(local, expr)),
            );
        }
        let place = self.try_lower_place(expr).unwrap_or_else(|| {
            let value = self.lower_expr_inner(expr);
            let mutability = match kind {
                BorrowKind::Mut => Mutability::Mutable,
                BorrowKind::Ref => Mutability::Immutable,
            };
            let temp = self.alloc_local(self.typed_body.expr_ty(self.db, expr), mutability, None);
            self.push_stmt(
                origin,
                SStmtKind::Assign {
                    dst: temp,
                    expr: SExpr::UseValue(SOperand::expr(value, expr)),
                },
            );
            SPlace::new(temp)
        });
        self.emit_expr_with_origin(
            origin,
            shape.carrier_ty(self.db),
            SExpr::Borrow {
                place,
                kind,
                provider: self.typed_body.expr_prop(self.db, expr).borrow_provider,
            },
        )
    }

    /// Lowers `expr`. A projection's yield site yields on its own path, so
    /// each path's grant has its own resume point.
    pub(super) fn lower_expr(&mut self, expr: ExprId) -> SValueId {
        let yield_leaf = self
            .typed_body
            .yield_shape(expr)
            .filter(|_| self.is_yield_leaf(expr));
        let value = match yield_leaf {
            Some(shape) => {
                self.yield_depth += 1;
                let value = self.lower_yield_leaf(expr, shape.clone());
                self.yield_depth -= 1;
                value
            }
            None => self.lower_expr_inner(expr),
        };
        if self.expr_ty(expr).is_never(self.db) && !self.is_terminated(self.current) {
            self.set_synthetic_terminator(self.current, STerminatorKind::Assert { message: None });
        }
        if let Some(shape) = yield_leaf
            && self.yield_depth == 0
            && !self.is_terminated(self.current)
        {
            let origin = SemOrigin::Expr(expr);
            let operand = SOperand::expr(value, expr);
            match shape {
                // A sum shape's empty variant skips the `yield` statement's
                // slide: a variant constructor builds it or not, and any other
                // site is tested, as `?` tests its operand.
                Shape::Sum { variant, .. } if self.slide.is_some() => {
                    match constructed_variant(
                        self.typed_body.value_path_ref(expr),
                        self.typed_body.callable_expr(expr),
                    ) {
                        Some(built) if built != *variant => self.exit_empty(origin, operand),
                        Some(_) => self.exit(origin, Some(operand)),
                        None => {
                            self.exit_on_empty(origin, value);
                            self.exit(origin, Some(operand));
                        }
                    }
                }
                _ => self.exit(origin, Some(operand)),
            }
        }
        value
    }

    fn lower_expr_inner(&mut self, expr: ExprId) -> SValueId {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            panic!("cannot lower absent expression")
        };
        let origin = SemOrigin::Expr(expr);
        let ty = self.expr_ty(expr);

        // A projection used as a value is read through its grant.
        if !matches!(expr_data, Expr::Un(..))
            && self.typed_body.yield_shape(expr).is_none()
            && self.typed_body.expr_prop(self.db, expr).shape.is_some()
        {
            let carrier = self.lower_access(expr);
            return self.emit_expr_with_origin(
                origin,
                ty,
                SExpr::UseValue(SOperand::expr(carrier, expr)),
            );
        }

        match expr_data {
            Expr::Lit(lit) => self.lower_leaf_literal(expr, lit),
            Expr::Path(_) => self.lower_path_expr(expr),
            Expr::Try(inner) => self.lower_try(expr, *inner, ty),
            Expr::Tuple(elems) | Expr::Array(elems) => {
                let fields = self.lower_expr_operands(elems).into_boxed_slice();
                self.emit_expr_with_origin(origin, ty, SExpr::AggregateMake { ty, fields })
            }
            // A closure is the aggregate of its captures, each copied or
            // moved in.
            Expr::Closure { .. } => {
                let captures = self
                    .typed_body
                    .closure_info(expr)
                    .expect("a typed closure has its captures")
                    .captures
                    .clone();
                let fields = captures
                    .into_iter()
                    .map(|capture| {
                        SOperand::expr(
                            self.lower_binding_read(expr, capture.binding, capture.ty, None),
                            expr,
                        )
                    })
                    .collect();
                self.emit_expr_with_origin(origin, ty, SExpr::AggregateMake { ty, fields })
            }
            Expr::ArrayRep(elem, _) => {
                let value = self.lower_expr_operand(*elem);
                self.emit_expr_with_origin(origin, ty, SExpr::ArrayRepeat { ty, value })
            }
            Expr::RecordInit(path, fields) => self.lower_record_init(expr, *path, fields),
            Expr::Field(base, _) => {
                if let Some(place) = self.try_lower_place(expr) {
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
                // An entry lives at its collection's place: a storage
                // collection is never a temporary.
                if self.typed_body.is_place_entry(expr) {
                    let place = self.lower_place(expr);
                    return self.emit_expr_with_origin(origin, ty, SExpr::ReadPlace { place });
                }
                if let Some(place) = self.try_lower_place(expr) {
                    return self.emit_expr_with_origin(origin, ty, SExpr::ReadPlace { place });
                }
                let base = self.take_expr_operand(*base);
                let index = self.lower_expr_operand(*index);
                self.emit_expr_with_origin(origin, ty, SExpr::Index { base, index })
            }
            Expr::Un(_, UnOp::Mut | UnOp::MaybeMut | UnOp::Ref) => self.lower_access(expr),
            Expr::Un(_, UnOp::Deref) => {
                let place = self.lower_place(expr);
                self.emit_expr_with_origin(origin, ty, SExpr::ReadPlace { place })
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
                let expr = match self.typed_body.builtin_op(expr) {
                    Some(BuiltinOp::Unary(op)) => SExpr::Unary { op, value },
                    // `+x` is `x`.
                    None => SExpr::Forward(value),
                    Some(BuiltinOp::Binary(op)) => {
                        panic!("unary operator {expr:?} has binary operation {op:?}")
                    }
                };
                self.emit_expr_with_origin(origin, ty, expr)
            }
            Expr::Bin(lhs, rhs, BinOp::Arith(ArithBinOp::Range)) => {
                let ty = self.expr_ty(expr);
                let lhs = self.take_expr_operand(*lhs);
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
            // An operator's operands are ordinary arguments, evaluated left
            // to right.
            Expr::Bin(lhs, rhs, op) => {
                if self.typed_body.semantic_expr_lowering(expr).is_some() {
                    return self.lower_call_like_expr(expr, ty, None, &[*lhs, *rhs]);
                }
                if matches!(op, BinOp::Logical(_)) {
                    return self.lower_logical_expr(expr);
                }
                let op = self.builtin_binary_op(expr);
                let lhs = self.take_expr_operand(*lhs);
                let rhs = self.lower_expr_operand(*rhs);
                self.emit_expr_with_origin(origin, ty, SExpr::Binary { op, lhs, rhs })
            }
            Expr::Cast(value, _) => {
                let from = self.expr_ty(*value);
                self.lower_cast_expr(origin, *value, from, ty)
            }
            Expr::Call(_, args) => {
                self.lower_call(expr, self.typed_body.call_receiver(self.db, expr), args)
            }
            Expr::Assert(args) => self.lower_assert(expr, args),
            Expr::UnsupportedMacroCall => {
                unreachable!("unsupported macro calls must be rejected before semantic lowering")
            }
            Expr::MethodCall(receiver, _, _, args) => self.lower_call(expr, Some(*receiver), args),
            // The target's root is resolved first, then the value, then the
            // target's sessions open, so the value may read what the target
            // is reached through.
            Expr::Assign(dst, src) => {
                self.prepare_place_root(*dst);
                let src = self.take_expr_operand(*src);
                if self.typed_body.semantic_expr_lowering(*dst).is_some() {
                    let dst = SPlace::new(self.lower_access(*dst));
                    self.drop_before_write(origin, &dst, self.locals[src.value.index()].ty);
                    self.push_stmt(origin, SStmtKind::Store { dst, src });
                } else {
                    let dst = self.lower_place(*dst);
                    self.push_place_write(origin, dst, src);
                }
                self.unit_value()
            }
            // `x op= y` reads `x` once, after `y`, and writes the operation's
            // result back: a built-in operation, or in unchecked code the
            // wrapping operator method (`x = x.wrapping_add(y)`). Otherwise it
            // calls the `*Assign` method. The method takes `y` at its
            // parameter's mode, and `x` as the value read out of the place,
            // which its `own self` takes.
            Expr::AugAssign(dst, src, _) => {
                let assigns_call = self.typed_body.assigns_operator_result(expr);
                let lowering = self.typed_body.semantic_expr_lowering(expr);
                if !assigns_call && lowering.is_some() {
                    return self.lower_call_like_expr(expr, ty, Some(*dst), &[*src]);
                }
                self.prepare_place_root(*dst);
                let rhs = match lowering {
                    Some(SemanticExprLowering::Call { callable }) => {
                        let mode = callable
                            .params(self.db)
                            .and_then(|params| params[1].shape.param_mode())
                            .expect("a wrapping operator method takes its operand at a mode");
                        let value = self.lower_callable_argument(*src, callable, mode, false);
                        SOperand::expr(value, *src)
                    }
                    _ => self.take_expr_operand(*src),
                };
                // A projection's result is read and written through its
                // access, as an assignment's target is.
                let projection = self.typed_body.semantic_expr_lowering(*dst).is_some();
                let dst_place = if projection {
                    SPlace::new(self.lower_access(*dst))
                } else {
                    self.lower_place(*dst)
                };
                let read = if projection {
                    Some(SExpr::UseValue(SOperand::expr(dst_place.local, *dst)))
                } else {
                    (!dst_place.path.is_empty()).then(|| SExpr::ReadPlace {
                        place: dst_place.clone(),
                    })
                };
                let lhs = match read {
                    Some(read) => {
                        let ty = self.expr_ty(*dst);
                        let value = self.emit_expr_with_origin(SemOrigin::Expr(*dst), ty, read);
                        SOperand::expr(value, *dst)
                    }
                    None => self.lower_expr_operand(*dst),
                };
                let dst_ty = self.projectable_place_ty(self.expr_ty(*dst));
                let result = SOperand::inherited(if assigns_call {
                    self.emit_call(expr, dst_ty, vec![lhs, rhs])
                } else {
                    let op = self.builtin_binary_op(expr);
                    self.emit_expr_with_origin(origin, dst_ty, SExpr::Binary { op, lhs, rhs })
                });
                if projection {
                    self.push_stmt(
                        origin,
                        SStmtKind::Store {
                            dst: dst_place,
                            src: result,
                        },
                    );
                } else {
                    self.push_place_write(origin, dst_place, result);
                }
                self.unit_value()
            }
            Expr::Block(stmts, _) => self.lower_block_expr(stmts),
            Expr::If(cond, then_expr, else_expr) => {
                self.lower_if_expr(expr, *cond, *then_expr, *else_expr)
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
            SExpr::Const(SConst::from_trusted_source(self.db, value)),
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
            SExpr::Const(SConst::from_trusted_source(self.db, value)),
        ))
    }

    fn lower_const_ref(&mut self, expr: ExprId, const_ref: ConstRef<'db>) -> SValueId {
        let ty = self.expr_ty(expr);
        // A constant is selected in the instance's environment: the bounds its
        // use was checked under, once substituted, may hold only by the bounds
        // of the context the instance was made for.
        let const_ref = match const_ref {
            ConstRef::TraitConst {
                use_,
                evidence,
                bound_evidence,
            } => ConstRef::TraitConst {
                use_: use_.with_env(use_.origin_scope(), self.assumptions),
                evidence,
                bound_evidence,
            },
            ConstRef::InherentConst(use_) => {
                ConstRef::InherentConst(use_.with_env(use_.origin_scope(), self.assumptions))
            }
            ConstRef::Const(_) => const_ref,
        };
        if let Some(const_ref) =
            resolve_semantic_const_ref(self.db, const_ref, ty, SemOrigin::Expr(expr))
        {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::Const(SConst::Ref(const_ref)),
            );
        }
        // Unresolved selection remains a typed description. Resolved constants
        // above retain their reference identity until the body is complete, so
        // evaluation can use the machine's const cycle detection.
        let symbolic_const_ty = match const_ref {
            ConstRef::TraitConst { use_: assoc, .. } => {
                const_ty_or_abstract_from_assoc_const_use(self.db, assoc, ty)
            }
            ConstRef::InherentConst(use_) => {
                const_ty_or_abstract_from_inherent_const_use(self.db, use_, ty)
            }
            ConstRef::Const(_) => None,
        };
        if let Some(const_ty) = symbolic_const_ty
            && let Some(symbolic) = sem_const_from_ty(self.db, TyId::const_ty(self.db, const_ty))
        {
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::Const(SConst::from_trusted_source(self.db, symbolic)),
            );
        }

        panic!("const ref should resolve to a semantic instance: {const_ref:?}");
    }

    fn lower_path_expr(&mut self, expr: ExprId) -> SValueId {
        if let Some(binding) = self.typed_body.expr_binding(expr) {
            let semantics = self
                .typed_body
                .path_expr_read_semantics(expr)
                .expect("binding path should have typed read semantics");
            return self.lower_binding_read(expr, binding, self.expr_ty(expr), Some(semantics));
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
                    enum_ty: self.expr_ty(expr),
                    variant: VariantIndex(variant.variant.idx),
                    fields: Box::new([]),
                },
            ),
            Some(ValuePathRef::TypeConst(ty)) => {
                if let Some(value) = sem_const_from_ty(self.db, ty) {
                    let TyData::ConstTy(template) = ty.data(self.db) else {
                        unreachable!("type-level value paths contain constant templates")
                    };
                    let instantiated =
                        instantiate_const_template(self.db, self.instance, *template);
                    let instantiated =
                        sem_const_from_ty(self.db, TyId::const_ty(self.db, instantiated))
                            .expect("instantiated constant template retains its value description");
                    let constant = reify_runtime_const_for_ty(
                        self.db,
                        self.instance,
                        self.expr_ty(expr),
                        instantiated,
                    )
                    .map_or_else(
                        || {
                            // Only a value path naming a declaration parameter
                            // retains formal identity for runtime ABI evidence.
                            if matches!(template.data(self.db), ConstTyData::TyParam(..)) {
                                SConst::Evidence(value)
                            } else {
                                SConst::from_trusted_source(self.db, instantiated)
                            }
                        },
                        |value| SConst::from_trusted_source(self.db, value),
                    );
                    self.emit_expr_with_origin(
                        SemOrigin::Expr(expr),
                        self.expr_ty(expr),
                        SExpr::Const(constant),
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
                let ty = self.expr_ty(expr);
                debug_assert!(
                    ty.is_func(self.db),
                    "function-item path has non-function type"
                );
                // A function item value is the fieldless record of its item
                // type, a constant rather than an aggregate construction.
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::Const(SConst::from_trusted_source(
                        self.db,
                        struct_const(self.db, ty, Box::new([])),
                    )),
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

    /// Reads `binding` as a `ty` value for `expr`: a path to it, or a
    /// closure capturing it. A closure body reads its captures from its
    /// environment.
    fn lower_binding_read(
        &mut self,
        expr: ExprId,
        binding: LocalBinding<'db>,
        ty: TyId<'db>,
        semantics: Option<PathReadSemantics>,
    ) -> SValueId {
        let origin = SemOrigin::Expr(expr);
        if let Some((place, _)) = self.capture_places.get(&binding).cloned() {
            return self.emit_expr_with_origin(origin, ty, SExpr::ReadPlace { place });
        }
        let local = *self
            .binding_locals
            .get(&binding)
            .expect("binding local should be allocated");
        match self.binding_path_read_semantics(
            binding,
            semantics.unwrap_or(PathReadSemantics::ReuseLocal),
            ty,
        ) {
            PathReadSemantics::ReuseLocal => local,
            PathReadSemantics::ForwardInterface => {
                self.emit_expr_with_origin(origin, ty, SExpr::Forward(SOperand::inherited(local)))
            }
            PathReadSemantics::MaterializeValue => {
                self.emit_expr_with_origin(origin, ty, SExpr::UseValue(SOperand::inherited(local)))
            }
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
            | SemanticLocalRole::PlaceBoundValue { .. } => PathReadSemantics::ForwardInterface,
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
        let ty = self.expr_ty(expr);
        let exprs: Vec<_> = fields.iter().map(|field| field.expr).collect();
        let operands = self.lower_expr_operands(&exprs);
        match self
            .typed_body
            .record_init_lowering(expr)
            .unwrap_or_else(|| panic!("record init lowering missing for {expr:?}"))
        {
            RecordInitLowering::EnumVariant(variant) => {
                let mut values = vec![None; fields.len()];
                for (field, operand) in fields.iter().zip(operands) {
                    let Some(label) = field.label_eagerly(self.db, self.body) else {
                        panic!("record variant init field label missing")
                    };
                    let idx = RecordLike::from_variant(variant)
                        .record_field_idx(self.db, label)
                        .expect("record variant field should resolve");
                    values[idx] = Some(operand);
                }
                self.emit_expr_with_origin(
                    SemOrigin::Expr(expr),
                    ty,
                    SExpr::EnumMake {
                        enum_ty: ty,
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
                for (field, operand) in fields.iter().zip(operands) {
                    let Some(label) = field.label_eagerly(self.db, self.body) else {
                        panic!("record init field label missing")
                    };
                    let idx = RecordLike::Type(ty)
                        .record_field_idx(self.db, label)
                        .expect("record field should resolve");
                    values[idx] = Some(operand);
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
                let receiver = receiver.map(|receiver| {
                    let prepared = self
                        .prepared_receivers
                        .remove(&expr)
                        .unwrap_or_else(|| self.prepare_receiver(receiver, callable));
                    (receiver, prepared)
                });
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
            .expr_code_region_ref(self.db, region_arg).map_or_else(|| {
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
            }, SemanticCodeRegionTarget::Resolved)
    }

    /// Lowers a call. Its receiver's root place was resolved before the
    /// arguments; its accesses open after them.
    fn lower_callable_expr(
        &mut self,
        expr: ExprId,
        ty: TyId<'db>,
        receiver: Option<(ExprId, Receiver<'db>)>,
        args: &[ExprId],
        callable: &Callable<'db>,
    ) -> SValueId {
        // An instance knows the shapes a template's opaque parameters are.
        let scope = self.body.scope();
        let params: Vec<_> = callable
            .params(self.db)
            .expect("a checked call's spread lists its parameters")
            .into_iter()
            .map(|param| CallParam {
                shape: normalize_ty(self.db, param.shape, scope, self.assumptions),
                ..param
            })
            .collect();
        let mut values = Vec::with_capacity(params.len());
        for (&arg, param) in args.iter().zip(&params[usize::from(receiver.is_some())..]) {
            let value = match param.shape.param_mode() {
                Some(mode) => self.lower_callable_argument(arg, callable, mode, false),
                // An opaque shape is transported whole, as its carrier: a
                // projection's result keeps its grant through the call.
                None if matches!(param.shape, Shape::Opaque(_)) => {
                    let value = self.lower_source(arg);
                    self.take_operand(arg, value)
                }
                None => {
                    self.lower_product_arg(arg, &param.shape, &mut values);
                    continue;
                }
            };
            values.push(SOperand::expr(value, arg));
        }
        if let Some((receiver, prepared)) = receiver {
            let value = self.open_receiver(receiver, prepared, callable);
            values.insert(0, SOperand::expr(value, receiver));
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
            CallableDef::StructCtor(_) => self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                ty,
                SExpr::AggregateMake {
                    ty,
                    fields: values.into_boxed_slice(),
                },
            ),
            CallableDef::Func(_) => self.emit_call(expr, ty, values),
        }
    }

    /// Emits the function call `expr` resolved to, with its inputs `args`.
    fn emit_call(&mut self, expr: ExprId, ty: TyId<'db>, mut args: Vec<SOperand>) -> SValueId {
        let call_site = self
            .call_sites
            .get(expr.index())
            .and_then(|site| site.as_ref())
            .unwrap_or_else(|| panic!("call lowering plan missing semantic callee for {expr:?}"));
        let callee = call_site
            .callee
            .unwrap_or_else(|| panic!("call lowering plan missing callee for {expr:?}"));
        // A function item called through a callable shape is not passed: the
        // function takes the operands after it.
        if call_site.drops_receiver {
            args.remove(0);
        }
        let effect_args = self.lower_effect_arg_slice(&call_site.effect_args);
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            ty,
            SExpr::Call {
                call_site: CallSiteId::Expr(expr),
                callee,
                args: args.into_boxed_slice(),
                effect_args,
            },
        )
    }

    /// The built-in operation of the binary operator `expr`, which the type
    /// checker records for operands it does not call an operator trait for.
    fn builtin_binary_op(&self, expr: ExprId) -> BinaryOp {
        match self.typed_body.builtin_op(expr) {
            Some(BuiltinOp::Binary(op)) => op,
            op => panic!("binary operator {expr:?} has no built-in binary operation: {op:?}"),
        }
    }

    /// Lowers the call input for a parameter of `mode`. A place passed to a view
    /// parameter is viewed in place unless it is a snapshot (a scalar or
    /// handle) or a `Copy` value an ordinary function may take by value:
    /// loading a non-`Copy` place would move it, and a projection's result
    /// over a `Copy` aggregate is the caller's place. (Either way the call
    /// reads the place for its duration.) A variable's value is taken where
    /// it appears, before the arguments after it: a mutable local is viewed
    /// here unless it is a receiver, which opens right before the call. Call
    /// normalization views other local roots directly; an effect binding is
    /// viewed here when its template type is not a parameter, so every
    /// instance lowers it alike.
    fn lower_callable_argument(
        &mut self,
        expr: ExprId,
        callable: &Callable<'db>,
        mode: FuncParamMode,
        receiver: bool,
    ) -> SValueId {
        let ty = self.expr_ty(expr);
        if mode == FuncParamMode::Own {
            let value = self.lower_expr(expr);
            return self.take_operand(expr, value);
        }
        // A closure literal or function item path passed to a `mut`
        // parameter is a temporary.
        if mode == FuncParamMode::Mut
            && (matches!(
                expr.data(self.db, self.body),
                Partial::Present(Expr::Closure { .. })
            ) || self.typed_body.value_path_ref(expr) == Some(ValuePathRef::FunctionItem))
        {
            let local = self.hoist_temporary(expr, ty);
            return self.emit_borrow(expr, SPlace::new(local), BorrowKind::Mut, ty);
        }
        // An access binding passed to a view parameter is viewed through its
        // carrier. Later arguments may write through a `mut` one, so an
        // argument other than the receiver is taken where it appears, as a
        // local's is: a snapshot is read there, and anything else viewed
        // there.
        let scope = self.body.scope();
        if matches!(
            expr.data(self.db, self.body),
            Partial::Present(Expr::Path(_))
        ) && let Some(binding) = self.typed_body.expr_binding(expr)
            && !self.capture_places.contains_key(&binding)
            && let Some(access) = self.typed_body.binding_access(binding)
        {
            if receiver || !access.is_mut() {
                return self.binding_locals[&binding];
            }
            if !ty_is_snapshot(self.db, scope, ty, self.assumptions)
                && let Some(place) = self.try_lower_place(expr)
            {
                return self.emit_borrow(expr, place, BorrowKind::Ref, ty);
            }
        }
        let in_place = mode == FuncParamMode::View
            && self.typed_body.expr_prop(self.db, expr).shape.is_none()
            && !ty_is_snapshot(self.db, scope, ty, self.assumptions)
            && (!ty_is_copy(self.db, scope, ty, self.assumptions)
                || matches!(
                    callable.callable_def(),
                    CallableDef::Func(func) if func.is_projection(self.db)
                ));
        if in_place
            && let Some(place) = self.try_lower_place(expr)
            && (!place.path.is_empty()
                || !receiver && self.locals[place.local.index()].mutability == Mutability::Mutable
                || !ty.is_zero_sized(self.db)
                    && infer_body(self.db, self.template_owner)
                        .1
                        .expr_ty(self.db, expr)
                        .as_generic_param(self.db)
                        .is_none()
                    && matches!(
                        self.locals[place.local.index()].source,
                        Some(LocalBinding::EffectParam { .. })
                    ))
        {
            return self.emit_borrow(expr, place, BorrowKind::Ref, ty);
        }
        let value = self.lower_source(expr);
        if in_place {
            value
        } else {
            self.take_operand(expr, value)
        }
    }

    /// Lowers `arg`, passed to a product parameter of `product`, to one input
    /// for each of its parts: a tuple's elements, or the carriers of a
    /// projection's result.
    fn lower_product_arg(&mut self, arg: ExprId, product: &Shape<'db>, values: &mut Vec<SOperand>) {
        if !self.typed_body.is_product_arg(arg) {
            let value = self.lower_source(arg);
            self.lower_product_value(arg, value, product, values);
            return;
        }
        let (Shape::Tuple(parts), Partial::Present(Expr::Tuple(elems))) =
            (product, arg.data(self.db, self.body))
        else {
            unreachable!("a product argument is a tuple of its parts")
        };
        for (&elem, part) in elems.iter().zip(parts) {
            let value = match part {
                Shape::Tuple(_) => {
                    self.lower_product_arg(elem, part, values);
                    continue;
                }
                // An opaque part is transported whole, as a value.
                Shape::Owned(_) | Shape::Opaque(_) => {
                    let value = self.lower_expr(elem);
                    self.take_operand(elem, value)
                }
                Shape::Access(kind, _) => self.lower_product_part(elem, *kind),
                Shape::Sum { .. } => unreachable!("a product's parts are no sums"),
            };
            values.push(SOperand::expr(value, elem));
        }
    }

    /// The carriers of `value`, the product of shape `product` that `arg`
    /// grants, one input for each part.
    fn lower_product_value(
        &mut self,
        arg: ExprId,
        value: SValueId,
        product: &Shape<'db>,
        values: &mut Vec<SOperand>,
    ) {
        let Shape::Tuple(parts) = product else {
            values.push(SOperand::expr(value, arg));
            return;
        };
        for (idx, part) in parts.iter().enumerate() {
            let mut place = SPlace::new(value);
            place.push_field(FieldIndex(idx as u16));
            let read = self.emit_expr(part.carrier_ty(self.db), SExpr::ReadPlace { place });
            self.lower_product_value(arg, read, part, values);
        }
    }

    /// Lowers `elem`, passed as a product parameter's part of `kind`, to the
    /// carrier of an access to the caller's place: one it is already, a
    /// place it names, or a temporary holding its value.
    fn lower_product_part(&mut self, elem: ExprId, kind: BorrowKind) -> SValueId {
        if self.typed_body.expr_prop(self.db, elem).shape.is_some() {
            return self.lower_source(elem);
        }
        if matches!(
            elem.data(self.db, self.body),
            Partial::Present(Expr::Path(_))
        ) && let Some(binding) = self.typed_body.expr_binding(elem)
            && !self.capture_places.contains_key(&binding)
            && matches!(
                self.typed_body.binding_access(binding),
                Some(BindingAccess::Ref | BindingAccess::Mut)
            )
        {
            return self.binding_locals[&binding];
        }
        let ty = self.expr_ty(elem);
        let place = match self.try_lower_place(elem) {
            Some(place) => place,
            None => SPlace::new(self.hoist_temporary(elem, ty)),
        };
        self.emit_borrow(elem, place, kind, ty)
    }

    /// Resolves a receiver before the call's arguments: a temporary is
    /// evaluated now, into a local for a `mut` receiver, while a place rooted
    /// at a binding is lowered whole after them, so the arguments may read
    /// and write it and the call sees what they wrote. The root of a
    /// projection chain, or of a place in a projection's grant, is prepared
    /// the same way, and the chain's sessions open after the arguments.
    fn prepare_receiver(&mut self, receiver: ExprId, callable: &Callable<'db>) -> Receiver<'db> {
        if let Some(&base) = self.loop_bases.get(&receiver) {
            return Receiver::Value(base);
        }
        let mode = callable
            .callable_def()
            .param_mode(self.db, 0, callable.mode());
        if mode != FuncParamMode::Own
            && self.typed_body.expr_prop(self.db, receiver).shape.is_some()
        {
            if let Some(SemanticExprLowering::Call { callable: inner }) =
                self.typed_body.semantic_expr_lowering(receiver)
                && let Partial::Present(
                    Expr::MethodCall(root, ..) | Expr::Bin(root, _, BinOp::Index),
                ) = receiver.data(self.db, self.body)
            {
                let root = *root;
                return Receiver::Chain(root, Box::new(self.prepare_receiver(root, inner)));
            }
            return Receiver::Value(self.lower_access(receiver));
        }
        // An access binding is passed by its carrier.
        if mode != FuncParamMode::Own
            && matches!(
                receiver.data(self.db, self.body),
                Partial::Present(Expr::Path(_))
            )
            && let Some(binding) = self.typed_body.expr_binding(receiver)
            && !self.capture_places.contains_key(&binding)
            && self.typed_body.binding_access(binding).is_some()
        {
            return Receiver::Value(self.binding_locals[&binding]);
        }
        if self.typed_body.expr_place(receiver).is_some() || self.prepare_place_root(receiver) {
            return Receiver::Place(None);
        }
        if mode != FuncParamMode::Mut {
            return Receiver::Value(self.lower_callable_argument(receiver, callable, mode, true));
        }
        let place = self.try_lower_place(receiver).unwrap_or_else(|| {
            let ty = normalize_ty(
                self.db,
                self.expr_ty(receiver),
                self.body.scope(),
                self.assumptions,
            );
            SPlace::new(self.hoist_temporary(receiver, ty))
        });
        Receiver::Place(Some(place))
    }

    /// Resolves the root of a place in a projection's grant, an assignment
    /// target or a receiver, before the value or the arguments: the innermost
    /// projection call on the place's path of fields and builtin indexes has
    /// its receiver prepared now, hoisting a temporary root, and opens its
    /// session when the place is lowered after them. Returns whether the
    /// path has a projection call.
    fn prepare_place_root(&mut self, place: ExprId) -> bool {
        let lowering = self.typed_body.semantic_expr_lowering(place);
        match (place.data(self.db, self.body), lowering) {
            (Partial::Present(Expr::Field(base, _)), _)
            | (Partial::Present(Expr::Bin(base, _, BinOp::Index)), None) => {
                self.prepare_place_root(*base)
            }
            (
                Partial::Present(
                    Expr::MethodCall(receiver, ..) | Expr::Bin(receiver, _, BinOp::Index),
                ),
                Some(SemanticExprLowering::Call { callable }),
            ) if self.typed_body.expr_prop(self.db, place).shape.is_some() => {
                let prepared = self.prepare_receiver(*receiver, callable);
                self.prepared_receivers.insert(place, prepared);
                true
            }
            _ => false,
        }
    }

    /// Evaluates `expr` into a fresh mutable local.
    fn hoist_temporary(&mut self, expr: ExprId, ty: TyId<'db>) -> SLocalId {
        let value = self.lower_expr(expr);
        let local = self.alloc_local(ty, Mutability::Mutable, None);
        self.push_stmt(
            SemOrigin::Expr(expr),
            SStmtKind::Assign {
                dst: local,
                expr: SExpr::UseValue(SOperand::expr(value, expr)),
            },
        );
        local
    }

    /// Opens a prepared receiver's accesses: its chain's projection sessions
    /// innermost first, then the receiver's access with its mode.
    fn open_receiver(
        &mut self,
        receiver: ExprId,
        prepared: Receiver<'db>,
        callable: &Callable<'db>,
    ) -> SValueId {
        let mode = callable
            .callable_def()
            .param_mode(self.db, 0, callable.mode());
        match prepared {
            Receiver::Value(value) => value,
            Receiver::Place(None) if mode != FuncParamMode::Mut => {
                self.lower_callable_argument(receiver, callable, mode, true)
            }
            Receiver::Chain(root, prepared) => {
                let Some(SemanticExprLowering::Call { callable: inner }) =
                    self.typed_body.semantic_expr_lowering(receiver)
                else {
                    unreachable!("a receiver chain link is a call")
                };
                let args: Vec<ExprId> = match receiver.data(self.db, self.body) {
                    Partial::Present(Expr::MethodCall(_, _, _, args)) => {
                        args.iter().map(|arg| arg.expr).collect()
                    }
                    Partial::Present(Expr::Bin(_, index, BinOp::Index)) => vec![*index],
                    _ => unreachable!("a receiver chain link has a receiver"),
                };
                let ty = self
                    .typed_body
                    .expr_prop(self.db, receiver)
                    .shape
                    .expect("a receiver chain link is an access")
                    .carrier_ty(self.db);
                self.lower_callable_expr(receiver, ty, Some((root, *prepared)), &args, inner)
            }
            Receiver::Place(place) => {
                let place = place.unwrap_or_else(|| self.lower_place(receiver));
                let ty = normalize_ty(
                    self.db,
                    self.expr_ty(receiver),
                    self.body.scope(),
                    self.assumptions,
                );
                self.emit_borrow(receiver, place, BorrowKind::Mut, ty)
            }
        }
    }

    fn emit_borrow(
        &mut self,
        expr: ExprId,
        place: SPlace<'db>,
        kind: BorrowKind,
        ty: TyId<'db>,
    ) -> SValueId {
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            TyId::borrow_of(self.db, kind, ty),
            SExpr::Borrow {
                place,
                kind,
                provider: self.typed_body.expr_prop(self.db, expr).borrow_provider,
            },
        )
    }

    fn lower_const_intrinsic(
        &mut self,
        expr: ExprId,
        callable: &Callable<'db>,
        kind: ConstIntrinsicKind,
    ) -> SValueId {
        let ty = normalize_ty(
            self.db,
            *callable
                .generic_args()
                .first()
                .expect("a size intrinsic's lowering requires a generic arg"),
            self.body.scope(),
            self.assumptions,
        );
        let result_ty = self.expr_ty(expr);
        // Admission reports failed concrete demands; raw lowering preserves
        // the symbolic form for provisional and not-yet-admitted bodies.
        let size = match kind {
            ConstIntrinsicKind::SizeOf => match runtime_size_bytes(self.db, ty) {
                Ok(Some(size)) => Some(BigInt::from(size)),
                Ok(None) | Err(_) => None,
            },
            ConstIntrinsicKind::StorageSlots if ty.has_param(self.db) || ty.has_var(self.db) => {
                None
            }
            ConstIntrinsicKind::StorageSlots => {
                storage_slot_span(self.db, ty).ok().map(BigInt::from)
            }
        };
        let Some(size) = size else {
            let caller = self.instance.key(self.db);
            let key = match self.binding_role_mode {
                BindingRoleMode::Final => semantic_callee_key_with_effect_providers(
                    self.db,
                    caller,
                    callable,
                    &[],
                    callable.effect_providers(),
                ),
                BindingRoleMode::Provisional => provisional_semantic_callee_key(
                    self.db,
                    caller,
                    callable,
                    &[],
                    self.assumptions,
                ),
            }
            .ok()
            .flatten()
            .expect("const intrinsic should resolve to a function")
            .key;
            let const_expr = ConstExpr::Invocation(ConstInvocation::new(self.db, key, Vec::new()));
            let const_ty = ConstTyId::new(
                self.db,
                ConstTyData::Abstract(ConstExprId::new(self.db, const_expr), result_ty),
            );
            let value = SemConstId::new(self.db, SemConstValue::Description(const_ty));
            return self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                result_ty,
                SExpr::Const(SConst::from_trusted_source(self.db, value)),
            );
        };
        self.emit_expr_with_origin(
            SemOrigin::Expr(expr),
            result_ty,
            SExpr::Const(SConst::from_trusted_source(
                self.db,
                int_const(self.db, result_ty, size),
            )),
        )
    }

    /// A block's scope holds its bindings and its tail's temporaries; its
    /// value passes to the enclosing scope.
    fn lower_block_expr(&mut self, stmts: &[StmtId]) -> SValueId {
        self.push_drop_scope(true);
        let value = self.lower_block_stmts(stmts);
        self.pop_drop_scope(Some(value));
        value
    }

    fn lower_block_stmts(&mut self, stmts: &[StmtId]) -> SValueId {
        let Some((tail, head)) = stmts.split_last() else {
            return self.unit_value();
        };

        for stmt in head {
            self.lower_stmt(*stmt);
            if self.is_terminated(self.current) {
                return self.unit_value();
            }
        }

        if let Partial::Present(Stmt::Expr(expr)) = tail.data(self.db, self.body) {
            self.lower_expr(*expr)
        } else {
            self.lower_stmt(*tail);
            self.unit_value()
        }
    }

    /// A statement's scope holds its temporaries; its bindings belong to
    /// the enclosing block.
    fn lower_stmt(&mut self, stmt: StmtId) {
        self.push_drop_scope(false);
        self.lower_stmt_in_scope(stmt);
        self.pop_drop_scope(None);
    }

    fn lower_stmt_in_scope(&mut self, stmt: StmtId) {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            panic!("cannot lower absent statement")
        };
        let origin = SemOrigin::Stmt(stmt);

        match stmt_data {
            Stmt::Let(pat, _, init, else_) => {
                if let Some(init) = init {
                    let value = if matches!(
                        pat.data(self.db, self.body),
                        Partial::Present(Pat::Path(..))
                    ) {
                        let value = self.lower_source(*init);
                        self.value_pattern_source(value)
                    } else {
                        self.lower_scrutinee(*init)
                    };
                    match else_ {
                        Some(else_) if !self.pattern_is_irrefutable(*pat) => {
                            let then_bb = self.new_block();
                            let else_bb = self.new_block();
                            let held = self.held_counts();
                            self.lower_pattern_branch(*pat, value, then_bb, else_bb);
                            self.switch_to(else_bb);
                            self.without_held_since(&held, |this| {
                                let _ = this.lower_expr(*else_);
                            });
                            self.switch_to(then_bb);
                        }
                        _ => self.bind_pattern(*pat, value),
                    }
                    // A temporary a binding's access borrows lives as long
                    // as the binding.
                    if self.pattern_holds_access(*pat) {
                        self.extend_temporaries();
                    }
                } else {
                    self.hold_declared(*pat);
                }
            }
            Stmt::While(cond, body_expr) => self.lower_while(*cond, *body_expr),
            Stmt::For(pat, _, body_expr, _) => self.lower_for(stmt, *pat, *body_expr),
            Stmt::Continue => {
                let is_reachable = !self.is_terminated(self.current);
                let scope = self.loop_stack.last_mut().expect("continue outside loop");
                scope.has_reachable_continue |= is_reachable;
                let (continue_bb, drop_depth) = (scope.continue_bb, scope.drop_depth);
                self.destroy_scopes_from(drop_depth, None);
                self.set_terminator(self.current, origin, STerminatorKind::Goto(continue_bb));
            }
            Stmt::Break => {
                let scope = self.loop_stack.last().copied().expect("break outside loop");
                self.destroy_scopes_from(scope.drop_depth, None);
                self.set_terminator(self.current, origin, STerminatorKind::Goto(scope.break_bb));
            }
            Stmt::Return(expr) => {
                let value = expr.map(|expr| self.lower_expr_operand(expr));
                if !self.is_terminated(self.current) {
                    self.exit(
                        origin,
                        value.filter(|_| {
                            expr.is_none_or(|expr| self.expr_ty(expr) != TyId::unit(self.db))
                        }),
                    );
                }
            }
            Stmt::Yield(expr) => {
                let slide = self.new_block();
                let outer = self.slide.replace(slide);
                let _ = self.lower_expr(*expr);
                self.slide = outer;
                self.current = slide;
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

        // An iteration's scope holds what its condition binds.
        self.switch_to(cond_bb);
        let drop_depth = self.drop_scopes.len();
        self.push_drop_scope(true);
        self.lower_cond_branch(cond, body_bb, exit_bb, drop_depth, true);

        self.loop_stack.push(LoopScope {
            continue_bb: cond_bb,
            break_bb: exit_bb,
            has_reachable_continue: false,
            drop_depth,
        });
        self.switch_to(body_bb);
        let _ = self.lower_expr(body_expr);
        self.pop_drop_scope(None);
        if !self.is_terminated(self.current) {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(cond_bb));
        }
        self.loop_stack.pop();

        self.switch_to(exit_bb);
    }

    /// A loop base: the carrier of an access binding, a value the loop holds
    /// (a local root, an access, a snapshot or a temporary), or a place it
    /// views afresh for each call, so a by-value traversal holds no access on
    /// it between steps. The template's type decides, so every instance of a
    /// template lowers alike.
    fn lower_loop_base(&mut self, base: ExprId) -> LoopBase<'db> {
        if self.typed_body.expr_prop(self.db, base).shape.is_none() {
            if matches!(
                base.data(self.db, self.body),
                Partial::Present(Expr::Path(_))
            ) && let Some(binding) = self.typed_body.expr_binding(base)
                && !self.capture_places.contains_key(&binding)
                && self.typed_body.binding_access(binding).is_some()
            {
                return LoopBase::Value(self.binding_locals[&binding]);
            }
            let template = &infer_body(self.db, self.template_owner).1;
            let template_ty = template.expr_ty(self.db, base);
            if !ty_is_snapshot(
                self.db,
                self.body.scope(),
                template_ty,
                template.assumptions(),
            ) && let Some(place) = self.try_lower_place_expr(base, true)
                && (!place.path.is_empty()
                    || template_ty.as_generic_param(self.db).is_none()
                        && matches!(
                            self.locals[place.local.index()].source,
                            Some(LocalBinding::EffectParam { .. })
                        ))
            {
                return LoopBase::Place(place, self.expr_ty(base));
            }
        }
        LoopBase::Value(self.lower_source(base))
    }

    fn loop_base_operand(&mut self, expr: ExprId, base: &LoopBase<'db>) -> SOperand {
        match base {
            LoopBase::Value(value) => SOperand::expr(*value, expr),
            LoopBase::Place(place, ty) => SOperand::expr(
                self.emit_borrow(expr, place.clone(), BorrowKind::Ref, *ty),
                expr,
            ),
        }
    }

    /// Lowers `for pat in base { body }` to the protocol its plan selects:
    /// the collection's own, or that of the method chain driving it. The
    /// header tests the state the body then projects from with `has`, so only
    /// the state is carried around the loop. The next state is taken at the
    /// latch, after the body and its element's cleanup, which is where
    /// `continue` goes; `break` leaves without advancing. The latch does not
    /// test the state again: a body over copied items may change the base,
    /// and `produce` the producer, so `next` accepts a state that may name no
    /// element by now, and the header tests what it returns.
    fn lower_for(&mut self, stmt: StmtId, pat: PatId, body_expr: ExprId) {
        let sites = self
            .for_loop_call_sites
            .get(stmt.index())
            .and_then(|sites| sites.as_ref())
            .unwrap_or_else(|| panic!("missing staged callee refs for for-loop {stmt:?}"));
        let plan = self
            .typed_body
            .for_loop_plan(stmt)
            .unwrap_or_else(|| panic!("missing loop protocol for for-loop {stmt:?}"));
        let base_expr = plan.base;
        let base = self.lower_loop_base(base_expr);
        // A method chain rooted at the base views it for its own calls.
        let value = self.loop_base_operand(base_expr, &base).value;
        self.loop_bases.insert(base_expr, value);
        // The driver is evaluated once, so the body cannot retarget it. A
        // producer advances its own copy, which the loop takes; a driver that
        // a projection yields is held as a base is, by a view of its place for
        // the whole loop, unless it is `Copy`.
        let driver = plan.driver.map(|driver| {
            let ty = self.expr_ty(driver);
            if !plan.produces
                && !ty_is_copy(self.db, self.body.scope(), ty, self.assumptions)
                && let Some(place) = self.try_lower_place(driver)
            {
                return SOperand::expr(
                    self.emit_borrow(driver, place, BorrowKind::Ref, ty),
                    driver,
                );
            }
            let value = self.lower_source(driver);
            let value = SOperand::expr(self.take_operand(driver, value), driver);
            let ty = self.locals[value.value.index()].ty;
            let temp = self.alloc_local(ty, Mutability::Mutable, None);
            self.push_synthetic_stmt(SStmtKind::Assign {
                dst: temp,
                expr: SExpr::Forward(value),
            });
            SOperand::synthetic(temp)
        });
        // The calls take the driver first, then the base, then the state.
        let inputs = |this: &mut Self| -> Vec<SOperand> {
            driver
                .into_iter()
                .chain([this.loop_base_operand(base_expr, &base)])
                .collect()
        };
        let with_state = |this: &mut Self, state| {
            let mut args = inputs(this);
            args.push(SOperand::synthetic(state));
            args
        };
        let call = |this: &mut Self, step, args: Vec<SOperand>, ty| {
            let site = sites.site(step);
            let effect_args = this.lower_effect_arg_slice(&site.effect_args);
            // Protocol calls belong to the loop, including when CTFE reports
            // a failure in a generated call rather than a written expression.
            this.emit_expr_with_origin(
                SemOrigin::Stmt(stmt),
                ty,
                SExpr::Call {
                    call_site: CallSiteId::ForLoop(stmt, step),
                    callee: site
                        .callee
                        .expect("a loop protocol call lowers to a semantic callee"),
                    args: args.into_boxed_slice(),
                    effect_args,
                },
            )
        };
        let shape = &plan.shape;
        let state = self.alloc_temp(plan.state_ty);
        let header_bb = self.new_block();
        let body_bb = self.new_block();
        let latch_bb = self.new_block();
        let exit_bb = self.new_block();
        // Holds the state `start` or `next` makes, and goes to the header.
        let advance = |this: &mut Self, value| {
            this.push_synthetic_stmt(SStmtKind::Assign {
                dst: state,
                expr: SExpr::Forward(SOperand::synthetic(value)),
            });
            this.set_synthetic_terminator(this.current, STerminatorKind::Goto(header_bb));
        };
        let args = inputs(self);
        let first = call(self, ForLoopStep::Start, args, plan.state_ty);
        advance(self, first);

        self.switch_to(header_bb);
        let args = with_state(self, state);
        let has = call(self, ForLoopStep::Has, args, TyId::bool(self.db));
        self.set_synthetic_terminator(
            self.current,
            STerminatorKind::Branch {
                cond: SOperand::synthetic(has),
                then_bb: body_bb,
                else_bb: exit_bb,
            },
        );

        // An iteration's scope holds the element and what the pattern binds.
        let drop_depth = self.drop_scopes.len();
        self.loop_stack.push(LoopScope {
            continue_bb: latch_bb,
            break_bb: exit_bb,
            has_reachable_continue: false,
            drop_depth,
        });
        self.switch_to(body_bb);
        self.push_drop_scope(true);
        let mut at_args = with_state(self, state);
        // `produce` advances the driver through a `mut` access.
        if let (Some(temp), Some(driver)) = (driver, plan.driver)
            && plan.produces
        {
            let ty = self.locals[temp.value.index()].ty;
            let place = SPlace::new(temp.value);
            at_args[0] = SOperand::synthetic(self.emit_borrow(driver, place, BorrowKind::Mut, ty));
        }
        // The pattern binds the step as a `let` binds a projection's result:
        // copies end the step's session before the body runs.
        let yielded = call(self, ForLoopStep::At, at_args, shape.carrier_ty(self.db));
        let step = self.value_pattern_source(yielded);
        self.bind_pattern(pat, step);
        let _ = self.lower_expr(body_expr);
        self.pop_drop_scope(None);
        let falls_through = !self.is_terminated(self.current);
        if falls_through {
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(latch_bb));
        }
        let scope = self.loop_stack.pop().expect("for loop scope");
        if falls_through || scope.has_reachable_continue {
            self.switch_to(latch_bb);
            let args = with_state(self, state);
            let next = call(self, ForLoopStep::Next, args, plan.state_ty);
            advance(self, next);
        } else {
            // Every body path leaves the loop, so nothing reaches the latch.
            // Close it like a dead `if`/`match` join.
            self.set_synthetic_terminator(latch_bb, STerminatorKind::Goto(latch_bb));
        }
        self.switch_to(exit_bb);
    }

    fn lower_if_expr(
        &mut self,
        expr: ExprId,
        cond: CondId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> SValueId {
        let result_ty = self.expr_ty(expr);
        let result = self.alloc_temp(result_ty);
        let then_bb = self.new_block();
        let else_bb = self.new_block();
        let join_bb = self.new_block();
        let mut join_reachable = false;

        // The `then` branch's scope holds what the condition binds.
        let then_depth = self.drop_scopes.len();
        self.push_drop_scope(true);
        self.lower_cond_branch(cond, then_bb, else_bb, then_depth, true);

        // Each branch's value passes to the join, so the join takes it.
        self.switch_to(then_bb);
        let then_value = self.lower_expr(then_expr);
        let then_falls = !self.is_terminated(self.current);
        if then_falls {
            let then_value = self.take_operand(then_expr, then_value);
            self.assign_branch_result(result, SOperand::expr(then_value, then_expr));
        }
        self.pop_drop_scope(None);
        if then_falls {
            join_reachable = true;
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        }

        self.switch_to(else_bb);
        self.push_drop_scope(false);
        let else_value = else_expr.map(|expr| (expr, self.lower_expr(expr)));
        let else_falls = !self.is_terminated(self.current);
        if else_falls {
            let else_value = match else_value {
                Some((expr, value)) => SOperand::expr(self.take_operand(expr, value), expr),
                None => SOperand::synthetic(self.unit_value()),
            };
            self.assign_branch_result(result, else_value);
        }
        self.pop_drop_scope(None);
        if else_falls {
            join_reachable = true;
            self.set_synthetic_terminator(self.current, STerminatorKind::Goto(join_bb));
        }

        self.switch_to(join_bb);
        if join_reachable {
            self.hold_defined(result);
        } else {
            self.set_synthetic_terminator(join_bb, STerminatorKind::Goto(join_bb));
        }
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
                SExpr::Const(SConst::from_trusted_source(
                    self.db,
                    bool_const(self.db, true),
                )),
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
                SExpr::Const(SConst::from_trusted_source(
                    self.db,
                    bool_const(self.db, false),
                )),
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
            // The condition's temporaries end before it branches.
            _ => {
                self.push_drop_scope(false);
                let cond = self.lower_expr(expr);
                self.pop_drop_scope(None);
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
        let value = self.lower_scrutinee(scrutinee);
        let result = self.alloc_temp(self.expr_ty(expr));
        let join_bb = self.new_block();
        self.lower_match_expr_with_decision_tree(value, result, join_bb, arms)
    }

    /// Branches on `cond`. What it binds, and the scrutinees those bindings
    /// come from, belong to the scope at `then_depth`, which a failed
    /// condition leaves when `else_bb` is outside it (`exits`).
    fn lower_cond_branch(
        &mut self,
        cond: CondId,
        then_bb: SBlockId,
        else_bb: SBlockId,
        then_depth: usize,
        exits: bool,
    ) {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            panic!("cannot lower absent condition")
        };
        let held = exits.then_some(then_depth);

        match cond_data {
            // The condition's temporaries end before it branches.
            Cond::Expr(expr) => {
                self.push_drop_scope(false);
                let cond = self.lower_expr(*expr);
                self.pop_drop_scope(None);
                let else_bb = self.condition_exit(else_bb, held, &[]);
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
                self.lower_cond_branch(*lhs, rhs_bb, else_bb, then_depth, exits);
                self.switch_to(rhs_bb);
                self.lower_cond_branch(*rhs, then_bb, else_bb, then_depth, exits);
            }
            Cond::Bin(lhs, rhs, LogicalBinOp::Or) => {
                let rhs_bb = self.new_block();
                self.lower_cond_branch(*lhs, then_bb, rhs_bb, then_depth, false);
                self.switch_to(rhs_bb);
                self.lower_cond_branch(*rhs, then_bb, else_bb, then_depth, exits);
            }
            // A matched scrutinee lives on with what is bound from it; a
            // failed match destroys it. An irrefutable pattern never fails.
            Cond::Let(pat, expr) => {
                self.push_drop_scope(false);
                let value = self.lower_scrutinee(*expr);
                let scrutinee = self.take_drop_scope();
                let else_bb = (!self.pattern_is_irrefutable(*pat))
                    .then(|| self.condition_exit(else_bb, held, &scrutinee));
                for owner in scrutinee {
                    self.hold_in(owner, then_depth);
                }
                match else_bb {
                    Some(else_bb) => self.lower_pattern_branch(*pat, value, then_bb, else_bb),
                    None => {
                        self.bind_pattern(*pat, value);
                        self.set_synthetic_terminator(self.current, STerminatorKind::Goto(then_bb));
                    }
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
        self.drop_before_write(origin, &dst, self.locals[src.value.index()].ty);
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
