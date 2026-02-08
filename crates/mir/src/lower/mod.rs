//! MIR lowering entrypoints and shared builder scaffolding. Dispatches to submodules that handle
//! expression lowering, intrinsics, matches, aggregates (records/variants), layout, and contract
//! metadata.

use std::{error::Error, fmt};

use common::ingot::IngotKind;
use hir::analysis::{
    HirAnalysisDb,
    diagnostics::SpannedHirAnalysisDb,
    ty::{
        adt_def::AdtRef,
        ty_check::{
            EffectParamSite, LocalBinding, ParamSite, RecordLike, TypedBody, check_func_body,
        },
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};
use hir::hir_def::{
    Attr, AttrArg, AttrArgValue, Body, CallableDef, Const, Expr, ExprId, Field, FieldIndex, Func,
    IdentId, ItemKind, LitKind, MatchArm, Partial, Pat, PatId, Stmt, StmtId, TopLevelMod,
    VariantKind, expr::BinOp,
};

use crate::{
    core_lib::CoreLib,
    ir::{
        AddressSpaceKind, BasicBlockId, BodyBuilder, CallOrigin, CodeRegionRoot, ContractFunction,
        ContractFunctionKind, IntrinsicOp, LocalData, LocalId, LoopInfo, MirBody, MirFunction,
        MirInst, MirModule, MirProjection, MirProjectionPath, Place, Rvalue, SwitchTarget,
        SwitchValue, SyntheticValue, Terminator, ValueData, ValueId, ValueOrigin, ValueRepr,
    },
    monomorphize::monomorphize_functions,
};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

mod aggregates;
mod contract;
mod contracts;
mod diagnostics;
mod expr;
mod intrinsics;
mod match_lowering;
mod prepass;
mod variants;

pub(super) use contract::extract_contract_function;
use hir::span::LazySpan;

/// Errors that can occur while lowering HIR into MIR.
#[derive(Debug)]
pub enum MirLowerError {
    MissingBody {
        func_name: String,
    },
    AnalysisDiagnostics {
        func_name: String,
        diagnostics: String,
    },
    UnloweredHirExpr {
        func_name: String,
        expr: String,
    },
    Unsupported {
        func_name: String,
        message: String,
    },
}

impl fmt::Display for MirLowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirLowerError::MissingBody { func_name } => {
                write!(f, "function `{func_name}` is missing a body")
            }
            MirLowerError::AnalysisDiagnostics {
                func_name,
                diagnostics,
            } => {
                writeln!(f, "analysis errors while lowering `{func_name}`:")?;
                write!(f, "{diagnostics}")
            }
            MirLowerError::UnloweredHirExpr { func_name, expr } => {
                write!(
                    f,
                    "unlowered HIR expression survived MIR lowering in `{func_name}`: {expr}"
                )
            }
            MirLowerError::Unsupported { func_name, message } => {
                write!(f, "unsupported while lowering `{func_name}`: {message}")
            }
        }
    }
}

impl Error for MirLowerError {}

pub type MirLowerResult<T> = Result<T, MirLowerError>;

/// Field type and byte offset information used when lowering record/variant accesses.
pub(super) struct FieldAccessInfo<'db> {
    pub(super) field_ty: TyId<'db>,
    pub(super) field_idx: usize,
}

/// Lowers every function within the top-level module into MIR.
///
/// # Parameters
/// - `db`: HIR analysis database.
/// - `top_mod`: The module containing functions/impls to lower.
///
/// # Returns
/// A populated `MirModule` on success.
pub fn lower_module<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> MirLowerResult<MirModule<'db>> {
    let mut templates = Vec::new();
    let mut funcs_to_lower = Vec::new();
    let mut seen = FxHashSet::default();

    let mut queue_func = |func: Func<'db>| {
        if seen.insert(func) {
            funcs_to_lower.push(func);
        }
    };

    // Skip associated functions here to avoid pulling in trait methods (which may refer to
    // abstract associated items) as MIR templates. Impl/impl-trait functions are queued below.
    for &func in top_mod.all_funcs(db) {
        if !func.is_associated_func(db) {
            queue_func(func);
        }
    }

    for &impl_block in top_mod.all_impls(db) {
        for func in impl_block.funcs(db) {
            queue_func(func);
        }
    }
    for &impl_trait in top_mod.all_impl_traits(db) {
        for func in impl_trait.methods(db) {
            queue_func(func);
        }
    }

    for func in funcs_to_lower {
        if func.body(db).is_none() {
            continue;
        }
        let (diags, typed_body) = check_func_body(db, func);
        if !diags.is_empty() {
            let func_name = func
                .name(db)
                .to_opt()
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_else(|| "<anonymous>".to_string());
            let rendered = diagnostics::format_func_body_diags(db, diags);
            return Err(MirLowerError::AnalysisDiagnostics {
                func_name,
                diagnostics: rendered,
            });
        }
        let lowered = lower_function(db, func, typed_body.clone(), None, Vec::new(), Vec::new())?;
        templates.push(lowered);
    }

    templates.extend(contracts::lower_contract_templates(db, top_mod)?);

    let mut functions = monomorphize_functions(db, templates);
    for func in &mut functions {
        crate::transform::canonicalize_transparent_newtypes(db, &mut func.body);
        crate::transform::insert_temp_binds(db, &mut func.body);
        crate::transform::canonicalize_zero_sized(db, &mut func.body);
    }
    Ok(MirModule { top_mod, functions })
}

/// Lowers a single HIR function (with its typed body) into a MIR function template.
///
/// # Parameters
/// - `db`: HIR analysis database.
/// - `func`: Function definition to lower.
/// - `typed_body`: Type-checked function body.
///
/// # Returns
/// The lowered MIR function template or an error when the function is missing a body.
pub(crate) fn lower_function<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: Func<'db>,
    typed_body: TypedBody<'db>,
    receiver_space: Option<AddressSpaceKind>,
    generic_args: Vec<TyId<'db>>,
    effect_param_space_overrides: Vec<Option<AddressSpaceKind>>,
) -> MirLowerResult<MirFunction<'db>> {
    let symbol_name = func
        .name(db)
        .to_opt()
        .map(|ident| ident.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into());
    let contract_function = extract_contract_function(db, func);

    let Some(body) = func.body(db) else {
        return Err(MirLowerError::MissingBody {
            func_name: symbol_name,
        });
    };

    let mut builder = MirBuilder::new_for_func(
        db,
        func,
        body,
        &typed_body,
        &generic_args,
        receiver_space,
        &effect_param_space_overrides,
    )?;
    let entry = builder.builder.entry_block();
    builder.move_to_block(entry);
    builder.lower_root(body.expr(db));
    builder.ensure_const_expr_values();
    if let Some(block) = builder.current_block() {
        let ret_ty = func.return_ty(db);
        let returns_value = !builder.is_unit_ty(ret_ty) && !ret_ty.is_never(db);
        if returns_value {
            let ret_val = builder.ensure_value(body.expr(db));
            builder.set_terminator(block, Terminator::Return(Some(ret_val)));
        } else {
            builder.set_terminator(block, Terminator::Return(None));
        }
    }
    let mir_body = builder.finish();

    if let Some(expr) = first_unlowered_expr_used_by_mir(&mir_body) {
        let expr_context = format_hir_expr_context(db, body, expr);
        // Generic functions are re-lowered from HIR during monomorphization, so their initial
        // templates are never codegen'd. Allow construction-time placeholders here.
        let is_uninstantiated_generic =
            generic_args.is_empty() && !CallableDef::Func(func).params(db).is_empty();
        if !is_uninstantiated_generic {
            return Err(MirLowerError::UnloweredHirExpr {
                func_name: symbol_name.clone(),
                expr: expr_context,
            });
        }
    }

    // Note: `MirFunction` may be used as a generic template during monomorphization.
    // Monomorphic instances get a fully-instantiated + normalized `ret_ty` in the
    // monomorphizer; this is the declared return type.
    let ret_ty = func.return_ty(db);
    let returns_value = !crate::layout::is_zero_sized_ty(db, ret_ty);

    Ok(MirFunction {
        origin: crate::ir::MirFunctionOrigin::Hir(func),
        body: mir_body,
        typed_body: Some(typed_body),
        generic_args,
        ret_ty,
        returns_value,
        contract_function,
        symbol_name,
        receiver_space,
    })
}

/// Stateful helper that incrementally constructs MIR while walking HIR.
pub(super) struct MirBuilder<'db, 'a> {
    pub(super) db: &'db dyn SpannedHirAnalysisDb,
    pub(super) hir_func: Option<Func<'db>>,
    pub(super) body: Body<'db>,
    pub(super) typed_body: &'a TypedBody<'db>,
    pub(super) generic_args: &'a [TyId<'db>],
    pub(super) return_ty: TyId<'db>,
    pub(super) builder: BodyBuilder<'db>,
    pub(super) core: CoreLib<'db>,
    pub(super) loop_stack: Vec<LoopScope>,
    pub(super) const_cache: FxHashMap<Const<'db>, ValueId>,
    pub(super) pat_address_space: FxHashMap<PatId, AddressSpaceKind>,
    pub(super) binding_locals: FxHashMap<LocalBinding<'db>, LocalId>,
    /// For methods, the address space variant being lowered.
    pub(super) receiver_space: Option<AddressSpaceKind>,
    /// Address space for each effect parameter, indexed by effect param position.
    pub(super) effect_param_spaces: Vec<AddressSpaceKind>,
    /// Address space overrides for effect bindings not tied to a function effect list.
    pub(super) effect_binding_spaces: FxHashMap<LocalBinding<'db>, AddressSpaceKind>,
}

/// Loop context capturing break/continue targets.
#[derive(Clone, Copy)]
pub(super) struct LoopScope {
    pub(super) continue_target: BasicBlockId,
    pub(super) break_target: BasicBlockId,
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Constructs a new builder for the given HIR body and typed information.
    ///
    /// # Parameters
    /// - `db`: HIR analysis database.
    /// - `body`: HIR body being lowered.
    /// - `typed_body`: Type-checked body information.
    ///
    /// # Returns
    /// A ready-to-use `MirBuilder` or an error if core helpers are missing.
    #[allow(clippy::too_many_arguments)]
    fn new(
        db: &'db dyn SpannedHirAnalysisDb,
        hir_func: Option<Func<'db>>,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        generic_args: &'a [TyId<'db>],
        return_ty: TyId<'db>,
        receiver_space: Option<AddressSpaceKind>,
        effect_param_space_overrides: &[Option<AddressSpaceKind>],
    ) -> Result<Self, MirLowerError> {
        let core = CoreLib::new(db, body.scope());

        let mut builder = Self {
            db,
            hir_func,
            body,
            typed_body,
            generic_args,
            return_ty,
            builder: BodyBuilder::new(),
            core,
            loop_stack: Vec::new(),
            const_cache: FxHashMap::default(),
            pat_address_space: FxHashMap::default(),
            binding_locals: FxHashMap::default(),
            receiver_space,
            effect_param_spaces: Vec::new(),
            effect_binding_spaces: FxHashMap::default(),
        };

        builder.effect_param_spaces = builder.compute_effect_param_spaces();
        for (idx, space) in effect_param_space_overrides.iter().enumerate() {
            if idx < builder.effect_param_spaces.len()
                && let Some(space) = *space
            {
                builder.effect_param_spaces[idx] = space;
            }
        }
        builder.seed_signature_locals();

        Ok(builder)
    }

    fn new_for_func(
        db: &'db dyn SpannedHirAnalysisDb,
        func: Func<'db>,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        generic_args: &'a [TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_param_space_overrides: &[Option<AddressSpaceKind>],
    ) -> Result<Self, MirLowerError> {
        let return_ty = func.return_ty(db);
        Self::new(
            db,
            Some(func),
            body,
            typed_body,
            generic_args,
            return_ty,
            receiver_space,
            effect_param_space_overrides,
        )
    }

    fn new_for_body_owner(
        db: &'db dyn SpannedHirAnalysisDb,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        generic_args: &'a [TyId<'db>],
        return_ty: TyId<'db>,
    ) -> Result<Self, MirLowerError> {
        Self::new(
            db,
            None,
            body,
            typed_body,
            generic_args,
            return_ty,
            None,
            &[],
        )
    }

    fn seed_synthetic_param_local(
        &mut self,
        name: String,
        ty: TyId<'db>,
        is_mut: bool,
        binding: Option<LocalBinding<'db>>,
    ) -> LocalId {
        let local = self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut,
            address_space: AddressSpaceKind::Memory,
        });
        self.builder.body.param_locals.push(local);
        if let Some(binding) = binding {
            self.binding_locals.insert(binding, local);
        }
        local
    }

    fn seed_synthetic_effect_param_local(
        &mut self,
        name: String,
        binding: LocalBinding<'db>,
        address_space: AddressSpaceKind,
    ) -> LocalId {
        let local = self.builder.body.alloc_local(LocalData {
            name,
            ty: self.u256_ty(),
            is_mut: binding.is_mut(),
            address_space,
        });
        self.builder.body.effect_param_locals.push(local);
        self.binding_locals.insert(binding, local);
        self.effect_binding_spaces.insert(binding, address_space);
        local
    }

    fn compute_effect_param_spaces(&self) -> Vec<AddressSpaceKind> {
        let Some(func) = self.hir_func else {
            return Vec::new();
        };
        let provider_arg_idx_by_effect =
            hir::analysis::ty::effects::place_effect_provider_param_index_map(self.db, func);

        let mut spaces = vec![AddressSpaceKind::Storage; func.effect_params(self.db).count()];
        for effect in func.effect_params(self.db) {
            let effect_idx = effect.index();
            if let Some(provider_arg_idx) = provider_arg_idx_by_effect
                .get(effect_idx)
                .copied()
                .flatten()
                && let Some(provider_ty) = self.generic_args.get(provider_arg_idx).copied()
            {
                if let Some(space) = self.effect_provider_space_for_provider_ty(provider_ty) {
                    spaces[effect_idx] = space;
                    continue;
                }

                // By-ref provider values are passed as pointers; default to memory so callers and
                // callees agree on the address space for projections.
                if matches!(
                    crate::repr::repr_kind_for_ty(self.db, &self.core, provider_ty),
                    crate::repr::ReprKind::Ref
                ) {
                    spaces[effect_idx] = AddressSpaceKind::Memory;
                    continue;
                }
            }

            if let Some(provider_ty) = self
                .contract_field_provider_ty_for_effect_site(EffectParamSite::Func(func), effect_idx)
                && let Some(space) = self.effect_provider_space_for_provider_ty(provider_ty)
            {
                spaces[effect_idx] = space;
            }
        }

        spaces
    }

    fn contract_field_provider_ty_for_effect_site(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<TyId<'db>> {
        let contract = match site {
            EffectParamSite::Func(func) => {
                let ItemKind::Contract(contract) = func.scope().parent_item(self.db)? else {
                    return None;
                };
                contract
            }
            EffectParamSite::Contract(contract)
            | EffectParamSite::ContractInit { contract }
            | EffectParamSite::ContractRecvArm { contract, .. } => contract,
        };

        let key_path = match site {
            EffectParamSite::Func(func) => func
                .effect_params(self.db)
                .nth(idx)
                .and_then(|effect| effect.key_path(self.db))?,
            EffectParamSite::Contract(contract) => contract
                .effect_params(self.db)
                .nth(idx)
                .and_then(|effect| effect.key_path(self.db))?,
            EffectParamSite::ContractInit { contract } => contract
                .init(self.db)?
                .effects(self.db)
                .data(self.db)
                .get(idx)?
                .key_path
                .to_opt()?,
            EffectParamSite::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract
                .recv_arm(self.db, recv_idx as usize, arm_idx as usize)?
                .effects
                .data(self.db)
                .get(idx)?
                .key_path
                .to_opt()?,
        };

        if key_path.len(self.db) != 1 {
            return None;
        }
        let field_name = key_path.ident(self.db).to_opt()?;
        let field = contract.fields(self.db).get(&field_name)?;
        field.is_provider.then_some(field.declared_ty)
    }

    /// Consumes the builder and returns the accumulated MIR body.
    ///
    /// # Returns
    /// The completed `MirBody`.
    fn finish(self) -> MirBody<'db> {
        let mut body = self.builder.build();
        body.pat_address_space = self.pat_address_space;
        body
    }

    /// Allocates and returns a fresh basic block.
    ///
    /// # Returns
    /// The identifier for the newly created block.
    fn alloc_block(&mut self) -> BasicBlockId {
        self.builder.make_block()
    }

    fn current_block(&self) -> Option<BasicBlockId> {
        self.builder.current_block()
    }

    fn move_to_block(&mut self, block: BasicBlockId) {
        self.builder.move_to_block(block);
    }

    /// Sets the terminator for the specified block.
    ///
    /// # Parameters
    /// - `block`: Target basic block.
    /// - `term`: Terminator to assign.
    fn set_terminator(&mut self, block: BasicBlockId, term: Terminator<'db>) {
        self.builder.set_block_terminator(block, term);
    }

    fn set_current_terminator(&mut self, term: Terminator<'db>) {
        self.builder.terminate_current(term);
    }

    fn goto(&mut self, target: BasicBlockId) {
        self.set_current_terminator(Terminator::Goto { target });
    }

    fn branch(&mut self, cond: ValueId, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        self.set_current_terminator(Terminator::Branch {
            cond,
            then_bb,
            else_bb,
        });
    }

    fn switch(&mut self, discr: ValueId, targets: Vec<SwitchTarget>, default: BasicBlockId) {
        self.set_current_terminator(Terminator::Switch {
            discr,
            targets,
            default,
        });
    }

    pub(super) fn alloc_temp_local(&mut self, ty: TyId<'db>, is_mut: bool, hint: &str) -> LocalId {
        let idx = self.builder.body.locals.len();
        let name = format!("tmp_{hint}{idx}");
        self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut,
            address_space: AddressSpaceKind::Memory,
        })
    }

    /// Appends an instruction to the given block.
    ///
    /// # Parameters
    /// - `block`: Block receiving the instruction.
    /// - `inst`: Instruction to append.
    fn push_inst(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.builder.push_inst_in(block, inst);
    }

    fn push_inst_here(&mut self, inst: MirInst<'db>) {
        if let Some(block) = self.current_block() {
            self.push_inst(block, inst);
        }
    }

    fn assign(&mut self, stmt: Option<StmtId>, dest: Option<LocalId>, rvalue: Rvalue<'db>) {
        self.push_inst_here(MirInst::Assign { stmt, dest, rvalue });
    }

    fn alloc_value(&mut self, ty: TyId<'db>, origin: ValueOrigin<'db>, repr: ValueRepr) -> ValueId {
        self.builder
            .body
            .alloc_value(ValueData { ty, origin, repr })
    }

    /// Determines the address space for a binding.
    ///
    /// # Parameters
    /// - `binding`: Binding metadata.
    ///
    /// # Returns
    /// The resolved address space kind.
    pub(super) fn address_space_for_binding(
        &self,
        binding: &LocalBinding<'db>,
    ) -> AddressSpaceKind {
        match binding {
            LocalBinding::EffectParam { site, idx, .. } => {
                if let Some(space) = self.effect_binding_spaces.get(binding).copied() {
                    return space;
                }
                if let Some(func) = self.hir_func
                    && matches!(site, EffectParamSite::Func(site_func) if *site_func == func)
                {
                    return self
                        .effect_param_spaces
                        .get(*idx)
                        .copied()
                        .unwrap_or(AddressSpaceKind::Storage);
                }
                AddressSpaceKind::Storage
            }
            LocalBinding::Local { pat, .. } => self
                .pat_address_space
                .get(pat)
                .copied()
                .unwrap_or(AddressSpaceKind::Memory),
            LocalBinding::Param { site, idx, .. } => match site {
                ParamSite::Func(_) => {
                    if *idx == 0 {
                        return self.receiver_space.unwrap_or(AddressSpaceKind::Memory);
                    }
                    AddressSpaceKind::Memory
                }
                ParamSite::ContractInit(_) => AddressSpaceKind::Memory,
                ParamSite::EffectField(effect_site) => match effect_site {
                    _ if self.effect_binding_spaces.contains_key(binding) => self
                        .effect_binding_spaces
                        .get(binding)
                        .copied()
                        .unwrap_or(AddressSpaceKind::Storage),
                    EffectParamSite::Func(effect_func)
                        if self.hir_func.is_some_and(|current| current == *effect_func) =>
                    {
                        self.effect_param_spaces
                            .get(*idx)
                            .copied()
                            .unwrap_or(AddressSpaceKind::Storage)
                    }
                    _ => AddressSpaceKind::Storage,
                },
            },
        }
    }

    /// Computes the address space for an expression, defaulting to memory.
    ///
    /// # Parameters
    /// - `expr`: Expression id to inspect.
    ///
    /// # Returns
    /// The address space kind for the expression.
    pub(super) fn expr_address_space(&self, expr: ExprId) -> AddressSpaceKind {
        // Propagate storage space through projections so nested fields continue to be treated as
        // storage pointers.
        let exprs = self.body.exprs(self.db);
        if let Partial::Present(expr_data) = &exprs[expr] {
            match expr_data {
                Expr::Field(base, _) => {
                    let base_space = self.expr_address_space(*base);
                    if !matches!(base_space, AddressSpaceKind::Memory) {
                        return base_space;
                    }
                }
                Expr::Bin(base, _, BinOp::Index) => {
                    let base_space = self.expr_address_space(*base);
                    if !matches!(base_space, AddressSpaceKind::Memory) {
                        return base_space;
                    }
                }
                _ => {}
            }
        }

        let prop = self.typed_body.expr_prop(self.db, expr);
        if let Some(binding) = prop.binding {
            self.address_space_for_binding(&binding)
        } else {
            AddressSpaceKind::Memory
        }
    }

    pub(super) fn u256_ty(&self) -> TyId<'db> {
        TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)))
    }

    /// Returns `true` when the given type is represented by-reference in MIR.
    ///
    /// Fe MIR represents user aggregates (structs/tuples/arrays/enums) as pointers into an address
    /// space. Effect pointer provider newtypes (`MemPtr`/`StorPtr`/`CalldataPtr`) are *not*
    /// represented by-reference: they are single-word values at runtime.
    pub(super) fn is_by_ref_ty(&self, ty: TyId<'db>) -> bool {
        matches!(
            crate::repr::repr_kind_for_ty(self.db, &self.core, ty),
            crate::repr::ReprKind::Ref
        )
    }

    pub(super) fn value_repr_for_expr(&self, expr: ExprId, ty: TyId<'db>) -> ValueRepr {
        match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
            crate::repr::ReprKind::Ptr(space) => ValueRepr::Ptr(space),
            crate::repr::ReprKind::Ref => ValueRepr::Ref(self.expr_address_space(expr)),
            crate::repr::ReprKind::Zst | crate::repr::ReprKind::Word => ValueRepr::Word,
        }
    }

    pub(super) fn value_repr_for_ty(&self, ty: TyId<'db>, space: AddressSpaceKind) -> ValueRepr {
        match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
            crate::repr::ReprKind::Ptr(space) => ValueRepr::Ptr(space),
            crate::repr::ReprKind::Ref => ValueRepr::Ref(space),
            crate::repr::ReprKind::Zst | crate::repr::ReprKind::Word => ValueRepr::Word,
        }
    }

    fn project_tuple_elem_value(
        &mut self,
        tuple_value: ValueId,
        tuple_ty: TyId<'db>,
        field_idx: usize,
        field_ty: TyId<'db>,
    ) -> ValueId {
        // Transparent newtype access: field 0 is a representation-preserving cast.
        if field_idx == 0 && crate::repr::transparent_newtype_field_ty(self.db, tuple_ty).is_some()
        {
            let base_repr = self.builder.body.value(tuple_value).repr;
            if !base_repr.is_ref() {
                let space = base_repr
                    .address_space()
                    .unwrap_or(AddressSpaceKind::Memory);
                return self.alloc_value(
                    field_ty,
                    ValueOrigin::TransparentCast { value: tuple_value },
                    self.value_repr_for_ty(field_ty, space),
                );
            }
        }

        let base_space = self.value_address_space(tuple_value);
        let place = Place::new(
            tuple_value,
            MirProjectionPath::from_projection(hir::projection::Projection::Field(field_idx)),
        );
        if self.is_by_ref_ty(field_ty) {
            return self.alloc_value(
                field_ty,
                ValueOrigin::PlaceRef(place),
                ValueRepr::Ref(base_space),
            );
        }
        let dest = self.alloc_temp_local(field_ty, false, "arg");
        self.builder.body.locals[dest.index()].address_space = AddressSpaceKind::Memory;
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.alloc_value(field_ty, ValueOrigin::Local(dest), ValueRepr::Word)
    }

    fn bind_pat_value(&mut self, pat: PatId, value: ValueId) {
        let Some(block) = self.current_block() else {
            return;
        };
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return;
        };

        match pat_data {
            Pat::WildCard | Pat::Rest => {}
            Pat::Path(_, is_mut) => {
                let binding = self
                    .typed_body
                    .pat_binding(pat)
                    .unwrap_or(LocalBinding::Local {
                        pat,
                        is_mut: *is_mut,
                    });
                let Some(local) = self.local_for_binding(binding) else {
                    return;
                };
                self.move_to_block(block);
                self.assign(None, Some(local), Rvalue::Value(value));
                let pat_ty = self.typed_body.pat_ty(self.db, pat);
                if self
                    .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                    .address_space()
                    .is_some()
                {
                    let space = self.value_address_space(value);
                    self.set_pat_address_space(pat, space);
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                let owner_ty = self.typed_body.pat_ty(self.db, pat);
                let owner_by_ref = self.is_by_ref_ty(owner_ty);
                let tuple_repr = self.builder.body.value(value).repr;
                for (idx, field_pat) in pats.iter().enumerate() {
                    if !owner_by_ref && idx != 0 {
                        continue;
                    }
                    let Partial::Present(field_pat_data) = field_pat.data(self.db, self.body)
                    else {
                        continue;
                    };
                    if matches!(field_pat_data, Pat::WildCard | Pat::Rest) {
                        continue;
                    }
                    let field_ty = self.typed_body.pat_ty(self.db, *field_pat);
                    let field_value = if owner_by_ref {
                        self.project_tuple_elem_value(value, owner_ty, idx, field_ty)
                    } else {
                        self.alloc_value(
                            field_ty,
                            ValueOrigin::TransparentCast { value },
                            tuple_repr,
                        )
                    };
                    self.bind_pat_value(*field_pat, field_value);
                    if self.current_block().is_none() {
                        break;
                    }
                }
            }
            Pat::Record(_, fields) => {
                let owner_ty = self.typed_body.pat_ty(self.db, pat);
                let owner_by_ref = self.is_by_ref_ty(owner_ty);
                let record_repr = self.builder.body.value(value).repr;
                let base_space = self.value_address_space(value);
                for field in fields {
                    let Some(label) = field.label(self.db, self.body) else {
                        continue;
                    };
                    let Some(info) = self.field_access_info(owner_ty, FieldIndex::Ident(label))
                    else {
                        continue;
                    };
                    if !owner_by_ref && info.field_idx != 0 {
                        continue;
                    }
                    let field_ty = self.typed_body.pat_ty(self.db, field.pat);
                    let field_value = if owner_by_ref {
                        if self
                            .value_repr_for_ty(field_ty, AddressSpaceKind::Memory)
                            .address_space()
                            .is_some()
                        {
                            self.set_pat_address_space(field.pat, base_space);
                        }
                        self.project_tuple_elem_value(value, owner_ty, info.field_idx, field_ty)
                    } else {
                        self.alloc_value(
                            field_ty,
                            ValueOrigin::TransparentCast { value },
                            record_repr,
                        )
                    };
                    self.bind_pat_value(field.pat, field_value);
                    if self.current_block().is_none() {
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    fn seed_signature_locals(&mut self) {
        let Some(func) = self.hir_func else {
            return;
        };
        for (idx, param) in func.params(self.db).enumerate() {
            let binding = self
                .typed_body
                .param_binding(idx)
                .unwrap_or(LocalBinding::Param {
                    site: ParamSite::Func(func),
                    idx,
                    ty: param.ty(self.db),
                    is_mut: param.is_mut(self.db),
                });
            let name = param
                .name(self.db)
                .map(|ident| ident.data(self.db).to_string())
                .unwrap_or_else(|| format!("arg{idx}"));
            let ty = match binding {
                LocalBinding::Param { ty, .. } => ty,
                _ => param.ty(self.db),
            };
            let local = self.builder.body.alloc_local(LocalData {
                name,
                ty,
                is_mut: binding.is_mut(),
                address_space: self.address_space_for_binding(&binding),
            });
            self.builder.body.param_locals.push(local);
            self.binding_locals.insert(binding, local);
        }

        for effect in func.effect_params(self.db) {
            let idx = effect.index();
            let Some(key_path) = effect.key_path(self.db) else {
                continue;
            };
            let binding = LocalBinding::EffectParam {
                site: EffectParamSite::Func(func),
                idx,
                key_path,
                is_mut: effect.is_mut(self.db),
            };
            let name = effect
                .name(self.db)
                .map(|ident| ident.data(self.db).to_string())
                .or_else(|| {
                    key_path
                        .ident(self.db)
                        .to_opt()
                        .map(|ident| ident.data(self.db).to_string())
                })
                .unwrap_or_else(|| format!("effect{idx}"));
            let local = self.builder.body.alloc_local(LocalData {
                name,
                ty: self.u256_ty(),
                is_mut: binding.is_mut(),
                address_space: self.address_space_for_binding(&binding),
            });
            self.builder.body.effect_param_locals.push(local);
            self.binding_locals.insert(binding, local);
        }
    }

    pub(super) fn local_for_binding(&mut self, binding: LocalBinding<'db>) -> Option<LocalId> {
        if let Some(&local) = self.binding_locals.get(&binding) {
            return Some(local);
        }
        let needs_effect_param_local = matches!(
            binding,
            LocalBinding::EffectParam {
                site: EffectParamSite::Contract(_)
                    | EffectParamSite::ContractInit { .. }
                    | EffectParamSite::ContractRecvArm { .. },
                ..
            }
        );
        if let LocalBinding::Param {
            site: ParamSite::EffectField(effect_site),
            idx,
            ..
        } = binding
            && let Some(current) = self.hir_func
            && matches!(effect_site, EffectParamSite::Func(func) if func == current)
            && let Some(&local) = self.builder.body.effect_param_locals.get(idx)
        {
            self.binding_locals.insert(binding, local);
            return Some(local);
        }
        let name = self.binding_name(binding)?;
        let (ty, is_mut) = match binding {
            LocalBinding::Local { pat, is_mut } => (self.typed_body.pat_ty(self.db, pat), is_mut),
            LocalBinding::Param { ty, is_mut, .. } => (ty, is_mut),
            LocalBinding::EffectParam { is_mut, .. } => (self.u256_ty(), is_mut),
        };
        let local = self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut,
            address_space: self.address_space_for_binding(&binding),
        });
        if needs_effect_param_local {
            self.builder.body.effect_param_locals.push(local);
        }
        self.binding_locals.insert(binding, local);
        Some(local)
    }

    pub(super) fn binding_name(&self, binding: LocalBinding<'db>) -> Option<String> {
        match binding {
            LocalBinding::Local { pat, .. } => match pat.data(self.db, self.body) {
                Partial::Present(Pat::Path(path, _)) => path
                    .to_opt()
                    .and_then(|path| path.as_ident(self.db))
                    .map(|ident| ident.data(self.db).to_string()),
                _ => None,
            },
            LocalBinding::Param { site, idx, .. } => match site {
                ParamSite::Func(func) => func
                    .params(self.db)
                    .nth(idx)
                    .and_then(|param| param.name(self.db))
                    .map(|ident| ident.data(self.db).to_string())
                    .or_else(|| Some(format!("arg{idx}"))),
                ParamSite::ContractInit(contract) => contract
                    .init(self.db)?
                    .params(self.db)
                    .data(self.db)
                    .get(idx)
                    .and_then(|param| param.name())
                    .map(|ident| ident.data(self.db).to_string())
                    .or_else(|| Some(format!("arg{idx}"))),
                ParamSite::EffectField(effect_site) => {
                    let name = match effect_site {
                        EffectParamSite::Func(func) => func
                            .effect_params(self.db)
                            .nth(idx)
                            .and_then(|effect| effect.name(self.db)),
                        EffectParamSite::Contract(contract) => contract
                            .effects(self.db)
                            .data(self.db)
                            .get(idx)
                            .and_then(|effect| effect.name),
                        EffectParamSite::ContractInit { contract } => contract
                            .init(self.db)?
                            .effects(self.db)
                            .data(self.db)
                            .get(idx)
                            .and_then(|effect| effect.name),
                        EffectParamSite::ContractRecvArm {
                            contract,
                            recv_idx,
                            arm_idx,
                        } => contract
                            .recv_arm(self.db, recv_idx as usize, arm_idx as usize)?
                            .effects
                            .data(self.db)
                            .get(idx)
                            .and_then(|effect| effect.name),
                    };
                    name.map(|ident| ident.data(self.db).to_string())
                        .or_else(|| Some(format!("effect_field{idx}")))
                }
            },
            LocalBinding::EffectParam {
                site,
                idx,
                key_path,
                ..
            } => {
                let explicit = match site {
                    EffectParamSite::Func(func) => func
                        .effect_params(self.db)
                        .nth(idx)
                        .and_then(|effect| effect.name(self.db))
                        .map(|ident| ident.data(self.db).to_string()),
                    EffectParamSite::Contract(contract) => contract
                        .effects(self.db)
                        .data(self.db)
                        .get(idx)
                        .and_then(|effect| effect.name)
                        .map(|ident| ident.data(self.db).to_string()),
                    EffectParamSite::ContractInit { contract } => contract
                        .init(self.db)?
                        .effects(self.db)
                        .data(self.db)
                        .get(idx)
                        .and_then(|effect| effect.name)
                        .map(|ident| ident.data(self.db).to_string()),
                    EffectParamSite::ContractRecvArm {
                        contract,
                        recv_idx,
                        arm_idx,
                    } => contract
                        .recv_arm(self.db, recv_idx as usize, arm_idx as usize)?
                        .effects
                        .data(self.db)
                        .get(idx)
                        .and_then(|effect| effect.name)
                        .map(|ident| ident.data(self.db).to_string()),
                };
                explicit.or_else(|| {
                    key_path
                        .ident(self.db)
                        .to_opt()
                        .map(|ident| ident.data(self.db).to_string())
                })
            }
        }
    }

    pub(super) fn binding_value(&mut self, binding: LocalBinding<'db>) -> Option<ValueId> {
        let local = self.local_for_binding(binding)?;
        let value_id = self.builder.body.alloc_value(ValueData {
            ty: self.u256_ty(),
            origin: ValueOrigin::Local(local),
            repr: ValueRepr::Word,
        });
        Some(value_id)
    }

    pub(super) fn effect_provider_space_for_provider_ty(
        &self,
        provider_ty: TyId<'db>,
    ) -> Option<AddressSpaceKind> {
        crate::repr::effect_provider_space_for_ty(self.db, &self.core, provider_ty)
    }

    /// Determines the address space associated with a MIR value.
    ///
    /// This is used when lowering projections and effect arguments that need to know whether a
    /// pointer-like value is addressing memory or storage.
    pub(super) fn value_address_space(&self, value: ValueId) -> AddressSpaceKind {
        self.builder.body.value_address_space(value)
    }

    /// Associates a pattern with an address space.
    ///
    /// # Parameters
    /// - `pat`: Pattern id to annotate.
    /// - `space`: Address space kind to record.
    pub(super) fn set_pat_address_space(&mut self, pat: PatId, space: AddressSpaceKind) {
        self.pat_address_space.insert(pat, space);
    }
}

fn format_hir_expr_context(db: &dyn SpannedHirAnalysisDb, body: Body<'_>, expr: ExprId) -> String {
    let span = expr.span(body).resolve(db);
    let span_context = if let Some(span) = span {
        let path = span
            .file
            .path(db)
            .as_ref()
            .map(|p| p.to_string())
            .unwrap_or_else(|| "<unknown file>".into());
        let start: usize = u32::from(span.range.start()) as usize;
        let text = span.file.text(db);
        let (mut line, mut col) = (1usize, 1usize);
        for byte in text.as_bytes().iter().take(start) {
            if *byte == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        format!("{path}:{line}:{col}")
    } else {
        "<no span>".into()
    };

    let expr_data = match expr.data(db, body) {
        Partial::Present(expr_data) => match expr_data {
            Expr::Path(path) => path
                .to_opt()
                .map(|path| format!("Path({})", path.pretty_print(db)))
                .unwrap_or_else(|| "Path(<absent>)".into()),
            Expr::Call(callee, args) => {
                let callee_data = match callee.data(db, body) {
                    Partial::Present(Expr::Path(path)) => path
                        .to_opt()
                        .map(|path| format!("Path({})", path.pretty_print(db)))
                        .unwrap_or_else(|| "Path(<absent>)".into()),
                    Partial::Present(other) => format!("{other:?}"),
                    Partial::Absent => "<absent>".into(),
                };
                format!("Call({callee:?} {callee_data}, {args:?})")
            }
            Expr::MethodCall(receiver, method, _, args) => {
                let method_name = method
                    .to_opt()
                    .map(|id| id.data(db).to_string())
                    .unwrap_or_else(|| "<absent>".into());
                format!("MethodCall({receiver:?}, {method_name}, {args:?})")
            }
            other => format!("{other:?}"),
        },
        Partial::Absent => "<absent>".into(),
    };

    format!("expr={expr:?} at {span_context}: {expr_data}")
}

fn first_unlowered_expr_used_by_mir<'db>(body: &MirBody<'db>) -> Option<ExprId> {
    let mut used_values: FxHashSet<ValueId> = FxHashSet::default();

    for block in &body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::Assign { rvalue, .. } => match rvalue {
                    crate::ir::Rvalue::ZeroInit => {}
                    crate::ir::Rvalue::Value(value) => {
                        used_values.insert(*value);
                    }
                    crate::ir::Rvalue::Call(call) => {
                        used_values.extend(call.args.iter().copied());
                        used_values.extend(call.effect_args.iter().copied());
                    }
                    crate::ir::Rvalue::Intrinsic { args, .. } => {
                        used_values.extend(args.iter().copied());
                    }
                    crate::ir::Rvalue::Load { place } => {
                        used_values.insert(place.base);
                        used_values.extend(dynamic_indices(&place.projection));
                    }
                    crate::ir::Rvalue::Alloc { .. } => {}
                },
                MirInst::BindValue { value } => {
                    used_values.insert(*value);
                }
                MirInst::Store { place, value } => {
                    used_values.insert(place.base);
                    used_values.insert(*value);
                    used_values.extend(dynamic_indices(&place.projection));
                }
                MirInst::InitAggregate { place, inits } => {
                    used_values.insert(place.base);
                    used_values.extend(dynamic_indices(&place.projection));
                    for (path, value) in inits {
                        used_values.extend(dynamic_indices(path));
                        used_values.insert(*value);
                    }
                }
                MirInst::SetDiscriminant { place, .. } => {
                    used_values.insert(place.base);
                    used_values.extend(dynamic_indices(&place.projection));
                }
            }
        }

        match &block.terminator {
            Terminator::Return(Some(value)) => {
                used_values.insert(*value);
            }
            Terminator::TerminatingCall(call) => match call {
                crate::ir::TerminatingCall::Call(call) => {
                    used_values.extend(call.args.iter().copied());
                    used_values.extend(call.effect_args.iter().copied());
                }
                crate::ir::TerminatingCall::Intrinsic { args, .. } => {
                    used_values.extend(args.iter().copied());
                }
            },
            Terminator::Branch { cond, .. } => {
                used_values.insert(*cond);
            }
            Terminator::Switch { discr, .. } => {
                used_values.insert(*discr);
            }
            Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
        }
    }

    let mut worklist: Vec<ValueId> = used_values.into_iter().collect();
    let mut visited: FxHashSet<ValueId> = FxHashSet::default();

    while let Some(value_id) = worklist.pop() {
        if !visited.insert(value_id) {
            continue;
        }

        match &body.value(value_id).origin {
            ValueOrigin::Expr(expr) => return Some(*expr),
            ValueOrigin::Unary { inner, .. } => worklist.push(*inner),
            ValueOrigin::Binary { lhs, rhs, .. } => {
                worklist.push(*lhs);
                worklist.push(*rhs);
            }
            ValueOrigin::TransparentCast { value } => worklist.push(*value),
            _ => {}
        }
    }

    None
}

fn dynamic_indices<'db, 'a>(
    path: &'a MirProjectionPath<'db>,
) -> impl Iterator<Item = ValueId> + 'a {
    path.iter().filter_map(|proj| match proj {
        hir::projection::Projection::Index(hir::projection::IndexSource::Dynamic(value_id)) => {
            Some(*value_id)
        }
        _ => None,
    })
}
