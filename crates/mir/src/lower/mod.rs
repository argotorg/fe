//! MIR lowering entrypoints and shared builder scaffolding. Dispatches to submodules that handle
//! expression lowering, intrinsics, matches, aggregates (records/variants), layout, and contract
//! metadata.

use std::{error::Error, fmt};

use common::ingot::IngotKind;
use hir::analysis::{
    HirAnalysisDb,
    diagnostics::SpannedHirAnalysisDb,
    name_resolution::{
        PathRes,
        path_resolver::{ResolvedVariant, resolve_path},
    },
    ty::{
        adt_def::AdtRef,
        trait_resolution::PredicateListId,
        ty_check::{
            EffectParamSite, LocalBinding, ParamSite, RecordLike, TypedBody, check_func_body,
        },
        ty_def::{PrimTy, TyBase, TyData, TyId},
        ty_lower::lower_opt_hir_ty,
    },
};
use hir::hir_def::{
    Attr, AttrArg, AttrArgValue, Body, CallableDef, Const, Expr, ExprId, Field, FieldIndex, Func,
    ItemKind, LitKind, MatchArm, Partial, Pat, PatId, PathId, Stmt, StmtId, TopLevelMod,
    VariantKind, expr::BinOp, scope_graph::ScopeId,
};

use crate::{
    core_lib::{CoreHelperTy, CoreLib, CoreLibError},
    ir::{
        AddressSpaceKind, BasicBlockId, BodyBuilder, CallOrigin, CodeRegionRoot, ContractFunction,
        ContractFunctionKind, EffectProviderKind, IntrinsicOp, LocalData, LocalId, LoopInfo,
        MirBody, MirFunction, MirInst, MirModule, MirProjection, MirProjectionPath, Place,
        SwitchTarget, SwitchValue, SyntheticValue, Terminator, ValueData, ValueId, ValueOrigin,
    },
    monomorphize::monomorphize_functions,
};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

mod aggregates;
mod contract;
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
    MissingCoreHelper {
        path: String,
    },
    AnalysisDiagnostics {
        func_name: String,
        diagnostics: String,
    },
    UnloweredHirExpr {
        func_name: String,
        expr: String,
    },
}

impl fmt::Display for MirLowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirLowerError::MissingBody { func_name } => {
                write!(f, "function `{func_name}` is missing a body")
            }
            MirLowerError::MissingCoreHelper { path } => {
                write!(f, "missing required core helper `{path}`")
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
        }
    }
}

impl From<CoreLibError> for MirLowerError {
    fn from(err: CoreLibError) -> Self {
        match err {
            CoreLibError::MissingFunc(path) | CoreLibError::MissingType(path) => {
                MirLowerError::MissingCoreHelper { path }
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

    for &func in top_mod.all_funcs(db) {
        queue_func(func);
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
        let default_effects = vec![EffectProviderKind::Storage; func.effect_params(db).count()];
        let lowered = lower_function(db, func, typed_body.clone(), None, default_effects)?;
        templates.push(lowered);
    }

    let mut functions = monomorphize_functions(db, templates);
    for func in &mut functions {
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
    effect_provider_kinds: Vec<EffectProviderKind>,
) -> MirLowerResult<MirFunction<'db>> {
    let symbol_name = func
        .name(db)
        .to_opt()
        .map(|ident| ident.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into());

    let Some(body) = func.body(db) else {
        return Err(MirLowerError::MissingBody {
            func_name: symbol_name,
        });
    };

    let effect_provider_kinds_for_func = effect_provider_kinds.clone();
    let mut builder = MirBuilder::new(
        db,
        func,
        body,
        &typed_body,
        receiver_space,
        effect_provider_kinds,
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
        return Err(MirLowerError::UnloweredHirExpr {
            func_name: symbol_name.clone(),
            expr: expr_context,
        });
    }

    Ok(MirFunction {
        func,
        body: mir_body,
        typed_body,
        generic_args: Vec::new(),
        effect_provider_kinds: effect_provider_kinds_for_func,
        contract_function: extract_contract_function(db, func),
        symbol_name,
        receiver_space,
    })
}

/// Stateful helper that incrementally constructs MIR while walking HIR.
pub(super) struct MirBuilder<'db, 'a> {
    pub(super) db: &'db dyn SpannedHirAnalysisDb,
    pub(super) func: Func<'db>,
    pub(super) body: Body<'db>,
    pub(super) typed_body: &'a TypedBody<'db>,
    pub(super) builder: BodyBuilder<'db>,
    pub(super) core: CoreLib<'db>,
    pub(super) loop_stack: Vec<LoopScope>,
    pub(super) const_cache: FxHashMap<Const<'db>, ValueId>,
    pub(super) pat_address_space: FxHashMap<PatId, AddressSpaceKind>,
    pub(super) value_address_space: FxHashMap<ValueId, AddressSpaceKind>,
    pub(super) binding_locals: FxHashMap<LocalBinding<'db>, LocalId>,
    /// For methods, the address space variant being lowered.
    pub(super) receiver_space: Option<AddressSpaceKind>,
    pub(super) effect_provider_kinds: Vec<EffectProviderKind>,
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
    fn new(
        db: &'db dyn SpannedHirAnalysisDb,
        func: Func<'db>,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        receiver_space: Option<AddressSpaceKind>,
        effect_provider_kinds: Vec<EffectProviderKind>,
    ) -> Result<Self, MirLowerError> {
        let core = CoreLib::new(db, body)?;

        let mut builder = Self {
            db,
            func,
            body,
            typed_body,
            builder: BodyBuilder::new(),
            core,
            loop_stack: Vec::new(),
            const_cache: FxHashMap::default(),
            pat_address_space: FxHashMap::default(),
            value_address_space: FxHashMap::default(),
            binding_locals: FxHashMap::default(),
            receiver_space,
            effect_provider_kinds,
        };

        builder.seed_signature_locals();

        Ok(builder)
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
            LocalBinding::EffectParam { idx, .. } => match self
                .effect_provider_kinds
                .get(*idx)
                .copied()
                .unwrap_or(EffectProviderKind::Storage)
            {
                EffectProviderKind::Memory => AddressSpaceKind::Memory,
                EffectProviderKind::Storage => AddressSpaceKind::Storage,
                EffectProviderKind::Calldata => AddressSpaceKind::Memory,
            },
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
                ParamSite::EffectField(_) => match self
                    .effect_provider_kinds
                    .get(*idx)
                    .copied()
                    .unwrap_or(EffectProviderKind::Storage)
                {
                    EffectProviderKind::Memory => AddressSpaceKind::Memory,
                    EffectProviderKind::Storage => AddressSpaceKind::Storage,
                    EffectProviderKind::Calldata => AddressSpaceKind::Memory,
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
        // Propagate storage space through field projections so nested storage fields
        // continue to be treated as storage pointers.
        let exprs = self.body.exprs(self.db);
        if let Partial::Present(expr_data) = &exprs[expr] {
            match expr_data {
                Expr::Field(base, _) => {
                    let base_space = self.expr_address_space(*base);
                    if matches!(base_space, AddressSpaceKind::Storage) {
                        return AddressSpaceKind::Storage;
                    }
                }
                Expr::Bin(base, _, BinOp::Index) => {
                    let base_space = self.expr_address_space(*base);
                    if matches!(base_space, AddressSpaceKind::Storage) {
                        return AddressSpaceKind::Storage;
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

    fn seed_signature_locals(&mut self) {
        for (idx, param) in self.func.params(self.db).enumerate() {
            let binding = self
                .typed_body
                .param_binding(idx)
                .unwrap_or(LocalBinding::Param {
                    site: ParamSite::Func(self.func),
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

        for effect in self.func.effect_params(self.db) {
            let idx = effect.index();
            let Some(key_path) = effect.key_path(self.db) else {
                continue;
            };
            let binding = LocalBinding::EffectParam {
                site: EffectParamSite::Func(self.func),
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
                site: EffectParamSite::Contract(_) | EffectParamSite::ContractRecvArm { .. },
                ..
            }
        );
        if let LocalBinding::Param {
            site: ParamSite::EffectField(effect_site),
            idx,
            ..
        } = binding
            && matches!(effect_site, EffectParamSite::Func(func) if func == self.func)
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
        });
        Some(value_id)
    }

    pub(super) fn effect_provider_kind_for_binding(
        &self,
        binding: LocalBinding<'db>,
    ) -> EffectProviderKind {
        match binding {
            LocalBinding::EffectParam { idx, .. } => self
                .effect_provider_kinds
                .get(idx)
                .copied()
                .unwrap_or(EffectProviderKind::Storage),
            LocalBinding::Param { ty, .. } => self
                .effect_provider_kind_for_provider_ty(ty)
                .unwrap_or(EffectProviderKind::Storage),
            LocalBinding::Local { pat, .. } => {
                let ty = self.typed_body.pat_ty(self.db, pat);
                self.effect_provider_kind_for_provider_ty(ty)
                    .unwrap_or(EffectProviderKind::Storage)
            }
        }
    }

    pub(super) fn effect_provider_kind_for_address_space(
        &self,
        space: AddressSpaceKind,
    ) -> EffectProviderKind {
        match space {
            AddressSpaceKind::Memory => EffectProviderKind::Memory,
            AddressSpaceKind::Storage => EffectProviderKind::Storage,
        }
    }

    pub(super) fn effect_provider_kind_for_provider_ty(
        &self,
        provider_ty: TyId<'db>,
    ) -> Option<EffectProviderKind> {
        let base_ty = provider_ty.base_ty(self.db);
        if let Some(mem_base) = self
            .core
            .helper_ty(CoreHelperTy::EffectMemPtr)
            .map(|ty| ty.base_ty(self.db))
        {
            if base_ty == mem_base {
                return Some(EffectProviderKind::Memory);
            }
        }
        if let Some(stor_base) = self
            .core
            .helper_ty(CoreHelperTy::EffectStorPtr)
            .map(|ty| ty.base_ty(self.db))
        {
            if base_ty == stor_base {
                return Some(EffectProviderKind::Storage);
            }
        }
        if let Some(calldata_base) = self
            .core
            .helper_ty(CoreHelperTy::EffectCalldataPtr)
            .map(|ty| ty.base_ty(self.db))
        {
            if base_ty == calldata_base {
                return Some(EffectProviderKind::Calldata);
            }
        }
        None
    }

    /// Records the address space for a newly allocated value derived from an expression.
    ///
    /// # Parameters
    /// - `expr`: Source expression.
    /// - `value`: Newly allocated value id.
    pub(super) fn record_value_address_space(&mut self, expr: ExprId, value: ValueId) {
        let space = self.expr_address_space(expr);
        self.value_address_space.entry(value).or_insert(space);
    }

    /// Returns the address space for a value, defaulting to memory.
    ///
    /// # Parameters
    /// - `value`: Value id to query.
    ///
    /// # Returns
    /// The recorded address space kind.
    pub(super) fn value_address_space(&self, value: ValueId) -> AddressSpaceKind {
        self.value_address_space
            .get(&value)
            .copied()
            .unwrap_or(AddressSpaceKind::Memory)
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
        Partial::Present(expr) => format!("{expr:?}"),
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

    for value_id in used_values {
        if let ValueOrigin::Expr(expr) = &body.value(value_id).origin {
            return Some(*expr);
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
