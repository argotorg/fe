//! MIR lowering entrypoints and shared builder scaffolding. Dispatches to submodules that handle
//! expression lowering, intrinsics, matches, aggregates (records/variants), layout, and contract
//! metadata.

use std::{error::Error, fmt};

use common::ingot::IngotKind;
use hir::analysis::{
    HirAnalysisDb,
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
        AddressSpaceKind, BasicBlock, BasicBlockId, CallOrigin, CodeRegionRoot, ContractFunction,
        ContractFunctionKind, DecisionTreeBinding, EffectProviderKind, IntrinsicOp, IntrinsicValue,
        LoopInfo, MatchArmLowering, MatchArmPattern, MatchLoweringInfo, MirBody, MirFunction,
        MirInst, MirModule, MirProjection, MirProjectionPath, Place, SwitchOrigin, SwitchTarget,
        SwitchValue, SyntheticValue, Terminator, ValueData, ValueId, ValueOrigin,
    },
    monomorphize::monomorphize_functions,
};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

mod aggregates;
mod contract;
mod expr;
mod intrinsics;
mod match_lowering;
mod prepass;
mod variants;

pub(super) use contract::extract_contract_function;

/// Errors that can occur while lowering HIR into MIR.
#[derive(Debug)]
pub enum MirLowerError {
    MissingBody { func_name: String },
    MissingCoreHelper { path: String },
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
    db: &'db dyn HirAnalysisDb,
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
        let (_diags, typed_body) = check_func_body(db, func);
        let default_effects = vec![EffectProviderKind::Storage; func.effect_params(db).count()];
        let lowered = lower_function(db, func, typed_body.clone(), None, default_effects)?;
        templates.push(lowered);
    }

    let functions = monomorphize_functions(db, templates);
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
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    typed_body: TypedBody<'db>,
    receiver_space: Option<AddressSpaceKind>,
    effect_provider_kinds: Vec<EffectProviderKind>,
) -> MirLowerResult<MirFunction<'db>> {
    let Some(body) = func.body(db) else {
        let func_name = func
            .name(db)
            .to_opt()
            .map(|ident| ident.data(db).to_string())
            .unwrap_or_else(|| "<anonymous>".to_string());
        return Err(MirLowerError::MissingBody { func_name });
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
    let entry = builder.alloc_block();
    let fallthrough = builder.lower_root(entry, body.expr(db));
    builder.ensure_const_expr_values();
    builder.ensure_field_expr_values();
    builder.ensure_call_expr_values();
    let ret_val = builder.ensure_value(body.expr(db));
    if let Some(block) = fallthrough {
        builder.set_terminator(block, Terminator::Return(Some(ret_val)));
    }
    let mir_body = builder.finish();
    let symbol_name = func
        .name(db)
        .to_opt()
        .map(|ident| ident.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into());

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
    pub(super) db: &'db dyn HirAnalysisDb,
    pub(super) func: Func<'db>,
    pub(super) body: Body<'db>,
    pub(super) typed_body: &'a TypedBody<'db>,
    pub(super) mir_body: MirBody<'db>,
    pub(super) core: CoreLib<'db>,
    pub(super) loop_stack: Vec<LoopScope>,
    pub(super) const_cache: FxHashMap<Const<'db>, ValueId>,
    pub(super) pat_address_space: FxHashMap<PatId, AddressSpaceKind>,
    pub(super) value_address_space: FxHashMap<ValueId, AddressSpaceKind>,
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
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        receiver_space: Option<AddressSpaceKind>,
        effect_provider_kinds: Vec<EffectProviderKind>,
    ) -> Result<Self, MirLowerError> {
        let core = CoreLib::new(db, body)?;

        let builder = Self {
            db,
            func,
            body,
            typed_body,
            mir_body: MirBody::new(),
            core,
            loop_stack: Vec::new(),
            const_cache: FxHashMap::default(),
            pat_address_space: FxHashMap::default(),
            value_address_space: FxHashMap::default(),
            receiver_space,
            effect_provider_kinds,
        };

        Ok(builder)
    }

    /// Consumes the builder and returns the accumulated MIR body.
    ///
    /// # Returns
    /// The completed `MirBody`.
    fn finish(self) -> MirBody<'db> {
        let mut body = self.mir_body;
        body.pat_address_space = self.pat_address_space;
        body
    }

    /// Allocates and returns a fresh basic block.
    ///
    /// # Returns
    /// The identifier for the newly created block.
    fn alloc_block(&mut self) -> BasicBlockId {
        self.mir_body.push_block(BasicBlock::new())
    }

    /// Sets the terminator for the specified block.
    ///
    /// # Parameters
    /// - `block`: Target basic block.
    /// - `term`: Terminator to assign.
    fn set_terminator(&mut self, block: BasicBlockId, term: Terminator) {
        self.mir_body.block_mut(block).set_terminator(term);
    }

    /// Appends an instruction to the given block.
    ///
    /// # Parameters
    /// - `block`: Block receiving the instruction.
    /// - `inst`: Instruction to append.
    fn push_inst(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.mir_body.block_mut(block).push_inst(inst);
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
            LocalBinding::Param { idx, .. } => {
                if *idx == 0 {
                    return self.receiver_space.unwrap_or(AddressSpaceKind::Memory);
                }
                AddressSpaceKind::Memory
            }
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
                ParamSite::EffectField(_) => None,
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
                    _ => None,
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
        let name = self.binding_name(binding)?;
        let value_id = self.mir_body.alloc_value(ValueData {
            ty: self.u256_ty(),
            origin: ValueOrigin::BindingName(name),
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
        let mem_base = self
            .core
            .helper_ty(CoreHelperTy::EffectMemPtr)
            .base_ty(self.db);
        if base_ty == mem_base {
            return Some(EffectProviderKind::Memory);
        }
        let stor_base = self
            .core
            .helper_ty(CoreHelperTy::EffectStorPtr)
            .base_ty(self.db);
        if base_ty == stor_base {
            return Some(EffectProviderKind::Storage);
        }
        let calldata_base = self
            .core
            .helper_ty(CoreHelperTy::EffectCalldataPtr)
            .base_ty(self.db);
        if base_ty == calldata_base {
            return Some(EffectProviderKind::Calldata);
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
