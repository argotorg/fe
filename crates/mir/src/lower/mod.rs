//! MIR lowering entrypoints and shared builder scaffolding. Dispatches to submodules that handle
//! expression lowering, intrinsics, matches, aggregates (records/variants), layout, and contract
//! metadata.

use std::{error::Error, fmt};

use common::diagnostics::{CompleteDiagnostic, Span};
use common::ingot::{Ingot, IngotKind};
use hir::analysis::{
    HirAnalysisDb,
    diagnostics::SpannedHirAnalysisDb,
    place::PlaceBase,
    ty::{
        adt_def::AdtRef,
        effects::EffectKeyKind,
        ty_check::{
            EffectArg, EffectParamSite, EffectPassMode, LocalBinding, ParamSite, PatBindingMode,
            RecordLike, ResolvedEffectArg, TypedBody, check_func_body,
        },
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};
use hir::hir_def::{
    Attr, AttrArg, AttrArgValue, Body, CallableDef, Cond, CondId, Const, Expr, ExprId, Field,
    FieldIndex, Func, HirIngot, IdentId, ItemKind, LitKind, MatchArm, Partial, Pat, PatId, Stmt,
    StmtId, TopLevelMod, VariantKind, expr::BinOp,
};

use crate::{
    capability_space::{
        CapabilitySpaceConflict, capability_spaces_for_ty_with_default,
        normalize_capability_space_entries,
    },
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
    MirDiagnostics {
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
                func_name: _,
                diagnostics,
            } => {
                write!(f, "{diagnostics}")
            }
            MirLowerError::MirDiagnostics {
                func_name: _,
                diagnostics,
            } => {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirDiagnosticsMode {
    TemplatesOnly,
    CompilerParity,
}

#[derive(Debug, Default)]
pub struct MirDiagnosticsOutput {
    pub diagnostics: Vec<CompleteDiagnostic>,
    pub internal_errors: Vec<MirLowerError>,
}

fn collect_funcs_to_lower<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<Func<'db>> {
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

    funcs_to_lower
}

fn mir_func_name<'db>(db: &'db dyn SpannedHirAnalysisDb, func: &MirFunction<'db>) -> String {
    match func.origin {
        crate::ir::MirFunctionOrigin::Hir(hir_func) => hir_func.pretty_print_signature(db),
        crate::ir::MirFunctionOrigin::Synthetic(_) => func.symbol_name.clone(),
    }
}

fn run_borrow_checks_collect<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    functions: &[MirFunction<'db>],
    output: &mut MirDiagnosticsOutput,
) {
    match crate::analysis::borrowck::compute_borrow_summaries(db, functions) {
        Ok(borrow_summaries) => {
            for func in functions {
                if let Some(diag) =
                    crate::analysis::borrowck::check_borrows(db, func, &borrow_summaries)
                {
                    output.diagnostics.push(diag);
                }
            }
        }
        Err(err) => output.diagnostics.push(err.diagnostic.clone()),
    }
}

fn run_borrow_checks_or_error<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    functions: &[MirFunction<'db>],
) -> MirLowerResult<()> {
    let borrow_summaries = crate::analysis::borrowck::compute_borrow_summaries(db, functions)
        .map_err(|err| {
            let diagnostics = hir::analysis::diagnostics::format_diags(db, [&err.diagnostic]);
            MirLowerError::MirDiagnostics {
                func_name: err.func_name,
                diagnostics,
            }
        })?;

    for func in functions {
        if let Some(diag) = crate::analysis::borrowck::check_borrows(db, func, &borrow_summaries) {
            let diagnostics = hir::analysis::diagnostics::format_diags(db, [&diag]);
            return Err(MirLowerError::MirDiagnostics {
                func_name: mir_func_name(db, func),
                diagnostics,
            });
        }
    }
    Ok(())
}

pub fn collect_mir_diagnostics<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    mode: MirDiagnosticsMode,
) -> MirDiagnosticsOutput {
    let mut output = MirDiagnosticsOutput::default();
    let mut templates = Vec::new();

    for func in collect_funcs_to_lower(db, top_mod) {
        if func.body(db).is_none() {
            continue;
        }
        let (diags, typed_body) = check_func_body(db, func);
        if !diags.is_empty() {
            continue;
        }
        match lower_function(
            db,
            func,
            typed_body.clone(),
            None,
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ) {
            Ok(lowered) => templates.push(lowered),
            Err(err) => output.internal_errors.push(err),
        }
    }

    match contracts::lower_contract_templates(db, top_mod) {
        Ok(contract_templates) => templates.extend(contract_templates),
        Err(err) => output.internal_errors.push(err),
    }

    run_borrow_checks_collect(db, &templates, &mut output);

    if matches!(mode, MirDiagnosticsMode::TemplatesOnly) {
        return output;
    }

    let mut functions = match monomorphize_functions(db, templates) {
        Ok(functions) => functions,
        Err(err) => {
            output.internal_errors.push(err);
            return output;
        }
    };
    for func in &mut functions {
        crate::transform::canonicalize_transparent_newtypes(db, &mut func.body);
        crate::transform::insert_temp_binds(db, &mut func.body);
    }

    for func in &functions {
        if let Some(diag) = crate::analysis::noesc::check_noesc_escapes(db, func) {
            output.diagnostics.push(diag);
        }
    }

    run_borrow_checks_collect(db, &functions, &mut output);

    output
}

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
    for func in collect_funcs_to_lower(db, top_mod) {
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
        let lowered = lower_function(
            db,
            func,
            typed_body.clone(),
            None,
            Vec::new(),
            Vec::new(),
            Vec::new(),
        )?;
        templates.push(lowered);
    }

    templates.extend(contracts::lower_contract_templates(db, top_mod)?);

    // Run MIR diagnostics on the generic templates as well as the monomorphized instances. This
    // ensures borrow/move errors are surfaced even when a generic function is never instantiated.
    run_borrow_checks_or_error(db, &templates)?;

    let mut functions = monomorphize_functions(db, templates)?;
    for func in &mut functions {
        crate::transform::canonicalize_transparent_newtypes(db, &mut func.body);
        crate::transform::insert_temp_binds(db, &mut func.body);
    }
    for func in &functions {
        if let Some(diag) = crate::analysis::noesc::check_noesc_escapes(db, func) {
            let func_name = match func.origin {
                crate::ir::MirFunctionOrigin::Hir(hir_func) => hir_func.pretty_print_signature(db),
                crate::ir::MirFunctionOrigin::Synthetic(_) => func.symbol_name.clone(),
            };
            let diagnostics = hir::analysis::diagnostics::format_diags(db, [&diag]);
            return Err(MirLowerError::MirDiagnostics {
                func_name,
                diagnostics,
            });
        }
    }
    run_borrow_checks_or_error(db, &functions)?;

    // Lower semantic capability MIR into backend-specific representation MIR for codegen.
    let core = CoreLib::new(db, top_mod.scope());
    for func in &mut functions {
        crate::transform::lower_capability_to_repr(
            db,
            &core,
            crate::ir::MirBackend::EvmYul,
            &mut func.body,
        );
        crate::transform::canonicalize_transparent_newtypes(db, &mut func.body);
        crate::transform::insert_temp_binds(db, &mut func.body);
        crate::transform::canonicalize_zero_sized(db, &mut func.body);
    }
    Ok(MirModule { top_mod, functions })
}

/// Lowers every function within every top-level module of an ingot into MIR.
///
/// This is primarily useful for emitting artifacts (e.g. `fe build`) across a whole ingot: all
/// contracts and contract entrypoints in any source file should be considered part of the same
/// compilation scope.
pub fn lower_ingot<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    ingot: Ingot<'db>,
) -> MirLowerResult<MirModule<'db>> {
    let mut templates = Vec::new();
    let mut funcs_to_lower = Vec::new();
    let mut seen = FxHashSet::default();

    let mut queue_func = |func: Func<'db>| {
        if seen.insert(func) {
            funcs_to_lower.push(func);
        }
    };

    for &top_mod in ingot.all_modules(db).iter() {
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
        let lowered = lower_function(
            db,
            func,
            typed_body.clone(),
            None,
            Vec::new(),
            Vec::new(),
            Vec::new(),
        )?;
        templates.push(lowered);
    }

    for &top_mod in ingot.all_modules(db).iter() {
        templates.extend(contracts::lower_contract_templates(db, top_mod)?);
    }

    // Run MIR diagnostics on the generic templates as well as the monomorphized instances. This
    // ensures borrow/move errors are surfaced even when a generic function is never instantiated.
    run_borrow_checks_or_error(db, &templates)?;

    let mut functions = monomorphize_functions(db, templates)?;
    for func in &mut functions {
        crate::transform::canonicalize_transparent_newtypes(db, &mut func.body);
        crate::transform::insert_temp_binds(db, &mut func.body);
    }

    for func in &functions {
        if let Some(diag) = crate::analysis::noesc::check_noesc_escapes(db, func) {
            let diagnostics = hir::analysis::diagnostics::format_diags(db, [&diag]);
            return Err(MirLowerError::MirDiagnostics {
                func_name: mir_func_name(db, func),
                diagnostics,
            });
        }
    }
    run_borrow_checks_or_error(db, &functions)?;

    // Lower semantic capability MIR into backend-specific representation MIR for codegen.
    let root_mod = ingot.root_mod(db);
    let core = CoreLib::new(db, root_mod.scope());
    for func in &mut functions {
        crate::transform::lower_capability_to_repr(
            db,
            &core,
            crate::ir::MirBackend::EvmYul,
            &mut func.body,
        );
        crate::transform::canonicalize_transparent_newtypes(db, &mut func.body);
        crate::transform::insert_temp_binds(db, &mut func.body);
        crate::transform::canonicalize_zero_sized(db, &mut func.body);
    }
    Ok(MirModule {
        top_mod: root_mod,
        functions,
    })
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
    param_capability_space_overrides: Vec<Vec<(MirProjectionPath<'db>, AddressSpaceKind)>>,
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
        LoweringOverrides {
            receiver_space,
            effect_param_space_overrides: &effect_param_space_overrides,
            param_capability_space_overrides: &param_capability_space_overrides,
        },
    )?;
    let entry = builder.builder.entry_block();
    builder.move_to_block(entry);
    builder.lower_root(body.expr(db));
    builder.ensure_const_expr_values();
    if let Some(block) = builder.current_block() {
        let ret_ty = func.return_ty(db);
        let returns_value = !builder.is_unit_ty(ret_ty) && !ret_ty.is_never(db);
        let source = builder.source_for_expr(body.expr(db));
        if returns_value {
            let ret_val = builder.ensure_value(body.expr(db));
            builder.set_terminator(
                block,
                Terminator::Return {
                    source,
                    value: Some(ret_val),
                },
            );
        } else {
            builder.set_terminator(
                block,
                Terminator::Return {
                    source,
                    value: None,
                },
            );
        }
    }
    let deferred_error = builder.deferred_error.take();
    let mir_body = builder.finish();

    if let Some(err) = deferred_error {
        return Err(err);
    }

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
    pub(super) const_array_data_cache: FxHashMap<Const<'db>, (TyId<'db>, Vec<u8>)>,
    pub(super) source_info_cache: FxHashMap<Span, crate::ir::SourceInfoId>,
    pub(super) pat_address_space: FxHashMap<PatId, AddressSpaceKind>,
    pub(super) binding_locals: FxHashMap<LocalBinding<'db>, LocalId>,
    /// For methods, the address space variant being lowered.
    pub(super) receiver_space: Option<AddressSpaceKind>,
    /// Address space for each effect parameter, indexed by effect param position.
    pub(super) effect_param_spaces: Vec<AddressSpaceKind>,
    /// Address space overrides for effect bindings not tied to a function effect list.
    pub(super) effect_binding_spaces: FxHashMap<LocalBinding<'db>, AddressSpaceKind>,
    /// Capability-space overrides for function parameters, indexed by parameter position.
    pub(super) param_capability_space_overrides:
        Vec<Vec<(MirProjectionPath<'db>, AddressSpaceKind)>>,
    /// Deferred error from intrinsic lowering (e.g. `encoded_size` on a non-static type).
    pub(super) deferred_error: Option<MirLowerError>,
}

/// Loop context capturing break/continue targets.
#[derive(Clone, Copy)]
pub(super) struct LoopScope {
    pub(super) continue_target: BasicBlockId,
    pub(super) break_target: BasicBlockId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum EffectProviderInferenceRationale {
    ConcreteProviderTy,
    ByRefProviderDefaultsToMemory,
    ContractFieldProviderTy,
    ByTempPlaceMemPtr,
    ForwardedEffectParamProviderTy,
    ForwardedEffectParamFallbackMemPtr,
    ContractFieldFallbackStorPtr,
    DefaultMemPtr,
    StorageDefault,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct InferredEffectProvider<'db> {
    pub(super) provider_ty: Option<TyId<'db>>,
    pub(super) address_space: AddressSpaceKind,
    pub(super) rationale: EffectProviderInferenceRationale,
}

#[derive(Debug, Clone, Copy)]
struct LoweringOverrides<'a, 'db> {
    receiver_space: Option<AddressSpaceKind>,
    effect_param_space_overrides: &'a [Option<AddressSpaceKind>],
    param_capability_space_overrides: &'a [Vec<(MirProjectionPath<'db>, AddressSpaceKind)>],
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
        param_capability_space_overrides: &[Vec<(MirProjectionPath<'db>, AddressSpaceKind)>],
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
            const_array_data_cache: FxHashMap::default(),
            source_info_cache: FxHashMap::default(),
            pat_address_space: FxHashMap::default(),
            binding_locals: FxHashMap::default(),
            receiver_space,
            effect_param_spaces: Vec::new(),
            effect_binding_spaces: FxHashMap::default(),
            param_capability_space_overrides: param_capability_space_overrides.to_vec(),
            deferred_error: None,
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

    fn source_info_for_span(&mut self, span: Option<Span>) -> crate::ir::SourceInfoId {
        let Some(span) = span else {
            return crate::ir::SourceInfoId::SYNTHETIC;
        };
        if let Some(&id) = self.source_info_cache.get(&span) {
            return id;
        }
        let id = self.builder.body.alloc_source_info(Some(span.clone()));
        self.source_info_cache.insert(span, id);
        id
    }

    fn source_for_expr(&mut self, expr: ExprId) -> crate::ir::SourceInfoId {
        self.source_info_for_span(expr.span(self.body).resolve(self.db))
    }

    fn source_for_stmt(&mut self, stmt: StmtId) -> crate::ir::SourceInfoId {
        self.source_info_for_span(stmt.span(self.body).resolve(self.db))
    }

    fn source_for_pat(&mut self, pat: PatId) -> crate::ir::SourceInfoId {
        self.source_info_for_span(pat.span(self.body).resolve(self.db))
    }

    fn source_for_func_param(&mut self, func: Func<'db>, idx: usize) -> crate::ir::SourceInfoId {
        let span = func
            .params(self.db)
            .nth(idx)
            .and_then(|param| param.span().resolve(self.db));
        self.source_info_for_span(span)
    }

    fn new_for_func(
        db: &'db dyn SpannedHirAnalysisDb,
        func: Func<'db>,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        generic_args: &'a [TyId<'db>],
        overrides: LoweringOverrides<'a, 'db>,
    ) -> Result<Self, MirLowerError> {
        let return_ty = func.return_ty(db);
        Self::new(
            db,
            Some(func),
            body,
            typed_body,
            generic_args,
            return_ty,
            overrides.receiver_space,
            overrides.effect_param_space_overrides,
            overrides.param_capability_space_overrides,
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
        let capability_spaces =
            capability_spaces_for_ty_with_default(self.db, ty, AddressSpaceKind::Memory);
        let local = self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut,
            source: crate::ir::SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            capability_spaces,
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
        let ty = self.u256_ty();
        let capability_spaces = capability_spaces_for_ty_with_default(self.db, ty, address_space);
        let local = self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut: binding.is_mut(),
            source: crate::ir::SourceInfoId::SYNTHETIC,
            address_space,
            capability_spaces,
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
            let inferred = self.infer_effect_provider_for_effect_param(
                func,
                effect.index(),
                provider_arg_idx_by_effect,
            );
            let _rationale = inferred.rationale;
            spaces[effect.index()] = inferred.address_space;
        }

        spaces
    }

    fn infer_effect_provider_from_provider_ty(
        &self,
        provider_ty: TyId<'db>,
        concrete_rationale: EffectProviderInferenceRationale,
    ) -> Option<InferredEffectProvider<'db>> {
        if let Some(space) = self.effect_provider_space_for_provider_ty(provider_ty) {
            return Some(InferredEffectProvider {
                provider_ty: Some(provider_ty),
                address_space: space,
                rationale: concrete_rationale,
            });
        }

        // By-ref provider values are passed as pointers; default to memory so callers and
        // callees agree on the address space for projections.
        if matches!(
            crate::repr::repr_kind_for_ty(self.db, &self.core, provider_ty),
            crate::repr::ReprKind::Ref
        ) {
            return Some(InferredEffectProvider {
                provider_ty: Some(provider_ty),
                address_space: AddressSpaceKind::Memory,
                rationale: EffectProviderInferenceRationale::ByRefProviderDefaultsToMemory,
            });
        }

        None
    }

    fn infer_effect_provider_for_effect_param(
        &self,
        func: Func<'db>,
        effect_idx: usize,
        provider_arg_idx_by_effect: &[Option<usize>],
    ) -> InferredEffectProvider<'db> {
        if let Some(provider_arg_idx) = provider_arg_idx_by_effect
            .get(effect_idx)
            .copied()
            .flatten()
            && let Some(provider_ty) = self.generic_args.get(provider_arg_idx).copied()
            && let Some(inferred) = self.infer_effect_provider_from_provider_ty(
                provider_ty,
                EffectProviderInferenceRationale::ConcreteProviderTy,
            )
        {
            return inferred;
        }

        if let Some(provider_ty) =
            self.contract_field_provider_ty_for_effect_site(EffectParamSite::Func(func), effect_idx)
            && let Some(inferred) = self.infer_effect_provider_from_provider_ty(
                provider_ty,
                EffectProviderInferenceRationale::ContractFieldProviderTy,
            )
        {
            return inferred;
        }

        InferredEffectProvider {
            provider_ty: None,
            address_space: AddressSpaceKind::Storage,
            rationale: EffectProviderInferenceRationale::StorageDefault,
        }
    }

    fn infer_effect_provider_or_fallback(
        &self,
        provider_ty: TyId<'db>,
        concrete_rationale: EffectProviderInferenceRationale,
        fallback_space: AddressSpaceKind,
        fallback_rationale: EffectProviderInferenceRationale,
    ) -> InferredEffectProvider<'db> {
        self.infer_effect_provider_from_provider_ty(provider_ty, concrete_rationale)
            .unwrap_or(InferredEffectProvider {
                provider_ty: Some(provider_ty),
                address_space: fallback_space,
                rationale: fallback_rationale,
            })
    }

    fn caller_effect_param_provider_ty(
        &self,
        binding: LocalBinding<'db>,
        caller_provider_arg_idx_by_effect: Option<&[Option<usize>]>,
    ) -> Option<TyId<'db>> {
        let LocalBinding::EffectParam { site, idx, .. } = binding else {
            return None;
        };
        let current_func = self.hir_func?;
        let EffectParamSite::Func(binding_func) = site else {
            return None;
        };
        if binding_func != current_func {
            return None;
        }

        let provider_idx = caller_provider_arg_idx_by_effect?
            .get(idx)
            .copied()
            .flatten()?;
        if let Some(concrete) = self.generic_args.get(provider_idx).copied() {
            return Some(concrete);
        }
        CallableDef::Func(current_func)
            .params(self.db)
            .get(provider_idx)
            .copied()
    }

    fn infer_effect_provider_for_resolved_arg(
        &self,
        resolved_arg: &ResolvedEffectArg<'db>,
        caller_provider_arg_idx_by_effect: Option<&[Option<usize>]>,
    ) -> Option<InferredEffectProvider<'db>> {
        if !matches!(resolved_arg.key_kind, EffectKeyKind::Type) {
            return None;
        }
        let target_ty = resolved_arg.instantiated_target_ty?;

        match resolved_arg.pass_mode {
            EffectPassMode::ByTempPlace => {
                let provider_ty = TyId::app(self.db, self.core.mem_ptr_ctor, target_ty);
                Some(self.infer_effect_provider_or_fallback(
                    provider_ty,
                    EffectProviderInferenceRationale::ByTempPlaceMemPtr,
                    AddressSpaceKind::Memory,
                    EffectProviderInferenceRationale::ByTempPlaceMemPtr,
                ))
            }
            EffectPassMode::ByPlace => {
                let EffectArg::Place(place) = &resolved_arg.arg else {
                    return None;
                };
                let PlaceBase::Binding(binding) = place.base;
                match binding {
                    binding @ LocalBinding::EffectParam { .. } => {
                        if let Some(provider_ty) = self.caller_effect_param_provider_ty(
                            binding,
                            caller_provider_arg_idx_by_effect,
                        ) {
                            return Some(self.infer_effect_provider_or_fallback(
                                provider_ty,
                                EffectProviderInferenceRationale::ForwardedEffectParamProviderTy,
                                AddressSpaceKind::Storage,
                                EffectProviderInferenceRationale::StorageDefault,
                            ));
                        }

                        let provider_ty = TyId::app(self.db, self.core.mem_ptr_ctor, target_ty);
                        Some(self.infer_effect_provider_or_fallback(
                            provider_ty,
                            EffectProviderInferenceRationale::ForwardedEffectParamFallbackMemPtr,
                            AddressSpaceKind::Memory,
                            EffectProviderInferenceRationale::ForwardedEffectParamFallbackMemPtr,
                        ))
                    }
                    LocalBinding::Param {
                        site: ParamSite::EffectField(effect_site),
                        idx,
                        ..
                    } => {
                        if let Some(provider_ty) =
                            self.contract_field_provider_ty_for_effect_site(effect_site, idx)
                        {
                            return Some(self.infer_effect_provider_or_fallback(
                                provider_ty,
                                EffectProviderInferenceRationale::ContractFieldProviderTy,
                                AddressSpaceKind::Storage,
                                EffectProviderInferenceRationale::StorageDefault,
                            ));
                        }

                        let provider_ty = TyId::app(self.db, self.core.stor_ptr_ctor, target_ty);
                        Some(self.infer_effect_provider_or_fallback(
                            provider_ty,
                            EffectProviderInferenceRationale::ContractFieldFallbackStorPtr,
                            AddressSpaceKind::Storage,
                            EffectProviderInferenceRationale::ContractFieldFallbackStorPtr,
                        ))
                    }
                    _ => {
                        let provider_ty = TyId::app(self.db, self.core.mem_ptr_ctor, target_ty);
                        Some(self.infer_effect_provider_or_fallback(
                            provider_ty,
                            EffectProviderInferenceRationale::DefaultMemPtr,
                            AddressSpaceKind::Memory,
                            EffectProviderInferenceRationale::DefaultMemPtr,
                        ))
                    }
                }
            }
            _ => None,
        }
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
        self.set_current_terminator(Terminator::Goto {
            source: crate::ir::SourceInfoId::SYNTHETIC,
            target,
        });
    }

    fn branch(&mut self, cond: ValueId, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        let source = self.builder.body.value(cond).source;
        self.set_current_terminator(Terminator::Branch {
            source,
            cond,
            then_bb,
            else_bb,
        });
    }

    fn switch(&mut self, discr: ValueId, targets: Vec<SwitchTarget>, default: BasicBlockId) {
        let source = self.builder.body.value(discr).source;
        self.set_current_terminator(Terminator::Switch {
            source,
            discr,
            targets,
            default,
        });
    }

    pub(super) fn alloc_temp_local(&mut self, ty: TyId<'db>, is_mut: bool, hint: &str) -> LocalId {
        let idx = self.builder.body.locals.len();
        let name = format!("tmp_{hint}{idx}");
        let capability_spaces =
            capability_spaces_for_ty_with_default(self.db, ty, AddressSpaceKind::Memory);
        self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut,
            source: crate::ir::SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            capability_spaces,
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

    fn normalize_capability_spaces(
        &mut self,
        spaces: Vec<(MirProjectionPath<'db>, AddressSpaceKind)>,
    ) -> Vec<(MirProjectionPath<'db>, AddressSpaceKind)> {
        match normalize_capability_space_entries(spaces) {
            Ok(normalized) => normalized,
            Err(conflict) => {
                self.defer_capability_space_conflict(conflict);
                Vec::new()
            }
        }
    }

    fn defer_capability_space_conflict(&mut self, conflict: CapabilitySpaceConflict<'db>) {
        if self.deferred_error.is_some() {
            return;
        }
        let func_name = self
            .hir_func
            .map(|func| func.pretty_print_signature(self.db))
            .unwrap_or_else(|| "<body owner>".to_owned());
        self.deferred_error = Some(MirLowerError::Unsupported {
            func_name,
            message: format!(
                "conflicting non-memory capability spaces for path `{:?}`: `{:?}` vs `{:?}`",
                conflict.path, conflict.existing, conflict.incoming
            ),
        });
    }

    fn capability_spaces_for_projection_from_local(
        &mut self,
        local: LocalId,
        projection: &MirProjectionPath<'db>,
    ) -> Vec<(MirProjectionPath<'db>, AddressSpaceKind)> {
        let Some(local_data) = self.builder.body.locals.get(local.index()) else {
            return Vec::new();
        };
        let local_capability_spaces = local_data.capability_spaces.clone();

        let mut spaces = Vec::new();
        for (path, space) in &local_capability_spaces {
            if let Some(suffix) = crate::ir::projection_strip_prefix(path, projection) {
                spaces.push((suffix, *space));
                continue;
            }
            if path.is_prefix_of(projection) {
                spaces.push((MirProjectionPath::new(), *space));
            }
        }
        self.normalize_capability_spaces(spaces)
    }

    fn capability_spaces_for_place(
        &mut self,
        place: &Place<'db>,
        target_ty: TyId<'db>,
    ) -> Vec<(MirProjectionPath<'db>, AddressSpaceKind)> {
        if let Some((local, base_projection)) =
            crate::ir::resolve_local_projection_root(&self.builder.body.values, place.base)
        {
            let full_projection = base_projection.concat(&place.projection);
            let spaces = self.capability_spaces_for_projection_from_local(local, &full_projection);
            if !spaces.is_empty() {
                return spaces;
            }
        }

        if target_ty.as_capability(self.db).is_some()
            && let Some(space) = crate::ir::try_value_address_space_in(
                &self.builder.body.values,
                &self.builder.body.locals,
                place.base,
            )
        {
            return vec![(MirProjectionPath::new(), space)];
        }

        Vec::new()
    }

    fn capability_spaces_for_value(
        &mut self,
        value: ValueId,
    ) -> Vec<(MirProjectionPath<'db>, AddressSpaceKind)> {
        let (ty, origin) = {
            let data = self.builder.body.value(value);
            (data.ty, data.origin.clone())
        };
        match origin {
            ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => self
                .builder
                .body
                .locals
                .get(local.index())
                .map(|local| local.capability_spaces.clone())
                .unwrap_or_default(),
            ValueOrigin::TransparentCast { value } => self.capability_spaces_for_value(value),
            ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                self.capability_spaces_for_place(&place, ty)
            }
            ValueOrigin::FieldPtr(field_ptr) if ty.as_capability(self.db).is_some() => {
                vec![(MirProjectionPath::new(), field_ptr.addr_space)]
            }
            _ if ty.as_capability(self.db).is_some() => crate::ir::try_value_address_space_in(
                &self.builder.body.values,
                &self.builder.body.locals,
                value,
            )
            .map(|space| vec![(MirProjectionPath::new(), space)])
            .unwrap_or_default(),
            _ => Vec::new(),
        }
    }

    fn value_root_capability_space_hint(&self, value: ValueId) -> AddressSpaceKind {
        if let Some((local, projection)) =
            crate::ir::resolve_local_projection_root(&self.builder.body.values, value)
            && let Some(space) = crate::ir::lookup_local_capability_space(
                &self.builder.body.locals,
                local,
                &projection,
            )
        {
            return space;
        }
        self.value_address_space_or_memory_fallback(value)
    }

    fn collect_explicit_return_param_sources_in_stmt(
        &self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        stmt: StmtId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
    ) {
        let Partial::Present(stmt_data) = stmt.data(self.db, body) else {
            return;
        };

        match stmt_data {
            Stmt::Let(_, _, Some(init)) => self.collect_explicit_return_param_sources_in_expr(
                body,
                typed_body,
                *init,
                out,
                saw_non_param,
            ),
            Stmt::For(_, iter, loop_body, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *iter,
                    out,
                    saw_non_param,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *loop_body,
                    out,
                    saw_non_param,
                );
            }
            Stmt::While(cond, loop_body) => {
                self.collect_explicit_return_param_sources_in_cond(
                    body,
                    typed_body,
                    *cond,
                    out,
                    saw_non_param,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *loop_body,
                    out,
                    saw_non_param,
                );
            }
            Stmt::Return(Some(expr)) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *expr,
                    out,
                    saw_non_param,
                );
                self.collect_implicit_return_param_sources_from_expr(
                    body,
                    typed_body,
                    *expr,
                    out,
                    saw_non_param,
                );
            }
            Stmt::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                body,
                typed_body,
                *expr,
                out,
                saw_non_param,
            ),
            Stmt::Let(_, _, None) | Stmt::Return(None) | Stmt::Continue | Stmt::Break => {}
        }
    }

    fn collect_explicit_return_param_sources_in_expr(
        &self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        expr: ExprId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
    ) {
        let Partial::Present(expr_data) = expr.data(self.db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                for stmt in stmts {
                    self.collect_explicit_return_param_sources_in_stmt(
                        body,
                        typed_body,
                        *stmt,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *lhs,
                    out,
                    saw_non_param,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *rhs,
                    out,
                    saw_non_param,
                );
            }
            Expr::Un(inner, _) | Expr::Cast(inner, _) | Expr::Field(inner, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *inner,
                    out,
                    saw_non_param,
                );
            }
            Expr::Call(callee, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *callee,
                    out,
                    saw_non_param,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        body,
                        typed_body,
                        arg.expr,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *receiver,
                    out,
                    saw_non_param,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        body,
                        typed_body,
                        arg.expr,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::RecordInit(_, fields) => {
                for field in fields {
                    self.collect_explicit_return_param_sources_in_expr(
                        body,
                        typed_body,
                        field.expr,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                for item in items {
                    self.collect_explicit_return_param_sources_in_expr(
                        body,
                        typed_body,
                        *item,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::ArrayRep(value, _) => self.collect_explicit_return_param_sources_in_expr(
                body,
                typed_body,
                *value,
                out,
                saw_non_param,
            ),
            Expr::If(cond, then_expr, else_expr) => {
                self.collect_explicit_return_param_sources_in_cond(
                    body,
                    typed_body,
                    *cond,
                    out,
                    saw_non_param,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *then_expr,
                    out,
                    saw_non_param,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_explicit_return_param_sources_in_expr(
                        body,
                        typed_body,
                        *else_expr,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *scrutinee,
                    out,
                    saw_non_param,
                );
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_explicit_return_param_sources_in_expr(
                            body,
                            typed_body,
                            arm.body,
                            out,
                            saw_non_param,
                        );
                    }
                }
            }
            Expr::With(bindings, with_body) => {
                for binding in bindings {
                    self.collect_explicit_return_param_sources_in_expr(
                        body,
                        typed_body,
                        binding.value,
                        out,
                        saw_non_param,
                    );
                }
                self.collect_explicit_return_param_sources_in_expr(
                    body,
                    typed_body,
                    *with_body,
                    out,
                    saw_non_param,
                );
            }
            Expr::Lit(_) | Expr::Path(_) => {}
        }
    }

    fn collect_implicit_return_param_sources_from_expr(
        &self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        expr: ExprId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
    ) {
        let Partial::Present(expr_data) = expr.data(self.db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                if let Some(last_stmt) = stmts.last()
                    && let Partial::Present(Stmt::Expr(tail_expr)) = last_stmt.data(self.db, body)
                {
                    self.collect_implicit_return_param_sources_from_expr(
                        body,
                        typed_body,
                        *tail_expr,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::If(_, then_expr, else_expr) => {
                self.collect_implicit_return_param_sources_from_expr(
                    body,
                    typed_body,
                    *then_expr,
                    out,
                    saw_non_param,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_implicit_return_param_sources_from_expr(
                        body,
                        typed_body,
                        *else_expr,
                        out,
                        saw_non_param,
                    );
                }
            }
            Expr::Match(_, arms) => {
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_implicit_return_param_sources_from_expr(
                            body,
                            typed_body,
                            arm.body,
                            out,
                            saw_non_param,
                        );
                    }
                }
            }
            Expr::With(_, with_body) => self.collect_implicit_return_param_sources_from_expr(
                body,
                typed_body,
                *with_body,
                out,
                saw_non_param,
            ),
            _ => {
                let Some(place) = typed_body.expr_place(self.db, expr) else {
                    *saw_non_param = true;
                    return;
                };
                match place.base {
                    PlaceBase::Binding(LocalBinding::Param { idx, .. }) => {
                        out.insert(idx);
                    }
                    PlaceBase::Binding(_) => *saw_non_param = true,
                }
            }
        }
    }

    fn collect_explicit_return_param_sources_in_cond(
        &self,
        body: Body<'db>,
        typed_body: &TypedBody<'db>,
        cond: CondId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
    ) {
        let Partial::Present(cond_data) = cond.data(self.db, body) else {
            return;
        };

        match cond_data {
            Cond::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                body,
                typed_body,
                *expr,
                out,
                saw_non_param,
            ),
            Cond::Let(_, value) => self.collect_explicit_return_param_sources_in_expr(
                body,
                typed_body,
                *value,
                out,
                saw_non_param,
            ),
            Cond::Bin(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_cond(
                    body,
                    typed_body,
                    *lhs,
                    out,
                    saw_non_param,
                );
                self.collect_explicit_return_param_sources_in_cond(
                    body,
                    typed_body,
                    *rhs,
                    out,
                    saw_non_param,
                );
            }
        }
    }

    fn call_return_param_sources(&self, call: &CallOrigin<'db>) -> Option<Vec<usize>> {
        let hir_target = call.hir_target.as_ref()?;
        let CallableDef::Func(func) = hir_target.callable_def else {
            return None;
        };
        let (diags, typed_body) = check_func_body(self.db, func);
        if !diags.is_empty() {
            return None;
        }
        let body = typed_body.body()?;
        let func_body = func.body(self.db)?;
        let mut out = FxHashSet::default();
        let mut saw_non_param = false;
        let root_expr = func_body.expr(self.db);
        self.collect_explicit_return_param_sources_in_expr(
            body,
            typed_body,
            root_expr,
            &mut out,
            &mut saw_non_param,
        );
        self.collect_implicit_return_param_sources_from_expr(
            body,
            typed_body,
            root_expr,
            &mut out,
            &mut saw_non_param,
        );
        if saw_non_param || out.is_empty() {
            return None;
        }

        let mut indices = out.into_iter().collect::<Vec<_>>();
        indices.sort_unstable();
        Some(indices)
    }

    fn defer_call_return_space_conflict(
        &mut self,
        call: &CallOrigin<'db>,
        existing: AddressSpaceKind,
        incoming: AddressSpaceKind,
    ) {
        if self.deferred_error.is_some() {
            return;
        }
        let func_name = self
            .hir_func
            .map(|func| func.pretty_print_signature(self.db))
            .unwrap_or_else(|| "<body owner>".to_owned());
        let callee = call
            .hir_target
            .as_ref()
            .and_then(|target| {
                if let CallableDef::Func(func) = target.callable_def {
                    return Some(func.pretty_print_signature(self.db));
                }
                None
            })
            .unwrap_or_else(|| "<call target>".to_owned());
        self.deferred_error = Some(MirLowerError::Unsupported {
            func_name,
            message: format!(
                "call to `{callee}` can return a capability from conflicting address spaces (`{existing:?}` vs `{incoming:?}`)"
            ),
        });
    }

    fn call_return_space_hint_from_args(
        &mut self,
        call: &CallOrigin<'db>,
        dest_ty: TyId<'db>,
    ) -> Option<AddressSpaceKind> {
        let has_receiver = call
            .hir_target
            .as_ref()
            .and_then(|target| target.callable_def.receiver_ty(self.db))
            .is_some();

        if let Some(return_param_indices) = self.call_return_param_sources(call) {
            let mut space_hint = None;
            for idx in return_param_indices {
                let Some(arg_value) = call.args.get(idx).copied() else {
                    continue;
                };
                let space = if has_receiver && idx == 0 {
                    call.receiver_space
                        .unwrap_or_else(|| self.value_root_capability_space_hint(arg_value))
                } else {
                    self.value_root_capability_space_hint(arg_value)
                };
                match space_hint {
                    None => space_hint = Some(space),
                    Some(prev) if prev == space => {}
                    Some(prev) => {
                        self.defer_call_return_space_conflict(call, prev, space);
                        return None;
                    }
                }
            }
            if space_hint.is_some() {
                return space_hint;
            }
        }

        let hir_target = call.hir_target.as_ref()?;
        let expected_arg_tys = hir_target
            .callable_def
            .arg_tys(self.db)
            .into_iter()
            .map(|ty| ty.instantiate(self.db, &hir_target.generic_args))
            .collect::<Vec<_>>();

        let mut space_hint = None;
        for (idx, arg_value) in call.args.iter().copied().enumerate() {
            if has_receiver && idx == 0 {
                continue;
            }
            let Some(expected_ty) = expected_arg_tys.get(idx).copied() else {
                continue;
            };
            if expected_ty != dest_ty {
                continue;
            }

            let space = self.value_root_capability_space_hint(arg_value);
            match space_hint {
                None => space_hint = Some(space),
                Some(prev) if prev == space => {}
                Some(_) => return Some(AddressSpaceKind::Memory),
            }
        }

        space_hint
    }

    fn capability_spaces_for_rvalue(
        &mut self,
        dest: LocalId,
        rvalue: &Rvalue<'db>,
    ) -> Vec<(MirProjectionPath<'db>, AddressSpaceKind)> {
        let (dest_ty, dest_address_space) = {
            let dest_local = self.builder.body.local(dest);
            (dest_local.ty, dest_local.address_space)
        };
        match rvalue {
            Rvalue::Value(value) => self.capability_spaces_for_value(*value),
            Rvalue::Load { place } => self.capability_spaces_for_place(place, dest_ty),
            Rvalue::Call(call) => {
                let mut spaces = capability_spaces_for_ty_with_default(
                    self.db,
                    dest_ty,
                    AddressSpaceKind::Memory,
                );
                if spaces.is_empty() && hir::analysis::ty::ty_is_noesc(self.db, dest_ty) {
                    spaces.push((MirProjectionPath::new(), AddressSpaceKind::Memory));
                }
                let call_space_hint = self
                    .call_return_space_hint_from_args(call, dest_ty)
                    .or_else(|| {
                        call.receiver_space
                            .filter(|space| !matches!(space, AddressSpaceKind::Memory))
                    });
                if let Some(space) = call_space_hint
                    && !matches!(space, AddressSpaceKind::Memory)
                {
                    if spaces.is_empty() && hir::analysis::ty::ty_is_noesc(self.db, dest_ty) {
                        spaces.push((MirProjectionPath::new(), space));
                    }
                    for (_, mapped_space) in &mut spaces {
                        *mapped_space = space;
                    }
                }
                if spaces.is_empty() && dest_ty.as_capability(self.db).is_some() {
                    return vec![(MirProjectionPath::new(), dest_address_space)];
                }
                spaces
            }
            Rvalue::Intrinsic { .. } => {
                if dest_ty.as_capability(self.db).is_some() {
                    vec![(MirProjectionPath::new(), dest_address_space)]
                } else {
                    Vec::new()
                }
            }
            Rvalue::ZeroInit | Rvalue::Alloc { .. } | Rvalue::ConstAggregate { .. } => Vec::new(),
        }
    }

    fn assign(&mut self, stmt: Option<StmtId>, dest: Option<LocalId>, rvalue: Rvalue<'db>) {
        if let Some(dest) = dest {
            let spaces = self.capability_spaces_for_rvalue(dest, &rvalue);
            self.builder.body.locals[dest.index()].capability_spaces =
                self.normalize_capability_spaces(spaces);
        }

        let source = stmt
            .map(|stmt| self.source_for_stmt(stmt))
            .unwrap_or(crate::ir::SourceInfoId::SYNTHETIC);
        self.push_inst_here(MirInst::Assign {
            source,
            dest,
            rvalue,
        });
    }

    fn alloc_value(&mut self, ty: TyId<'db>, origin: ValueOrigin<'db>, repr: ValueRepr) -> ValueId {
        self.builder.body.alloc_value(ValueData {
            ty,
            origin,
            source: crate::ir::SourceInfoId::SYNTHETIC,
            repr,
        })
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
        if ty.as_capability(self.db).is_some() {
            if matches!(self.builder.body.stage, crate::ir::MirStage::Capability) {
                return ValueRepr::Word;
            }
            let space = self.expr_address_space(expr);
            return match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
                crate::repr::ReprKind::Ptr(_) => ValueRepr::Ptr(space),
                crate::repr::ReprKind::Ref => ValueRepr::Ref(space),
                crate::repr::ReprKind::Zst | crate::repr::ReprKind::Word => ValueRepr::Word,
            };
        }

        match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
            crate::repr::ReprKind::Ptr(space) => ValueRepr::Ptr(space),
            crate::repr::ReprKind::Ref => ValueRepr::Ref(self.expr_address_space(expr)),
            crate::repr::ReprKind::Zst | crate::repr::ReprKind::Word => ValueRepr::Word,
        }
    }

    pub(super) fn value_repr_for_ty(&self, ty: TyId<'db>, space: AddressSpaceKind) -> ValueRepr {
        if ty.as_capability(self.db).is_some() {
            if matches!(self.builder.body.stage, crate::ir::MirStage::Capability) {
                return ValueRepr::Word;
            }
            return match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
                crate::repr::ReprKind::Ptr(_) => ValueRepr::Ptr(space),
                crate::repr::ReprKind::Ref => ValueRepr::Ref(space),
                crate::repr::ReprKind::Zst | crate::repr::ReprKind::Word => ValueRepr::Word,
            };
        }

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
        binding_mode: PatBindingMode,
    ) -> ValueId {
        let is_borrow_binding = matches!(binding_mode, PatBindingMode::ByBorrow);
        // Transparent newtype access: field 0 is a representation-preserving cast.
        if !is_borrow_binding
            && field_ty.as_capability(self.db).is_none()
            && crate::repr::transparent_field0_inner_ty(self.db, tuple_ty, field_idx).is_some()
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
        if is_borrow_binding {
            let field_space = self.value_address_space(tuple_value);
            return self.alloc_value(
                field_ty,
                ValueOrigin::PlaceRef(place),
                self.value_repr_for_ty(field_ty, field_space),
            );
        }
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
                let carries_space = self
                    .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                    .address_space()
                    .is_some()
                    || pat_ty.as_capability(self.db).is_some();
                if carries_space {
                    let space = crate::ir::try_value_address_space_in(
                        &self.builder.body.values,
                        &self.builder.body.locals,
                        value,
                    )
                    .unwrap_or(AddressSpaceKind::Memory);
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
                    let binding_mode = self
                        .typed_body
                        .pat_binding_mode(*field_pat)
                        .unwrap_or(PatBindingMode::ByValue);
                    let field_value = if owner_by_ref
                        || matches!(binding_mode, PatBindingMode::ByBorrow)
                    {
                        self.project_tuple_elem_value(value, owner_ty, idx, field_ty, binding_mode)
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
                    let binding_mode = self
                        .typed_body
                        .pat_binding_mode(field.pat)
                        .unwrap_or(PatBindingMode::ByValue);
                    let field_value =
                        if owner_by_ref || matches!(binding_mode, PatBindingMode::ByBorrow) {
                            if self
                                .value_repr_for_ty(field_ty, AddressSpaceKind::Memory)
                                .address_space()
                                .is_some()
                            {
                                self.set_pat_address_space(field.pat, base_space);
                            }
                            self.project_tuple_elem_value(
                                value,
                                owner_ty,
                                info.field_idx,
                                field_ty,
                                binding_mode,
                            )
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
            let source = self.source_info_for_span(param.span().resolve(self.db));
            let binding = self
                .typed_body
                .param_binding(idx)
                .unwrap_or(LocalBinding::Param {
                    site: ParamSite::Func(func),
                    idx,
                    mode: param.mode(self.db),
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
            let address_space = self.address_space_for_binding(&binding);
            let mut capability_spaces =
                capability_spaces_for_ty_with_default(self.db, ty, address_space);
            if let Some(overrides) = self.param_capability_space_overrides.get(idx) {
                for (path, space) in overrides {
                    capability_spaces.retain(|(existing, _)| existing != path);
                    capability_spaces.push((path.clone(), *space));
                }
                capability_spaces = self.normalize_capability_spaces(capability_spaces);
            }
            let local = self.builder.body.alloc_local(LocalData {
                name,
                ty,
                is_mut: binding.is_mut(),
                source,
                address_space,
                capability_spaces,
            });
            self.builder.body.param_locals.push(local);
            self.binding_locals.insert(binding, local);
        }

        let effects_source = self.source_info_for_span(func.span().effects().resolve(self.db));
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
                source: effects_source,
                address_space: self.address_space_for_binding(&binding),
                capability_spaces: Vec::new(),
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
        let source = match &binding {
            LocalBinding::Local { pat, .. } => self.source_for_pat(*pat),
            LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                ..
            } => self.source_for_func_param(*func, *idx),
            _ => crate::ir::SourceInfoId::SYNTHETIC,
        };
        let local = self.builder.body.alloc_local(LocalData {
            name,
            ty,
            is_mut,
            source,
            address_space: self.address_space_for_binding(&binding),
            capability_spaces: capability_spaces_for_ty_with_default(
                self.db,
                ty,
                self.address_space_for_binding(&binding),
            ),
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
        let value_id = self.alloc_value(self.u256_ty(), ValueOrigin::Local(local), ValueRepr::Word);
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

    pub(super) fn value_address_space_or_memory_fallback(
        &self,
        value: ValueId,
    ) -> AddressSpaceKind {
        crate::ir::try_value_address_space_in(
            &self.builder.body.values,
            &self.builder.body.locals,
            value,
        )
        .unwrap_or(AddressSpaceKind::Memory)
    }

    pub(super) fn value_local_address_space_hint(
        &self,
        value: ValueId,
    ) -> Option<AddressSpaceKind> {
        let mut root = value;
        while let ValueOrigin::TransparentCast { value } = &self.builder.body.value(root).origin {
            root = *value;
        }

        match self.builder.body.value(root).origin {
            ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
                Some(self.builder.body.local(local).address_space)
            }
            _ => None,
        }
    }

    pub(super) fn capability_binding_space_from_container(
        &self,
        container: ValueId,
    ) -> AddressSpaceKind {
        if let Some(space) = self.value_local_address_space_hint(container)
            && space != AddressSpaceKind::Memory
        {
            return space;
        }

        self.value_address_space_or_memory_fallback(container)
    }

    pub(super) fn ty_contains_capability(&self, ty: TyId<'db>) -> bool {
        fn visit<'db>(
            builder: &MirBuilder<'db, '_>,
            ty: TyId<'db>,
            seen: &mut FxHashSet<TyId<'db>>,
        ) -> bool {
            if !seen.insert(ty) {
                return false;
            }

            if ty.as_capability(builder.db).is_some() {
                return true;
            }

            if let Some(inner) = crate::repr::transparent_newtype_field_ty(builder.db, ty)
                && visit(builder, inner, seen)
            {
                return true;
            }

            for arg in ty.generic_args(builder.db) {
                if visit(builder, *arg, seen) {
                    return true;
                }
            }

            for field_ty in ty.field_types(builder.db) {
                if visit(builder, field_ty, seen) {
                    return true;
                }
            }

            false
        }

        let mut seen = FxHashSet::default();
        visit(self, ty, &mut seen)
    }

    /// Associates a pattern with an address space.
    ///
    /// # Parameters
    /// - `pat`: Pattern id to annotate.
    /// - `space`: Address space kind to record.
    pub(super) fn set_pat_address_space(&mut self, pat: PatId, space: AddressSpaceKind) {
        self.pat_address_space.insert(pat, space);
        let locals_to_update: Vec<LocalId> = self
            .binding_locals
            .iter()
            .filter_map(|(binding, local)| match binding {
                LocalBinding::Local {
                    pat: binding_pat, ..
                } if *binding_pat == pat => Some(*local),
                _ => None,
            })
            .collect();
        for local in locals_to_update {
            self.builder.body.locals[local.index()].address_space = space;
        }
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
                    crate::ir::Rvalue::ConstAggregate { .. } => {}
                },
                MirInst::BindValue { value, .. } => {
                    used_values.insert(*value);
                }
                MirInst::Store { place, value, .. } => {
                    used_values.insert(place.base);
                    used_values.insert(*value);
                    used_values.extend(dynamic_indices(&place.projection));
                }
                MirInst::InitAggregate { place, inits, .. } => {
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
            Terminator::Return {
                value: Some(value), ..
            } => {
                used_values.insert(*value);
            }
            Terminator::TerminatingCall { call, .. } => match call {
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
            Terminator::Return { value: None, .. }
            | Terminator::Goto { .. }
            | Terminator::Unreachable { .. } => {}
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
