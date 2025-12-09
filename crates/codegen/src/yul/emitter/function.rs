use driver::DriverDataBase;
use hir::{
    analysis::{
        name_resolution::{PathRes, resolve_path},
        ty::{
            adt_def::AdtRef,
            trait_resolution::PredicateListId,
            ty_def::{PrimTy, TyBase, TyData, TyId, prim_int_bits},
        },
    },
    hir_def::{Body, Expr, ExprId, Partial, Pat, PatId, PathId, Stmt, StmtId},
};
use mir::MirFunction;
use rustc_hash::FxHashMap;

use crate::yul::{doc::YulDoc, errors::YulError, state::BlockState};

use super::util::function_name;

/// Emits Yul for a single MIR function.
pub(super) struct FunctionEmitter<'db> {
    pub(super) db: &'db DriverDataBase,
    pub(super) mir_func: &'db MirFunction<'db>,
    body: Body<'db>,
    /// Temporaries allocated for expression values that must be re-used later (e.g. struct ptrs).
    pub(super) expr_temps: FxHashMap<ExprId, String>,
    pub(super) match_values: FxHashMap<ExprId, String>,
    /// Number of MIR references per value so we can avoid evaluating them twice.
    pub(super) value_use_counts: Vec<usize>,
    /// Mapping from monomorphized function symbols to code region labels.
    pub(super) code_regions: &'db FxHashMap<String, String>,
}

impl<'db> FunctionEmitter<'db> {
    /// Constructs a new emitter for the given MIR function.
    ///
    /// * `db` - Driver database providing access to bodies and type info.
    /// * `mir_func` - MIR function to lower into Yul.
    ///
    /// Returns the initialized emitter or [`YulError::MissingBody`] if the
    /// function lacks a body.
    pub(super) fn new(
        db: &'db DriverDataBase,
        mir_func: &'db MirFunction<'db>,
        code_regions: &'db FxHashMap<String, String>,
    ) -> Result<Self, YulError> {
        let body = mir_func
            .func
            .body(db)
            .ok_or_else(|| YulError::MissingBody(function_name(db, mir_func.func)))?;
        let value_use_counts = Self::collect_value_use_counts(&mir_func.body);
        Ok(Self {
            db,
            mir_func,
            body,
            expr_temps: FxHashMap::default(),
            match_values: FxHashMap::default(),
            value_use_counts,
            code_regions,
        })
    }

    /// Counts how many MIR instructions/terminators use each `ValueId`.
    fn collect_value_use_counts(body: &mir::MirBody<'db>) -> Vec<usize> {
        let mut counts = vec![0; body.values.len()];
        for block in &body.blocks {
            for inst in &block.insts {
                match inst {
                    mir::MirInst::Let { value, .. } => {
                        if let Some(value) = value {
                            counts[value.index()] += 1;
                        }
                    }
                    mir::MirInst::Assign { value, .. }
                    | mir::MirInst::AugAssign { value, .. }
                    | mir::MirInst::Eval { value, .. }
                    | mir::MirInst::EvalExpr { value, .. } => {
                        counts[value.index()] += 1;
                    }
                    mir::MirInst::IntrinsicStmt { args, .. } => {
                        for arg in args {
                            counts[arg.index()] += 1;
                        }
                    }
                }
            }
            match &block.terminator {
                mir::Terminator::Return(Some(value)) => counts[value.index()] += 1,
                mir::Terminator::ReturnData { offset, size } => {
                    counts[offset.index()] += 1;
                    counts[size.index()] += 1;
                }
                mir::Terminator::Branch { cond, .. } => counts[cond.index()] += 1,
                mir::Terminator::Switch { discr, .. } => counts[discr.index()] += 1,
                mir::Terminator::Return(None)
                | mir::Terminator::Goto { .. }
                | mir::Terminator::Unreachable => {}
            }
        }
        counts
    }

    /// Produces the final Yul docs for the current MIR function, including any prologue
    /// needed to seed effect bindings (e.g. storage base pointer for contract entrypoints).
    ///
    /// Returns the document tree containing a single Yul `function` block or a
    /// [`YulError`] when lowering fails.
    pub(super) fn emit_doc(mut self) -> Result<Vec<YulDoc>, YulError> {
        let func_name = self.mir_func.symbol_name.as_str();
        let (param_names, mut state, mut prologue) = self.init_function_state()?;
        let mut body_docs = self.emit_block(self.mir_func.body.entry, &mut state)?;
        if !prologue.is_empty() {
            prologue.append(&mut body_docs);
            body_docs = prologue;
        }
        let function_doc = YulDoc::block(
            format!(
                "{} ",
                self.format_function_signature(func_name, &param_names)
            ),
            body_docs,
        );
        Ok(vec![function_doc])
    }

    /// Initializes the `BlockState` with parameter bindings, returning Yul parameter names,
    /// the populated block state, and any prologue statements needed to seed effect bindings
    /// (contract entrypoints get a synthesized storage pointer; other functions take effects
    /// as explicit parameters).
    pub(super) fn init_function_state(
        &self,
    ) -> Result<(Vec<String>, BlockState, Vec<YulDoc>), YulError> {
        let mut state = BlockState::new();
        let mut params_out = Vec::new();
        for (idx, param) in self.mir_func.func.params(self.db).enumerate() {
            let original = param
                .name(self.db)
                .map(|id| id.data(self.db).to_string())
                .unwrap_or_else(|| format!("arg{idx}"));
            let yul_name = original.clone();
            params_out.push(yul_name.clone());
            state.insert_binding(original, yul_name);
        }
        let mut prologue = Vec::new();
        let effect_params: Vec<_> = self.mir_func.func.effect_params(self.db).collect();
        let is_contract_entry = self.mir_func.contract_function.is_some();
        if is_contract_entry {
            let mut storage_offset = 0u64;
            for effect in effect_params {
                let binding = self.effect_binding_name(effect);
                let temp = state.alloc_local();
                state.insert_binding(binding.clone(), temp.clone());
                let base = self.storage_base_ptr_expr();
                let ptr_expr = if storage_offset == 0 {
                    base.to_string()
                } else {
                    format!("add({base}, {storage_offset})")
                };
                prologue.push(YulDoc::line(format!("let {temp} := {ptr_expr}")));
                let stride = self.effect_size_bytes(effect, &binding)?;
                storage_offset = storage_offset.saturating_add(stride);
            }
        } else {
            for effect in effect_params {
                let binding = self.effect_binding_name(effect);
                params_out.push(binding.clone());
                state.insert_binding(binding.clone(), binding);
            }
        }
        Ok((params_out, state, prologue))
    }

    /// Returns true if the Fe function has a return type.
    pub(super) fn returns_value(&self) -> bool {
        self.mir_func.func.has_explicit_return_ty(self.db)
    }

    /// Formats the Fe function name and parameters into a Yul signature.
    fn format_function_signature(&self, func_name: &str, params: &[String]) -> String {
        let params_str = params.join(", ");
        let ret_suffix = if self.returns_value() { " -> ret" } else { "" };
        if params.is_empty() {
            format!("function {func_name}(){ret_suffix}")
        } else {
            format!("function {func_name}({params_str}){ret_suffix}")
        }
    }

    /// Extracts the identifier bound by a pattern.
    pub(super) fn pattern_ident(&self, pat_id: PatId) -> Result<String, YulError> {
        let pat = self.expect_pat(pat_id)?;
        match pat {
            Pat::Path(path, _) => self
                .path_ident(*path)
                .ok_or_else(|| YulError::Unsupported("unsupported pattern path".into())),
            _ => Err(YulError::Unsupported(
                "only identifier patterns are supported".into(),
            )),
        }
    }

    /// Resolves an expression that should represent a path (e.g. assignment target).
    pub(super) fn path_from_expr(&self, expr_id: ExprId) -> Result<String, YulError> {
        let expr = self.expect_expr(expr_id)?;
        if let Expr::Path(path) = expr {
            self.path_ident(*path)
                .ok_or_else(|| YulError::Unsupported("unsupported assignment target".into()))
        } else {
            Err(YulError::Unsupported(
                "only identifier assignments are supported".into(),
            ))
        }
    }

    /// Returns the identifier name represented by a path, if it is a plain ident.
    pub(super) fn path_ident(&self, path: Partial<PathId<'_>>) -> Option<String> {
        let path = path.to_opt()?;
        path.as_ident(self.db)
            .map(|id| id.data(self.db).to_string())
    }

    /// Fetches the expression from HIR, converting missing data into `YulError`.
    pub(super) fn expect_expr(&self, expr_id: ExprId) -> Result<&Expr<'db>, YulError> {
        match expr_id.data(self.db, self.body) {
            Partial::Present(expr) => Ok(expr),
            Partial::Absent => Err(YulError::Unsupported("expression data unavailable".into())),
        }
    }

    /// Fetches the pattern from HIR, converting missing data into `YulError`.
    pub(super) fn expect_pat(&self, pat_id: PatId) -> Result<&Pat<'db>, YulError> {
        match pat_id.data(self.db, self.body) {
            Partial::Present(pat) => Ok(pat),
            Partial::Absent => Err(YulError::Unsupported("unsupported pattern".into())),
        }
    }

    /// Fetches the statement from HIR, converting missing data into `YulError`.
    pub(super) fn expect_stmt(&self, stmt_id: StmtId) -> Result<&Stmt<'db>, YulError> {
        match stmt_id.data(self.db, self.body) {
            Partial::Present(stmt) => Ok(stmt),
            Partial::Absent => Err(YulError::Unsupported("statement data unavailable".into())),
        }
    }

    /// Computes the binding name for an effect parameter, following the same fallback
    /// logic used during type checking (explicit name, otherwise the key ident, otherwise `_effect`).
    pub(super) fn effect_binding_name(
        &self,
        effect: hir::core::semantic::EffectParamView<'db>,
    ) -> String {
        if let Some(name) = effect.name(self.db) {
            name.data(self.db).to_string()
        } else if let Some(path) = effect.key_path(self.db)
            && let Some(ident) = path.as_ident(self.db)
        {
            ident.data(self.db).to_string()
        } else {
            "_effect".to_string()
        }
    }

    /// Returns the Yul expression used as the storage base pointer for contract entrypoints.
    pub(super) fn storage_base_ptr_expr(&self) -> &'static str {
        "0"
    }

    /// Computes the byte size of an effect's storage type to space out bindings.
    ///
    /// Returns an error if the effect type cannot be resolved or its size cannot be computed.
    fn effect_size_bytes(
        &self,
        effect: hir::core::semantic::EffectParamView<'db>,
        binding_name: &str,
    ) -> Result<u64, YulError> {
        let key_path = effect.key_path(self.db).ok_or_else(|| {
            YulError::Unsupported(format!(
                "cannot determine storage size for effect `{binding_name}`: missing type path"
            ))
        })?;
        let scope = effect.func().scope();
        let path_res = resolve_path(
            self.db,
            key_path,
            scope,
            PredicateListId::empty_list(self.db),
            false,
        )
        .map_err(|_| {
            YulError::Unsupported(format!(
                "cannot determine storage size for effect `{binding_name}`: failed to resolve type"
            ))
        })?;
        let ty = match path_res {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => ty,
            _ => {
                return Err(YulError::Unsupported(format!(
                    "cannot determine storage size for effect `{binding_name}`: path does not resolve to a type"
                )));
            }
        };
        self.ty_size_bytes(ty).ok_or_else(|| {
            YulError::Unsupported(format!(
                "cannot determine storage size for effect `{binding_name}`: unsupported type"
            ))
        })
    }

    /// Best-effort byte size computation for types used as storage effects.
    fn ty_size_bytes(&self, ty: TyId<'db>) -> Option<u64> {
        if ty.is_tuple(self.db) {
            let mut size = 0u64;
            for field_ty in ty.field_types(self.db) {
                size += self.ty_size_bytes(field_ty)?;
            }
            return Some(size);
        }
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Prim(prim)) => {
                if *prim == PrimTy::Bool {
                    Some(1)
                } else {
                    prim_int_bits(*prim).map(|bits| (bits / 8) as u64)
                }
            }
            TyData::TyBase(TyBase::Adt(adt_def)) => match adt_def.adt_ref(self.db) {
                AdtRef::Struct(_) => {
                    let mut size = 0u64;
                    for field_ty in ty.field_types(self.db) {
                        size += self.ty_size_bytes(field_ty)?;
                    }
                    Some(size)
                }
                _ => None,
            },
            _ => None,
        }
    }
}
