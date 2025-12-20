use driver::DriverDataBase;
use hir::analysis::{
    name_resolution::{PathRes, resolve_path},
    ty::trait_resolution::PredicateListId,
};
use mir::{MirFunction, layout};
use rustc_hash::FxHashMap;

use crate::yul::{doc::YulDoc, errors::YulError, state::BlockState};

use super::util::function_name;

/// Emits Yul for a single MIR function.
pub(super) struct FunctionEmitter<'db> {
    pub(super) db: &'db DriverDataBase,
    pub(super) mir_func: &'db MirFunction<'db>,
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
        if mir_func.func.body(db).is_none() {
            return Err(YulError::MissingBody(function_name(db, mir_func.func)));
        }
        let value_use_counts = Self::collect_value_use_counts(&mir_func.body);
        Ok(Self {
            db,
            mir_func,
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
                    mir::MirInst::LocalInit { value, .. } => {
                        if let Some(value) = value {
                            counts[value.index()] += 1;
                        }
                    }
                    mir::MirInst::LocalSet { value, .. }
                    | mir::MirInst::LocalAugAssign { value, .. }
                    | mir::MirInst::Eval { value, .. }
                    | mir::MirInst::EvalValue { value }
                    | mir::MirInst::BindValue { value } => {
                        counts[value.index()] += 1;
                    }
                    mir::MirInst::IntrinsicStmt { args, .. } => {
                        for arg in args {
                            counts[arg.index()] += 1;
                        }
                    }
                    mir::MirInst::Store { place, value, .. } => {
                        counts[value.index()] += 1;
                        counts[place.base.index()] += 1;
                        for proj in place.projection.iter() {
                            if let hir::projection::Projection::Index(
                                hir::projection::IndexSource::Dynamic(value_id),
                            ) = proj
                            {
                                counts[value_id.index()] += 1;
                            }
                        }
                    }
                    mir::MirInst::InitAggregate { place, inits, .. } => {
                        counts[place.base.index()] += 1;
                        for proj in place.projection.iter() {
                            if let hir::projection::Projection::Index(
                                hir::projection::IndexSource::Dynamic(value_id),
                            ) = proj
                            {
                                counts[value_id.index()] += 1;
                            }
                        }
                        for (path, value) in inits {
                            counts[value.index()] += 1;
                            for proj in path.iter() {
                                if let hir::projection::Projection::Index(
                                    hir::projection::IndexSource::Dynamic(value_id),
                                ) = proj
                                {
                                    counts[value_id.index()] += 1;
                                }
                            }
                        }
                    }
                    mir::MirInst::SetDiscriminant { place, .. } => {
                        counts[place.base.index()] += 1;
                        for proj in place.projection.iter() {
                            if let hir::projection::Projection::Index(
                                hir::projection::IndexSource::Dynamic(value_id),
                            ) = proj
                            {
                                counts[value_id.index()] += 1;
                            }
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
                mir::Terminator::Revert { offset, size } => {
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
        for &local in &self.mir_func.body.param_locals {
            let name = self.mir_func.body.local(local).name.clone();
            params_out.push(name.clone());
            state.insert_local(local, name);
        }
        let mut prologue = Vec::new();
        let is_contract_entry = self.mir_func.contract_function.is_some();
        if is_contract_entry {
            let effect_params: Vec<_> = self.mir_func.func.effect_params(self.db).collect();
            for (effect, &local) in effect_params
                .iter()
                .zip(self.mir_func.body.effect_param_locals.iter())
            {
                let binding = self.mir_func.body.local(local).name.clone();
                let temp = state.alloc_local();
                state.insert_local(local, temp.clone());
                let slots = self.effect_storage_slots(*effect, &binding)?;
                if slots != 0 {
                    return Err(YulError::Unsupported(format!(
                        "contract entrypoint effect `{binding}` must be zero-sized (instantiate storage pointers manually)"
                    )));
                }
                prologue.push(YulDoc::line(format!("let {temp} := 0")));
            }
        } else {
            for &local in &self.mir_func.body.effect_param_locals {
                let binding = self.mir_func.body.local(local).name.clone();
                params_out.push(binding.clone());
                state.insert_local(local, binding);
            }
        }
        Ok((params_out, state, prologue))
    }

    /// Returns true if the Fe function has a return type.
    pub(super) fn returns_value(&self) -> bool {
        let ret_ty = self.mir_func.func.return_ty(self.db);
        (!ret_ty.is_tuple(self.db) || ret_ty.field_count(self.db) != 0) && !ret_ty.is_never(self.db)
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

    /// Returns the Yul expression used as the storage base pointer for contract entrypoints.
    /// Computes the storage slot size of an effect type (for contract entrypoints).
    ///
    /// Returns an error if the effect type cannot be resolved or its size cannot be computed.
    fn effect_storage_slots(
        &self,
        effect: hir::core::semantic::EffectParamView<'db>,
        binding_name: &str,
    ) -> Result<usize, YulError> {
        let key_path = effect.key_path(self.db).ok_or_else(|| {
            YulError::Unsupported(format!(
                "cannot determine storage size for effect `{binding_name}`: missing type path"
            ))
        })?;
        let scope = effect.owner.scope();
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
        layout::ty_storage_slots(self.db, ty).ok_or_else(|| {
            YulError::Unsupported(format!(
                "cannot determine storage size for effect `{binding_name}`: unsupported type"
            ))
        })
    }
}
