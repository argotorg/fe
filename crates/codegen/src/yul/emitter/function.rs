use driver::DriverDataBase;
use hir::analysis::{
    name_resolution::{resolve_path, PathRes},
    ty::trait_resolution::PredicateListId,
};
use mir::{layout, BasicBlockId, MirFunction, Terminator};
use rustc_hash::FxHashMap;

use crate::yul::{doc::YulDoc, errors::YulError, state::BlockState};

use super::util::function_name;

/// Emits Yul for a single MIR function.
pub(super) struct FunctionEmitter<'db> {
    pub(super) db: &'db DriverDataBase,
    pub(super) mir_func: &'db MirFunction<'db>,
    /// Mapping from monomorphized function symbols to code region labels.
    pub(super) code_regions: &'db FxHashMap<String, String>,
    ipdom: Vec<Option<BasicBlockId>>,
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
        let ipdom = compute_immediate_postdominators(&mir_func.body);
        Ok(Self {
            db,
            mir_func,
            code_regions,
            ipdom,
        })
    }

    pub(super) fn ipdom(&self, block: BasicBlockId) -> Option<BasicBlockId> {
        self.ipdom.get(block.index()).copied().flatten()
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
        !layout::is_zero_sized_ty(self.db, ret_ty)
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

    // xxx codegen shouldn't be resolving paths;
    // it should get all info from the MIR representation
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
            PathRes::Trait(_) => return Ok(0),
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

fn compute_immediate_postdominators(body: &mir::MirBody<'_>) -> Vec<Option<BasicBlockId>> {
    let blocks_len = body.blocks.len();
    let exit = blocks_len;
    let node_count = blocks_len + 1;
    let words = node_count.div_ceil(64);
    let last_mask = if node_count.is_multiple_of(64) {
        !0u64
    } else {
        (1u64 << (node_count % 64)) - 1
    };

    fn set_bit(bits: &mut [u64], idx: usize) {
        bits[idx / 64] |= 1u64 << (idx % 64);
    }

    fn clear_bit(bits: &mut [u64], idx: usize) {
        bits[idx / 64] &= !(1u64 << (idx % 64));
    }

    fn has_bit(bits: &[u64], idx: usize) -> bool {
        (bits[idx / 64] & (1u64 << (idx % 64))) != 0
    }

    fn popcount(bits: &[u64]) -> u32 {
        bits.iter().map(|w| w.count_ones()).sum()
    }

    let mut postdom: Vec<Vec<u64>> = vec![vec![0u64; words]; node_count];
    for (idx, p) in postdom.iter_mut().enumerate() {
        if idx == exit {
            set_bit(p, exit);
        } else {
            p.fill(!0u64);
            *p.last_mut().expect("postdom bitset") &= last_mask;
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for b in 0..blocks_len {
            let successors: Vec<usize> = match &body.blocks[b].terminator {
                Terminator::Goto { target } => vec![target.index()],
                Terminator::Branch {
                    then_bb, else_bb, ..
                } => vec![then_bb.index(), else_bb.index()],
                Terminator::Switch {
                    targets, default, ..
                } => {
                    let mut out = Vec::with_capacity(targets.len() + 1);
                    out.extend(targets.iter().map(|t| t.block.index()));
                    out.push(default.index());
                    out
                }
                Terminator::Return(..)
                | Terminator::TerminatingCall(_)
                | Terminator::Unreachable => vec![exit],
            };

            let mut new_bits = vec![!0u64; words];
            new_bits[words - 1] &= last_mask;
            for succ in successors {
                for w in 0..words {
                    new_bits[w] &= postdom[succ][w];
                }
            }
            new_bits[words - 1] &= last_mask;
            set_bit(&mut new_bits, b);

            if new_bits != postdom[b] {
                postdom[b] = new_bits;
                changed = true;
            }
        }
    }

    let mut ipdom = vec![None; blocks_len];
    for b in 0..blocks_len {
        let mut candidates = postdom[b].clone();
        clear_bit(&mut candidates, b);
        clear_bit(&mut candidates, exit);

        let mut best = None;
        let mut best_size = 0u32;
        #[allow(clippy::needless_range_loop)]
        for c in 0..blocks_len {
            if !has_bit(&candidates, c) {
                continue;
            }
            let size = popcount(&postdom[c]);
            if size > best_size || (size == best_size && best.is_some_and(|best| c < best)) {
                best = Some(c);
                best_size = size;
            }
        }
        ipdom[b] = best.map(|idx| BasicBlockId(idx as u32));
    }

    ipdom
}
