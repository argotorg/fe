use driver::DriverDataBase;
use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::fold::{TyFoldable, TyFolder};
use hir::analysis::ty::normalize::normalize_ty;
use hir::analysis::ty::trait_resolution::PredicateListId;
use hir::analysis::ty::ty_def::{TyData, TyId};
use hir::hir_def::HirIngot;
use hir::hir_def::scope_graph::ScopeId;
use mir::{BasicBlockId, MirFunction, Terminator, layout};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::yul::{doc::YulDoc, errors::YulError, state::BlockState};

use super::util::{escape_yul_reserved, function_name};

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

    /// Produces the final Yul docs for the current MIR function.
    pub(super) fn emit_doc(mut self) -> Result<Vec<YulDoc>, YulError> {
        let func_name = self.mir_func.symbol_name.as_str();
        let (param_names, mut state) = self.init_entry_state();
        let body_docs = self.emit_block(self.mir_func.body.entry, &mut state)?;
        let function_doc = YulDoc::block(
            format!(
                "{} ",
                self.format_function_signature(func_name, &param_names)
            ),
            body_docs,
        );
        Ok(vec![function_doc])
    }

    /// Initializes the `BlockState` with parameter bindings.
    ///
    /// Returns:
    /// - the Yul function parameter names (in signature order)
    /// - the initial block state mapping MIR locals to those names
    fn init_entry_state(&self) -> (Vec<String>, BlockState) {
        let mut state = BlockState::new();
        let mut params_out = Vec::new();
        let mut used_names = FxHashSet::default();
        for &local in &self.mir_func.body.param_locals {
            let raw_name = self.mir_func.body.local(local).name.clone();
            let name = unique_yul_name(&raw_name, &mut used_names);
            params_out.push(name.clone());
            state.insert_local(local, name);
        }
        if self.mir_func.contract_function.is_none() {
            for &local in &self.mir_func.body.effect_param_locals {
                let raw_name = self.mir_func.body.local(local).name.clone();
                let binding = unique_yul_name(&raw_name, &mut used_names);
                params_out.push(binding.clone());
                state.insert_local(local, binding);
            }
        }
        (params_out, state)
    }

    /// Returns true if the Fe function has a return type.
    pub(super) fn returns_value(&self) -> bool {
        let ret_ty = self.mir_func.func.return_ty(self.db);
        // Substitute generic parameters with their concrete arguments for monomorphized functions
        let ret_ty = if !self.mir_func.generic_args.is_empty() {
            let mut folder = ParamSubstFolder {
                args: &self.mir_func.generic_args,
            };
            let substituted = ret_ty.fold_with(self.db, &mut folder);
            // Normalize associated types (e.g., M::Return -> () when M = Increment)
            let scope = normalization_scope_for_args(
                self.db,
                self.mir_func.func,
                &self.mir_func.generic_args,
            );
            let assumptions = PredicateListId::empty_list(self.db);
            normalize_ty(self.db, substituted, scope, assumptions)
        } else {
            ret_ty
        };
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
}

fn unique_yul_name(raw_name: &str, used: &mut FxHashSet<String>) -> String {
    let base = escape_yul_reserved(raw_name);
    let mut candidate = base.clone();
    let mut suffix = 0;
    while used.contains(&candidate) {
        suffix += 1;
        candidate = format!("{base}_{suffix}");
    }
    used.insert(candidate.clone());
    candidate
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
                    let mut s: Vec<_> = targets.iter().map(|t| t.block.index()).collect();
                    s.push(default.index());
                    s
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

/// Type folder that substitutes type parameters with concrete types.
struct ParamSubstFolder<'db, 'a> {
    args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for ParamSubstFolder<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) => self.args.get(param.idx).copied().unwrap_or(ty),
            _ => ty.super_fold_with(db, self),
        }
    }
}

/// Returns a scope suitable for normalizing types with the given generic arguments.
///
/// * `db` - HIR analysis database.
/// * `func` - Function whose scope is used as a fallback.
/// * `args` - Concrete generic argument types.
///
/// Returns the scope to use for type normalization.
fn normalization_scope_for_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: hir::hir_def::Func<'db>,
    args: &[TyId<'db>],
) -> ScopeId<'db> {
    args.iter()
        .find_map(|ty| ty.ingot(db))
        .map(|ingot| ingot.root_mod(db).scope())
        .unwrap_or_else(|| func.scope())
}
