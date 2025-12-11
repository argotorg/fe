//! Structural hashing utilities shared by the MIR deduplication pass.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;

use hir::analysis::HirAnalysisDb;
use hir::hir_def::{ExprId, PatId};
use hir::projection::Projection;
use num_bigint::BigUint;
use rustc_hash::FxHashMap;

use crate::{
    CallOrigin, MirFunction, MirInst, SwitchValue, Terminator, ValueId, ValueOrigin,
    ir::{Place, SyntheticValue},
};

/// Hashes a MIR function (including its callees) so structurally equivalent bodies
/// produce the same value even if they originated from different instantiations.
pub(crate) fn hash_function<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    symbol_to_idx: &FxHashMap<String, usize>,
    canonical_symbols: &[Option<String>],
) -> u64 {
    let mut hasher = FunctionHasher::new(db, symbol_to_idx, canonical_symbols);
    hasher.hash_function(func);
    hasher.finish()
}

/// Stateful helper that incrementally hashes MIR nodes while de-duplicating IDs.
///
/// Each HIR/MIR node carries arena indices that are unstable across instances.
/// This helper assigns dense, per-function placeholders so hashes capture
/// structure rather than arbitrary allocation order.
struct FunctionHasher<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    hasher: DefaultHasher,
    expr_map: FxHashMap<ExprId, u32>,
    pat_map: FxHashMap<PatId, u32>,
    value_map: FxHashMap<ValueId, u32>,
    next_expr: u32,
    next_pat: u32,
    next_value: u32,
    symbol_to_idx: &'a FxHashMap<String, usize>,
    canonical_symbols: &'a [Option<String>],
}

impl<'db, 'a> FunctionHasher<'db, 'a> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        symbol_to_idx: &'a FxHashMap<String, usize>,
        canonical_symbols: &'a [Option<String>],
    ) -> Self {
        Self {
            db,
            hasher: DefaultHasher::new(),
            expr_map: FxHashMap::default(),
            pat_map: FxHashMap::default(),
            value_map: FxHashMap::default(),
            next_expr: 0,
            next_pat: 0,
            next_value: 0,
            symbol_to_idx,
            canonical_symbols,
        }
    }

    fn finish(self) -> u64 {
        self.hasher.finish()
    }

    /// Record every MIR value and block so the hasher can refer to them by compact IDs.
    fn hash_function(&mut self, func: &MirFunction<'db>) {
        self.value_map.clear();
        for (idx, _) in func.body.values.iter().enumerate() {
            let val = ValueId(idx as u32);
            self.value_map.insert(val, idx as u32);
        }
        self.next_value = func.body.values.len() as u32;

        // Include receiver_space in the hash so functions differing only by
        // address space (e.g. storage_slot_mem vs storage_slot_stor) are not
        // erroneously deduplicated.
        match func.receiver_space {
            Some(crate::ir::AddressSpaceKind::Memory) => self.write_u8(1),
            Some(crate::ir::AddressSpaceKind::Storage) => self.write_u8(2),
            None => self.write_u8(0),
        }

        self.write_usize(func.body.entry.index());
        self.write_usize(func.body.values.len());
        for value in func.body.values.iter() {
            self.hash_value(value);
        }

        self.write_usize(func.body.blocks.len());
        for block in &func.body.blocks {
            self.write_usize(block.insts.len());
            for inst in &block.insts {
                self.hash_inst(inst);
            }
            self.hash_terminator(&block.terminator);
        }
    }

    /// Hash the origin of a MIR value, ignoring type information (handled elsewhere).
    fn hash_value(&mut self, value: &crate::ValueData<'db>) {
        self.write_u8(0x10);
        self.hash_value_origin(&value.origin);
    }

    fn hash_value_origin(&mut self, origin: &ValueOrigin<'db>) {
        match origin {
            ValueOrigin::Expr(expr) => {
                self.write_u8(0x01);
                let expr_slot = self.placeholder_expr(*expr);
                self.write_u32(expr_slot);
            }
            ValueOrigin::Synthetic(SyntheticValue::Int(int)) => {
                self.write_u8(0x02);
                self.hash_biguint(int);
            }
            ValueOrigin::Synthetic(SyntheticValue::Bool(flag)) => {
                self.write_u8(0x03);
                self.write_u8(if *flag { 1 } else { 0 });
            }
            ValueOrigin::Pat(pat) => {
                self.write_u8(0x04);
                let pat_slot = self.placeholder_pat(*pat);
                self.write_u32(pat_slot);
            }
            ValueOrigin::Param(_, idx) => {
                self.write_u8(0x05);
                self.write_usize(*idx);
            }
            ValueOrigin::BindingName(name) => {
                self.write_u8(0x09);
                self.write_str(name);
            }
            ValueOrigin::Call(call) => {
                self.write_u8(0x06);
                self.hash_call_origin(call);
            }
            ValueOrigin::Intrinsic(intr) => {
                self.write_u8(0x07);
                self.write_u8(intr.op as u8);
                self.write_usize(intr.args.len());
                for arg in &intr.args {
                    let slot = self.placeholder_value(*arg);
                    self.write_u32(slot);
                }
            }
            ValueOrigin::FieldPtr(field_ptr) => {
                self.write_u8(0x08);
                let slot = self.placeholder_value(field_ptr.base);
                self.write_u32(slot);
                self.write_u64(field_ptr.offset_bytes as u64);
                self.write_u8(match field_ptr.addr_space {
                    crate::ir::AddressSpaceKind::Memory => 1,
                    crate::ir::AddressSpaceKind::Storage => 2,
                });
            }
            ValueOrigin::PlaceLoad(place) => {
                self.write_u8(0x09);
                self.hash_place(place);
            }
            ValueOrigin::PlaceRef(place) => {
                self.write_u8(0x0A);
                self.hash_place(place);
            }
            ValueOrigin::Alloc {
                size_bytes,
                address_space,
            } => {
                self.write_u8(0x0B);
                self.write_u64(*size_bytes as u64);
                self.write_u8(match address_space {
                    crate::ir::AddressSpaceKind::Memory => 1,
                    crate::ir::AddressSpaceKind::Storage => 2,
                });
            }
        }
    }

    fn hash_place(&mut self, place: &Place<'db>) {
        let slot = self.placeholder_value(place.base);
        self.write_u32(slot);
        // Hash address space (0 for memory, 1 for storage)
        self.write_u8(match place.address_space {
            crate::ir::AddressSpaceKind::Memory => 0,
            crate::ir::AddressSpaceKind::Storage => 1,
        });
        self.write_usize(place.projection.len());
        for proj in place.projection.iter() {
            match proj {
                Projection::Field(idx) => {
                    self.write_u8(0x00);
                    self.write_usize(*idx);
                }
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => {
                    // Include variant index and enum type identity to distinguish
                    // operations on different enum types/variants. While structurally
                    // equivalent code might produce the same Yul, we hash the enum
                    // type to preserve type system semantics in deduplication.
                    self.write_u8(0x01);
                    self.write_str(enum_ty.pretty_print(self.db));
                    self.write_usize(variant.idx as usize);
                    self.write_usize(*field_idx);
                }
                Projection::Discriminant => {
                    self.write_u8(0x02);
                }
                Projection::Index(idx_source) => {
                    self.write_u8(0x03);
                    match idx_source {
                        hir::projection::IndexSource::Constant(idx) => {
                            self.write_u8(0x00);
                            self.write_usize(*idx);
                        }
                        hir::projection::IndexSource::Dynamic(value) => {
                            self.write_u8(0x01);
                            let slot = self.placeholder_value(*value);
                            self.write_u32(slot);
                        }
                    }
                }
                Projection::Deref => {
                    self.write_u8(0x04);
                }
            }
        }
    }

    /// Hashes call metadata, normalising callee symbols via `canonical_symbols`.
    fn hash_call_origin(&mut self, call: &CallOrigin<'db>) {
        self.write_usize(call.args.len());
        for arg in &call.args {
            let slot = self.placeholder_value(*arg);
            self.write_u32(slot);
        }
        self.write_usize(call.effect_args.len());
        for arg in &call.effect_args {
            let slot = self.placeholder_value(*arg);
            self.write_u32(slot);
        }
        self.write_usize(call.effect_kinds.len());
        for kind in &call.effect_kinds {
            self.write_u8(match kind {
                crate::ir::EffectProviderKind::Memory => 1,
                crate::ir::EffectProviderKind::Storage => 2,
                crate::ir::EffectProviderKind::Calldata => 3,
            });
        }
        self.write_usize(call.callable.generic_args().len());
        let symbol = call
            .resolved_name
            .as_ref()
            .and_then(|name| {
                self.symbol_to_idx
                    .get(name)
                    .and_then(|idx| self.canonical_symbols[*idx].as_ref())
            })
            .cloned()
            .or_else(|| call.resolved_name.clone())
            .unwrap_or_else(|| {
                call.callable
                    .callable_def
                    .name(self.db)
                    .map(|n| n.data(self.db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string())
            });
        self.write_str(&symbol);
    }

    /// Hash a MIR instruction, tagging each variant with a unique byte.
    fn hash_inst(&mut self, inst: &MirInst<'db>) {
        match inst {
            MirInst::Let { value, .. } => {
                self.write_u8(0x20);
                if let Some(val) = value {
                    self.write_u8(1);
                    let slot = self.placeholder_value(*val);
                    self.write_u32(slot);
                } else {
                    self.write_u8(0);
                }
            }
            MirInst::Assign { value, .. } => {
                self.write_u8(0x21);
                let slot = self.placeholder_value(*value);
                self.write_u32(slot);
            }
            MirInst::AugAssign { value, op, .. } => {
                self.write_u8(0x22);
                let slot = self.placeholder_value(*value);
                self.write_u32(slot);
                self.write_u8(*op as u8);
            }
            MirInst::Eval { value, .. } => {
                self.write_u8(0x23);
                let slot = self.placeholder_value(*value);
                self.write_u32(slot);
            }
            MirInst::EvalExpr {
                value, bind_value, ..
            } => {
                self.write_u8(0x24);
                let slot = self.placeholder_value(*value);
                self.write_u32(slot);
                self.write_u8(if *bind_value { 1 } else { 0 });
            }
            MirInst::IntrinsicStmt { expr, op, args } => {
                self.write_u8(0x25);
                let expr_slot = self.placeholder_expr(*expr);
                self.write_u32(expr_slot);
                self.write_u8(*op as u8);
                self.write_usize(args.len());
                for arg in args {
                    let slot = self.placeholder_value(*arg);
                    self.write_u32(slot);
                }
            }
            MirInst::Store { expr, place, value } => {
                self.write_u8(0x26);
                let expr_slot = self.placeholder_expr(*expr);
                self.write_u32(expr_slot);
                self.hash_place(place);
                let slot = self.placeholder_value(*value);
                self.write_u32(slot);
            }
            MirInst::SetDiscriminant {
                expr,
                place,
                variant,
            } => {
                self.write_u8(0x27);
                let expr_slot = self.placeholder_expr(*expr);
                self.write_u32(expr_slot);
                self.hash_place(place);
                self.write_usize(variant.idx as usize);
            }
        }
    }

    /// Hash a terminator, including block indices for CFG structure.
    fn hash_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Return(val) => {
                self.write_u8(0x30);
                if let Some(value) = val {
                    self.write_u8(1);
                    let slot = self.placeholder_value(*value);
                    self.write_u32(slot);
                } else {
                    self.write_u8(0);
                }
            }
            Terminator::ReturnData { offset, size } => {
                self.write_u8(0x35);
                let offset_slot = self.placeholder_value(*offset);
                let size_slot = self.placeholder_value(*size);
                self.write_u32(offset_slot);
                self.write_u32(size_slot);
            }
            Terminator::Revert { offset, size } => {
                self.write_u8(0x36);
                let offset_slot = self.placeholder_value(*offset);
                let size_slot = self.placeholder_value(*size);
                self.write_u32(offset_slot);
                self.write_u32(size_slot);
            }
            Terminator::Goto { target } => {
                self.write_u8(0x31);
                self.write_usize(target.index());
            }
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                self.write_u8(0x32);
                let slot = self.placeholder_value(*cond);
                self.write_u32(slot);
                self.write_usize(then_bb.index());
                self.write_usize(else_bb.index());
            }
            Terminator::Switch {
                discr,
                targets,
                default,
                ..
            } => {
                self.write_u8(0x33);
                let slot = self.placeholder_value(*discr);
                self.write_u32(slot);
                self.write_usize(targets.len());
                for target in targets {
                    self.hash_switch_value(&target.value);
                    self.write_usize(target.block.index());
                }
                self.write_usize(default.index());
            }
            Terminator::Unreachable => {
                self.write_u8(0x34);
            }
        }
    }

    /// Hash literal switch values (bool/int/enum discriminant).
    fn hash_switch_value(&mut self, value: &SwitchValue) {
        match value {
            SwitchValue::Bool(flag) => {
                self.write_u8(0x40);
                self.write_u8(if *flag { 1 } else { 0 });
            }
            SwitchValue::Int(int) => {
                self.write_u8(0x41);
                self.hash_biguint(int);
            }
            SwitchValue::Enum(idx) => {
                self.write_u8(0x42);
                self.write_u64(*idx);
            }
        }
    }

    /// Returns a dense placeholder for an expression so hashes do not depend on arena IDs.
    fn placeholder_expr(&mut self, expr: ExprId) -> u32 {
        if let Some(&id) = self.expr_map.get(&expr) {
            id
        } else {
            let id = self.next_expr;
            self.next_expr += 1;
            self.expr_map.insert(expr, id);
            id
        }
    }

    /// Returns a dense placeholder for a pattern.
    fn placeholder_pat(&mut self, pat: PatId) -> u32 {
        if let Some(&id) = self.pat_map.get(&pat) {
            id
        } else {
            let id = self.next_pat;
            self.next_pat += 1;
            self.pat_map.insert(pat, id);
            id
        }
    }

    /// Returns a dense placeholder for a MIR value.
    fn placeholder_value(&mut self, value: ValueId) -> u32 {
        if let Some(&id) = self.value_map.get(&value) {
            id
        } else {
            let id = self.next_value;
            self.next_value += 1;
            self.value_map.insert(value, id);
            id
        }
    }

    /// Write helpers keep the serialization format compact but structured.
    fn write_usize(&mut self, value: usize) {
        self.hasher.write_u64(value as u64);
    }

    fn write_u64(&mut self, value: u64) {
        self.hasher.write_u64(value);
    }

    fn write_u32(&mut self, value: u32) {
        self.hasher.write_u32(value);
    }

    fn write_u8(&mut self, value: u8) {
        self.hasher.write_u8(value);
    }

    fn write_str(&mut self, value: &str) {
        self.hasher.write(value.as_bytes());
    }

    fn hash_biguint(&mut self, value: &BigUint) {
        let bytes = value.to_bytes_be();
        self.write_usize(bytes.len());
        self.hasher.write(&bytes);
    }
}
