use fe_analyzer::namespace::{items as analyzer_items, types as analyzer_types};
use fe_common::impl_intern_key;
use fxhash::FxHashMap;
use id_arena::Arena;
use num_bigint::BigInt;
use smol_str::SmolStr;
use std::collections::BTreeMap;

use super::{
    basic_block::BasicBlock,
    body_order::BodyOrder,
    inst::{BranchInfo, Inst, InstId, InstKind},
    types::TypeId,
    value::{AssignableValue, Local, Value, ValueId},
    BasicBlockId, SourceInfo,
};

/// Represents function signature.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,
    pub resolved_generics: BTreeMap<SmolStr, analyzer_types::TypeId>,
    pub return_type: Option<TypeId>,
    pub module_id: analyzer_items::ModuleId,
    pub analyzer_func_id: analyzer_items::FunctionId,
    pub linkage: Linkage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionParam {
    pub name: SmolStr,
    pub ty: TypeId,
    pub source: SourceInfo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub u32);
impl_intern_key!(FunctionId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Linkage {
    /// A function can only be called within the same module.
    Private,

    /// A function can be called from other modules, but can NOT be called from
    /// other accounts and transactions.
    Public,

    /// A function can be called from other modules, and also can be called from
    /// other accounts and transactions.
    Export,
}

impl Linkage {
    pub fn is_exported(self) -> bool {
        self == Linkage::Export
    }
}

/// A function body, which is not stored in salsa db to enable in-place
/// transformation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBody {
    pub fid: FunctionId,

    pub store: BodyDataStore,

    /// Tracks order of basic blocks and instructions in a function body.
    pub order: BodyOrder,

    pub source: SourceInfo,
}

impl FunctionBody {
    pub fn new(fid: FunctionId, source: SourceInfo) -> Self {
        let mut store = BodyDataStore::default();
        let entry_bb = store.store_block(BasicBlock {});
        Self {
            fid,
            store,
            order: BodyOrder::new(entry_bb),
            source,
        }
    }
}

/// A collection of basic block, instructions and values appear in a function
/// body.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct BodyDataStore {
    /// Instructions appear in a function body.
    insts: Arena<Inst>,

    /// All values in a function.
    values: Arena<Value>,

    blocks: Arena<BasicBlock>,

    /// Maps an immediate to a value to ensure the same immediate results in the
    /// same value.
    immediates: FxHashMap<(BigInt, TypeId), ValueId>,

    unit_value: Option<ValueId>,

    /// Maps an instruction to a value.
    inst_results: FxHashMap<InstId, AssignableValue>,

    /// All declared local variables in a function.
    locals: Vec<ValueId>,
}

impl BodyDataStore {
    pub fn store_inst(&mut self, inst: Inst) -> InstId {
        self.insts.alloc(inst)
    }

    pub fn inst_data(&self, inst: InstId) -> &Inst {
        &self.insts[inst]
    }

    pub fn inst_data_mut(&mut self, inst: InstId) -> &mut Inst {
        &mut self.insts[inst]
    }

    pub fn replace_inst(&mut self, inst: InstId, new: Inst) -> Inst {
        let old = &mut self.insts[inst];
        std::mem::replace(old, new)
    }

    pub fn store_value(&mut self, value: Value) -> ValueId {
        match value {
            Value::Immediate { imm, ty } => self.store_immediate(imm, ty),

            Value::Unit { .. } => {
                if let Some(unit_value) = self.unit_value {
                    unit_value
                } else {
                    let unit_value = self.values.alloc(value);
                    self.unit_value = Some(unit_value);
                    unit_value
                }
            }

            Value::Local(ref local) => {
                let is_user_defined = !local.is_tmp;
                let value_id = self.values.alloc(value);
                if is_user_defined {
                    self.locals.push(value_id);
                }
                value_id
            }

            _ => self.values.alloc(value),
        }
    }

    pub fn is_nop(&self, inst: InstId) -> bool {
        matches!(&self.inst_data(inst).kind, InstKind::Nop)
    }

    pub fn is_terminator(&self, inst: InstId) -> bool {
        self.inst_data(inst).is_terminator()
    }

    pub fn branch_info(&self, inst: InstId) -> BranchInfo {
        self.inst_data(inst).branch_info()
    }

    pub fn value_data(&self, value: ValueId) -> &Value {
        &self.values[value]
    }

    pub fn value_data_mut(&mut self, value: ValueId) -> &mut Value {
        &mut self.values[value]
    }

    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.values.iter().map(|(_, value_data)| value_data)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        self.values.iter_mut().map(|(_, value_data)| value_data)
    }

    pub fn store_block(&mut self, block: BasicBlock) -> BasicBlockId {
        self.blocks.alloc(block)
    }

    /// Returns an instruction result
    pub fn inst_result(&self, inst: InstId) -> Option<&AssignableValue> {
        self.inst_results.get(&inst)
    }

    pub fn map_result(&mut self, inst: InstId, result: AssignableValue) {
        self.inst_results.insert(inst, result);
    }

    pub fn remove_inst_result(&mut self, inst: InstId) -> Option<AssignableValue> {
        self.inst_results.remove(&inst)
    }

    pub fn rewrite_branch_dest(&mut self, inst: InstId, from: BasicBlockId, to: BasicBlockId) {
        match &mut self.inst_data_mut(inst).kind {
            InstKind::Jump { dest } => {
                if *dest == from {
                    *dest = to;
                }
            }
            InstKind::Branch { then, else_, .. } => {
                if *then == from {
                    *then = to;
                }
                if *else_ == from {
                    *else_ = to;
                }
            }
            _ => unreachable!("inst is not a branch"),
        }
    }

    pub fn value_ty(&self, vid: ValueId) -> TypeId {
        self.values[vid].ty()
    }

    pub fn locals(&self) -> &[ValueId] {
        &self.locals
    }

    pub fn locals_mut(&mut self) -> &[ValueId] {
        &mut self.locals
    }

    pub fn func_args(&self) -> impl Iterator<Item = ValueId> + '_ {
        self.locals()
            .iter()
            .filter(|value| match self.value_data(**value) {
                Value::Local(local) => local.is_arg,
                _ => unreachable!(),
            })
            .copied()
    }

    pub fn func_args_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        self.values_mut().filter(|value| match value {
            Value::Local(local) => local.is_arg,
            _ => false,
        })
    }

    /// Returns Some(`local_name`) if value is `Value::Local`.
    pub fn local_name(&self, value: ValueId) -> Option<&str> {
        match self.value_data(value) {
            Value::Local(Local { name, .. }) => Some(name),
            _ => None,
        }
    }

    pub fn replace_value(&mut self, value: ValueId, to: Value) -> Value {
        std::mem::replace(&mut self.values[value], to)
    }

    fn store_immediate(&mut self, imm: BigInt, ty: TypeId) -> ValueId {
        if let Some(value) = self.immediates.get(&(imm.clone(), ty)) {
            *value
        } else {
            let id = self.values.alloc(Value::Immediate {
                imm: imm.clone(),
                ty,
            });
            self.immediates.insert((imm, ty), id);
            id
        }
    }
}
