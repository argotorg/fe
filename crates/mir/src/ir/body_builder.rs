use fe_analyzer::namespace::items::ContractId;
use num_bigint::BigInt;

use crate::ir::{
    body_cursor::{BodyCursor, CursorLocation},
    inst::{BinOp, Inst, InstKind, UnOp},
    value::{AssignableValue, Local},
    BasicBlock, BasicBlockId, FunctionBody, FunctionId, InstId, SourceInfo, TypeId,
};

use super::{
    inst::{CallType, CastKind, SwitchTable, YulIntrinsicOp},
    ConstantId, Value, ValueId,
};

#[derive(Debug)]
pub struct BodyBuilder {
    pub body: FunctionBody,
    loc: CursorLocation,
}

macro_rules! impl_unary_inst {
    ($name:ident, $code:path) => {
        pub fn $name(&mut self, value: ValueId, source: SourceInfo) -> InstId {
            let inst = Inst::unary($code, value, source);
            self.insert_inst(inst)
        }
    };
}

macro_rules! impl_binary_inst {
    ($name:ident, $code:path) => {
        pub fn $name(&mut self, lhs: ValueId, rhs: ValueId, source: SourceInfo) -> InstId {
            let inst = Inst::binary($code, lhs, rhs, source);
            self.insert_inst(inst)
        }
    };
}

impl BodyBuilder {
    pub fn new(fid: FunctionId, source: SourceInfo) -> Self {
        let body = FunctionBody::new(fid, source);
        let entry_block = body.order.entry();
        Self {
            body,
            loc: CursorLocation::BlockTop(entry_block),
        }
    }

    pub fn build(self) -> FunctionBody {
        self.body
    }

    pub fn func_id(&self) -> FunctionId {
        self.body.fid
    }

    pub fn make_block(&mut self) -> BasicBlockId {
        let block = BasicBlock {};
        let block_id = self.body.store.store_block(block);
        self.body.order.append_block(block_id);
        block_id
    }

    pub fn make_value(&mut self, value: impl Into<Value>) -> ValueId {
        self.body.store.store_value(value.into())
    }

    pub fn map_result(&mut self, inst: InstId, result: AssignableValue) {
        self.body.store.map_result(inst, result)
    }

    pub fn inst_result(&mut self, inst: InstId) -> Option<&AssignableValue> {
        self.body.store.inst_result(inst)
    }

    pub fn move_to_block(&mut self, block: BasicBlockId) {
        self.loc = CursorLocation::BlockBottom(block)
    }

    pub fn move_to_block_top(&mut self, block: BasicBlockId) {
        self.loc = CursorLocation::BlockTop(block)
    }

    pub fn make_unit(&mut self, unit_ty: TypeId) -> ValueId {
        self.body.store.store_value(Value::Unit { ty: unit_ty })
    }

    pub fn make_imm(&mut self, imm: BigInt, ty: TypeId) -> ValueId {
        self.body.store.store_value(Value::Immediate { imm, ty })
    }

    pub fn make_imm_from_bool(&mut self, imm: bool, ty: TypeId) -> ValueId {
        if imm {
            self.make_imm(1u8.into(), ty)
        } else {
            self.make_imm(0u8.into(), ty)
        }
    }

    pub fn make_constant(&mut self, constant: ConstantId, ty: TypeId) -> ValueId {
        self.body
            .store
            .store_value(Value::Constant { constant, ty })
    }

    pub fn declare(&mut self, local: Local) -> ValueId {
        let source = local.source.clone();
        let local_id = self.body.store.store_value(Value::Local(local));

        let kind = InstKind::Declare { local: local_id };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst);
        local_id
    }

    pub fn store_func_arg(&mut self, local: Local) -> ValueId {
        self.body.store.store_value(Value::Local(local))
    }

    impl_unary_inst!(not, UnOp::Not);
    impl_unary_inst!(neg, UnOp::Neg);
    impl_unary_inst!(inv, UnOp::Inv);

    impl_binary_inst!(add, BinOp::Add);
    impl_binary_inst!(sub, BinOp::Sub);
    impl_binary_inst!(mul, BinOp::Mul);
    impl_binary_inst!(div, BinOp::Div);
    impl_binary_inst!(modulo, BinOp::Mod);
    impl_binary_inst!(pow, BinOp::Pow);
    impl_binary_inst!(shl, BinOp::Shl);
    impl_binary_inst!(shr, BinOp::Shr);
    impl_binary_inst!(bit_or, BinOp::BitOr);
    impl_binary_inst!(bit_xor, BinOp::BitXor);
    impl_binary_inst!(bit_and, BinOp::BitAnd);
    impl_binary_inst!(logical_and, BinOp::LogicalAnd);
    impl_binary_inst!(logical_or, BinOp::LogicalOr);
    impl_binary_inst!(eq, BinOp::Eq);
    impl_binary_inst!(ne, BinOp::Ne);
    impl_binary_inst!(ge, BinOp::Ge);
    impl_binary_inst!(gt, BinOp::Gt);
    impl_binary_inst!(le, BinOp::Le);
    impl_binary_inst!(lt, BinOp::Lt);

    pub fn primitive_cast(
        &mut self,
        value: ValueId,
        result_ty: TypeId,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::Cast {
            kind: CastKind::Primitive,
            value,
            to: result_ty,
        };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn untag_cast(&mut self, value: ValueId, result_ty: TypeId, source: SourceInfo) -> InstId {
        let kind = InstKind::Cast {
            kind: CastKind::Untag,
            value,
            to: result_ty,
        };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn aggregate_construct(
        &mut self,
        ty: TypeId,
        args: Vec<ValueId>,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::AggregateConstruct { ty, args };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn bind(&mut self, src: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::Bind { src };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn mem_copy(&mut self, src: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::MemCopy { src };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn load(&mut self, src: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::Load { src };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn aggregate_access(
        &mut self,
        value: ValueId,
        indices: Vec<ValueId>,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::AggregateAccess { value, indices };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn map_access(&mut self, value: ValueId, key: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::MapAccess { value, key };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn call(
        &mut self,
        func: FunctionId,
        args: Vec<ValueId>,
        call_type: CallType,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::Call {
            func,
            args,
            call_type,
        };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn keccak256(&mut self, arg: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::Keccak256 { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn abi_encode(&mut self, arg: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::AbiEncode { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn create(&mut self, value: ValueId, contract: ContractId, source: SourceInfo) -> InstId {
        let kind = InstKind::Create { value, contract };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn create2(
        &mut self,
        value: ValueId,
        salt: ValueId,
        contract: ContractId,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::Create2 {
            value,
            salt,
            contract,
        };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn yul_intrinsic(
        &mut self,
        op: YulIntrinsicOp,
        args: Vec<ValueId>,
        source: SourceInfo,
    ) -> InstId {
        let inst = Inst::intrinsic(op, args, source);
        self.insert_inst(inst)
    }

    pub fn jump(&mut self, dest: BasicBlockId, source: SourceInfo) -> InstId {
        let kind = InstKind::Jump { dest };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn branch(
        &mut self,
        cond: ValueId,
        then: BasicBlockId,
        else_: BasicBlockId,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::Branch { cond, then, else_ };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn switch(
        &mut self,
        disc: ValueId,
        table: SwitchTable,
        default: Option<BasicBlockId>,
        source: SourceInfo,
    ) -> InstId {
        let kind = InstKind::Switch {
            disc,
            table,
            default,
        };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn revert(&mut self, arg: Option<ValueId>, source: SourceInfo) -> InstId {
        let kind = InstKind::Revert { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn emit(&mut self, arg: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::Emit { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn ret(&mut self, arg: ValueId, source: SourceInfo) -> InstId {
        let kind = InstKind::Return { arg: arg.into() };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn nop(&mut self, source: SourceInfo) -> InstId {
        let kind = InstKind::Nop;
        let inst = Inst::new(kind, source);
        self.insert_inst(inst)
    }

    pub fn value_ty(&mut self, value: ValueId) -> TypeId {
        self.body.store.value_ty(value)
    }

    pub fn value_data(&mut self, value: ValueId) -> &Value {
        self.body.store.value_data(value)
    }

    /// Returns `true` if current block is terminated.
    pub fn is_block_terminated(&mut self, block: BasicBlockId) -> bool {
        self.body.order.is_terminated(&self.body.store, block)
    }

    pub fn is_current_block_terminated(&mut self) -> bool {
        let current_block = self.current_block();
        self.is_block_terminated(current_block)
    }

    pub fn current_block(&mut self) -> BasicBlockId {
        self.cursor().expect_block()
    }

    pub fn remove_inst(&mut self, inst: InstId) {
        let mut cursor = BodyCursor::new(&mut self.body, CursorLocation::Inst(inst));
        if self.loc == cursor.loc() {
            self.loc = cursor.prev_loc();
        }
        cursor.remove_inst();
    }

    pub fn inst_data(&self, inst: InstId) -> &Inst {
        self.body.store.inst_data(inst)
    }

    fn insert_inst(&mut self, inst: Inst) -> InstId {
        let mut cursor = self.cursor();
        let inst_id = cursor.store_and_insert_inst(inst);

        // Set cursor to the new inst.
        self.loc = CursorLocation::Inst(inst_id);

        inst_id
    }

    fn cursor(&mut self) -> BodyCursor {
        BodyCursor::new(&mut self.body, self.loc)
    }
}
