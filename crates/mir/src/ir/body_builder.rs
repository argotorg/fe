use hir::analysis::ty::ty_def::TyId;
use hir::projection::Projection;
use num_bigint::BigUint;

use super::{
    AddressSpaceKind, BasicBlock, BasicBlockId, CodeRegionRoot, LocalData, LocalId, MirBody,
    MirInst, MirProjectionPath, Place, Rvalue, SwitchTarget, SyntheticValue, Terminator, ValueData,
    ValueId, ValueOrigin, ValueRepr,
};

/// Convenience result for `BodyBuilder` helpers that materialize a value in a fresh local.
#[derive(Debug, Clone, Copy)]
pub struct LocalValue {
    pub local: LocalId,
    pub value: ValueId,
}

#[derive(Debug)]
pub struct BodyBuilder<'db> {
    pub body: MirBody<'db>,
    current_block: Option<BasicBlockId>,
}

impl<'db> BodyBuilder<'db> {
    pub fn new() -> Self {
        let mut body = MirBody::new();
        let entry = body.push_block(BasicBlock::new());
        Self {
            body,
            current_block: Some(entry),
        }
    }

    pub fn build(self) -> MirBody<'db> {
        self.body
    }

    pub fn entry_block(&self) -> BasicBlockId {
        self.body.entry
    }

    pub fn current_block(&self) -> Option<BasicBlockId> {
        self.current_block
    }

    pub fn make_block(&mut self) -> BasicBlockId {
        self.body.push_block(BasicBlock::new())
    }

    pub fn move_to_block(&mut self, block: BasicBlockId) {
        self.current_block = Some(block);
    }

    pub fn clear_current_block(&mut self) {
        self.current_block = None;
    }

    pub fn push_inst_in(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.body.block_mut(block).push_inst(inst);
    }

    pub fn push_inst(&mut self, inst: MirInst<'db>) {
        let block = self.current_block.unwrap();
        self.push_inst_in(block, inst);
    }

    pub fn set_block_terminator(&mut self, block: BasicBlockId, term: Terminator<'db>) {
        self.body.block_mut(block).set_terminator(term);
    }

    pub fn terminate_current(&mut self, term: Terminator<'db>) {
        let block = self.current_block.unwrap();
        self.set_block_terminator(block, term);
        self.current_block = None;
    }

    pub fn alloc_local(
        &mut self,
        name: impl Into<String>,
        ty: TyId<'db>,
        is_mut: bool,
        address_space: AddressSpaceKind,
    ) -> LocalId {
        self.body.alloc_local(LocalData {
            name: name.into(),
            ty,
            is_mut,
            address_space,
        })
    }

    pub fn alloc_value(
        &mut self,
        ty: TyId<'db>,
        origin: ValueOrigin<'db>,
        repr: ValueRepr,
    ) -> ValueId {
        self.body.alloc_value(ValueData { ty, origin, repr })
    }

    pub fn unit_value(&mut self, ty: TyId<'db>) -> ValueId {
        self.alloc_value(ty, ValueOrigin::Unit, ValueRepr::Word)
    }

    pub fn const_int_value(&mut self, ty: TyId<'db>, value: BigUint) -> ValueId {
        self.alloc_value(
            ty,
            ValueOrigin::Synthetic(SyntheticValue::Int(value)),
            ValueRepr::Word,
        )
    }

    pub fn const_bool_value(&mut self, ty: TyId<'db>, flag: bool) -> ValueId {
        self.alloc_value(
            ty,
            ValueOrigin::Synthetic(SyntheticValue::Bool(flag)),
            ValueRepr::Word,
        )
    }

    pub fn local_value(&mut self, ty: TyId<'db>, local: LocalId, repr: ValueRepr) -> ValueId {
        self.alloc_value(ty, ValueOrigin::Local(local), repr)
    }

    pub fn func_item_value(&mut self, ty: TyId<'db>, root: CodeRegionRoot<'db>) -> ValueId {
        self.alloc_value(ty, ValueOrigin::FuncItem(root), ValueRepr::Word)
    }

    pub fn assign(&mut self, dest: Option<LocalId>, rvalue: Rvalue<'db>) {
        self.push_inst(MirInst::Assign {
            stmt: None,
            dest,
            rvalue,
        });
    }

    pub fn assign_in(&mut self, block: BasicBlockId, dest: Option<LocalId>, rvalue: Rvalue<'db>) {
        self.push_inst_in(
            block,
            MirInst::Assign {
                stmt: None,
                dest,
                rvalue,
            },
        );
    }

    pub fn store(&mut self, place: Place<'db>, value: ValueId) {
        self.push_inst(MirInst::Store { place, value });
    }

    pub fn store_in(&mut self, block: BasicBlockId, place: Place<'db>, value: ValueId) {
        self.push_inst_in(block, MirInst::Store { place, value });
    }

    pub fn place_field(&self, base: ValueId, field_idx: usize) -> Place<'db> {
        Place::new(
            base,
            MirProjectionPath::from_projection(Projection::Field(field_idx)),
        )
    }

    pub fn store_field(&mut self, base: ValueId, field_idx: usize, value: ValueId) {
        let place = self.place_field(base, field_idx);
        self.store(place, value);
    }

    pub fn store_field_in(
        &mut self,
        block: BasicBlockId,
        base: ValueId,
        field_idx: usize,
        value: ValueId,
    ) {
        let place = self.place_field(base, field_idx);
        self.store_in(block, place, value);
    }

    pub fn goto(&mut self, target: BasicBlockId) {
        self.terminate_current(Terminator::Goto { target });
    }

    pub fn branch(&mut self, cond: ValueId, then_bb: BasicBlockId, else_bb: BasicBlockId) {
        self.terminate_current(Terminator::Branch {
            cond,
            then_bb,
            else_bb,
        });
    }

    pub fn switch(&mut self, discr: ValueId, targets: Vec<SwitchTarget>, default: BasicBlockId) {
        self.terminate_current(Terminator::Switch {
            discr,
            targets,
            default,
        });
    }

    pub fn return_value(&mut self, value: ValueId) {
        self.terminate_current(Terminator::Return(Some(value)));
    }

    pub fn return_unit(&mut self) {
        self.terminate_current(Terminator::Return(None));
    }

    pub fn assign_to_new_local(
        &mut self,
        name: impl Into<String>,
        ty: TyId<'db>,
        is_mut: bool,
        address_space: AddressSpaceKind,
        rvalue: Rvalue<'db>,
    ) -> LocalValue {
        let local = self.alloc_local(name, ty, is_mut, address_space);
        self.assign(Some(local), rvalue);
        let value = self.local_value(ty, local, ValueRepr::Word);
        LocalValue { local, value }
    }

    pub fn assign_to_new_local_in(
        &mut self,
        block: BasicBlockId,
        name: impl Into<String>,
        ty: TyId<'db>,
        is_mut: bool,
        address_space: AddressSpaceKind,
        rvalue: Rvalue<'db>,
    ) -> LocalValue {
        let local = self.alloc_local(name, ty, is_mut, address_space);
        self.assign_in(block, Some(local), rvalue);
        let value = self.local_value(ty, local, ValueRepr::Word);
        LocalValue { local, value }
    }
}

impl<'db> Default for BodyBuilder<'db> {
    fn default() -> Self {
        Self::new()
    }
}
