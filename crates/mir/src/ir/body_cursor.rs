//! This module provides a collection of structs to modify function body
//! in-place.
// The design used here is greatly inspired by [`cranelift`](https://crates.io/crates/cranelift)

use super::{
    value::AssignableValue, BasicBlock, BasicBlockId, FunctionBody, Inst, InstId, ValueId,
};

/// Specify a current location of [`BodyCursor`]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorLocation {
    Inst(InstId),
    BlockTop(BasicBlockId),
    BlockBottom(BasicBlockId),
    NoWhere,
}

pub struct BodyCursor<'a> {
    body: &'a mut FunctionBody,
    loc: CursorLocation,
}

impl<'a> BodyCursor<'a> {
    pub fn new(body: &'a mut FunctionBody, loc: CursorLocation) -> Self {
        Self { body, loc }
    }

    pub fn new_at_entry(body: &'a mut FunctionBody) -> Self {
        let entry = body.order.entry();
        Self {
            body,
            loc: CursorLocation::BlockTop(entry),
        }
    }
    pub fn set_loc(&mut self, loc: CursorLocation) {
        self.loc = loc;
    }

    pub fn loc(&self) -> CursorLocation {
        self.loc
    }

    pub fn next_loc(&self) -> CursorLocation {
        match self.loc() {
            CursorLocation::Inst(inst) => self.body.order.next_inst(inst).map_or_else(
                || CursorLocation::BlockBottom(self.body.order.inst_block(inst)),
                CursorLocation::Inst,
            ),
            CursorLocation::BlockTop(block) => self
                .body
                .order
                .first_inst(block)
                .map_or_else(|| CursorLocation::BlockBottom(block), CursorLocation::Inst),
            CursorLocation::BlockBottom(block) => self
                .body()
                .order
                .next_block(block)
                .map_or(CursorLocation::NoWhere, |next_block| {
                    CursorLocation::BlockTop(next_block)
                }),
            CursorLocation::NoWhere => CursorLocation::NoWhere,
        }
    }

    pub fn prev_loc(&self) -> CursorLocation {
        match self.loc() {
            CursorLocation::Inst(inst) => self.body.order.prev_inst(inst).map_or_else(
                || CursorLocation::BlockTop(self.body.order.inst_block(inst)),
                CursorLocation::Inst,
            ),
            CursorLocation::BlockTop(block) => self
                .body
                .order
                .prev_block(block)
                .map_or(CursorLocation::NoWhere, |prev_block| {
                    CursorLocation::BlockBottom(prev_block)
                }),
            CursorLocation::BlockBottom(block) => self
                .body
                .order
                .last_inst(block)
                .map_or_else(|| CursorLocation::BlockTop(block), CursorLocation::Inst),
            CursorLocation::NoWhere => CursorLocation::NoWhere,
        }
    }

    pub fn next_block(&self) -> Option<BasicBlockId> {
        let block = self.expect_block();
        self.body.order.next_block(block)
    }

    pub fn prev_block(&self) -> Option<BasicBlockId> {
        let block = self.expect_block();
        self.body.order.prev_block(block)
    }

    pub fn proceed(&mut self) {
        self.set_loc(self.next_loc())
    }

    pub fn back(&mut self) {
        self.set_loc(self.prev_loc());
    }

    pub fn body(&self) -> &FunctionBody {
        self.body
    }

    pub fn body_mut(&mut self) -> &mut FunctionBody {
        self.body
    }

    /// Sets a cursor to an entry block.
    pub fn set_to_entry(&mut self) {
        let entry_bb = self.body().order.entry();
        let loc = CursorLocation::BlockTop(entry_bb);
        self.set_loc(loc);
    }

    /// Insert [`InstId`] to a location where a cursor points.
    /// If you need to store and insert [`Inst`], use [`store_and_insert_inst`].
    ///
    /// # Panics
    /// Panics if a cursor points [`CursorLocation::NoWhere`].
    pub fn insert_inst(&mut self, inst: InstId) {
        match self.loc() {
            CursorLocation::Inst(at) => self.body.order.insert_inst_after(inst, at),
            CursorLocation::BlockTop(block) => self.body.order.prepend_inst(inst, block),
            CursorLocation::BlockBottom(block) => self.body.order.append_inst(inst, block),
            CursorLocation::NoWhere => panic!("cursor loc points to `NoWhere`"),
        }
    }

    pub fn store_and_insert_inst(&mut self, data: Inst) -> InstId {
        let inst = self.body.store.store_inst(data);
        self.insert_inst(inst);
        inst
    }

    /// Remove a current pointed [`Inst`] from a function body. A cursor
    /// proceeds to a next inst.
    ///
    /// # Panics
    /// Panics if a cursor doesn't point [`CursorLocation::Inst`].
    pub fn remove_inst(&mut self) {
        let inst = self.expect_inst();
        let next_loc = self.next_loc();
        self.body.order.remove_inst(inst);
        self.set_loc(next_loc);
    }

    /// Remove a current pointed `block` and contained insts from a function
    /// body. A cursor proceeds to a next block.
    ///
    /// # Panics
    /// Panics if a cursor doesn't point [`CursorLocation::Inst`].
    pub fn remove_block(&mut self) {
        let block = match self.loc() {
            CursorLocation::Inst(inst) => self.body.order.inst_block(inst),
            CursorLocation::BlockTop(block) | CursorLocation::BlockBottom(block) => block,
            CursorLocation::NoWhere => panic!("cursor loc points `NoWhere`"),
        };

        // Store next block of the current block for later use.
        let next_block = self.body.order.next_block(block);

        // Remove all insts in the current block.
        if let Some(first_inst) = self.body.order.first_inst(block) {
            self.set_loc(CursorLocation::Inst(first_inst));
            while matches!(self.loc(), CursorLocation::Inst(..)) {
                self.remove_inst();
            }
        }
        // Remove current block.
        self.body.order.remove_block(block);

        // Set cursor location to next block if exists.
        if let Some(next_block) = next_block {
            self.set_loc(CursorLocation::BlockTop(next_block))
        } else {
            self.set_loc(CursorLocation::NoWhere)
        }
    }

    /// Insert [`BasicBlockId`] to a location where a cursor points.
    /// If you need to store and insert [`BasicBlock`], use
    /// [`store_and_insert_block`].
    ///
    /// # Panics
    /// Panics if a cursor points [`CursorLocation::NoWhere`].
    pub fn insert_block(&mut self, block: BasicBlockId) {
        let current = self.expect_block();
        self.body.order.insert_block_after_block(block, current)
    }

    pub fn store_and_insert_block(&mut self, block: BasicBlock) -> BasicBlockId {
        let block_id = self.body.store.store_block(block);
        self.insert_block(block_id);
        block_id
    }

    pub fn map_result(&mut self, result: AssignableValue) -> Option<ValueId> {
        let inst = self.expect_inst();
        let result_value = result.value_id();
        self.body.store.map_result(inst, result);
        result_value
    }

    /// Returns current inst that cursor points.
    ///
    /// # Panics
    /// Panics if a cursor doesn't point [`CursorLocation::Inst`].
    pub fn expect_inst(&self) -> InstId {
        match self.loc {
            CursorLocation::Inst(inst) => inst,
            _ => panic!("Cursor doesn't point any inst."),
        }
    }

    /// Returns current block that cursor points.
    ///
    /// # Panics
    /// Panics if a cursor points [`CursorLocation::NoWhere`].
    pub fn expect_block(&self) -> BasicBlockId {
        match self.loc {
            CursorLocation::Inst(inst) => self.body.order.inst_block(inst),
            CursorLocation::BlockTop(block) | CursorLocation::BlockBottom(block) => block,
            CursorLocation::NoWhere => panic!("cursor loc points `NoWhere`"),
        }
    }
}
