//! The functions native code panics through. Each reports the panic on stderr,
//! flushes the C streams and aborts, so output written before a panic is not
//! lost. A module that traps instead calls none of them.

use smallvec1::smallvec;
use sonatina_ir::{
    BlockId, I256, InlineHint, Linkage, Signature, Type, ValueId,
    builder::FunctionBuilder,
    func_cursor::InstInserter,
    inst::{
        arith::{Add, Mul, Shr, Sub},
        cast::{Bitcast, IntToPtr, Trunc, Zext},
        cmp::{Eq, Gt, IsZero, Lt},
        control_flow::{Br, BrTable, Call, Jump, Phi, Unreachable},
        data::{Alloca, Gep, Mload, Mstore},
        logic::{And, Bswap, Or},
    },
    module::FuncRef,
};

use super::{HostFunction, LowerError, LoweringInstSet, ModuleLowerer};

/// The prefix of the symbols the compiler defines, which `extern` blocks
/// cannot declare.
pub(super) const COMPILER_SYMBOL_PREFIX: &str = "__fe_";

/// How native code ends at a panic or a revert. The EVM ignores it, and only
/// the `cranelift` feature compiles native code.
#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(not(feature = "cranelift"), allow(dead_code))]
pub(crate) enum NativePanics {
    /// Report the panic on stderr and abort.
    Abort,
    /// As `Abort`, except that revert data goes to the host's `__fe_revert`,
    /// as the `fe test` launcher defines it.
    HostRevert,
    /// Trap without a report.
    Trap,
}

/// A function native code panics through.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum NativePanic {
    /// `__fe_trap()`, a panic without data, as `core::panic`.
    Trap,
    /// `__fe_revert(data: *i8, len: i64)`, a revert with `len` bytes of data.
    Revert,
    /// `__fe_panic_code(code: i256)`, a revert with Solidity `Panic(uint256)`
    /// data.
    Code,
}

impl NativePanic {
    pub(super) const ALL: [Self; 3] = [Self::Trap, Self::Revert, Self::Code];

    pub(super) fn symbol(self) -> &'static str {
        match self {
            Self::Trap => "__fe_trap",
            Self::Revert => "__fe_revert",
            Self::Code => "__fe_panic_code",
        }
    }
}

/// The selectors of Solidity `Panic(uint256)` and `Error(string)` data.
const PANIC_SELECTOR: [u8; 4] = [0x4e, 0x48, 0x7b, 0x71];
const ERROR_SELECTOR: [u8; 4] = [0x08, 0xc3, 0x79, 0xa0];

/// The reports of the Solidity panic codes.
const PANIC_REASONS: [(u8, &str); 10] = [
    (0x00, "generic panic"),
    (0x01, "assertion failed"),
    (0x11, "arithmetic overflow"),
    (0x12, "division by zero"),
    (0x21, "invalid enum value"),
    (0x22, "invalid storage encoding"),
    (0x31, "pop from an empty array"),
    (0x32, "index out of bounds"),
    (0x41, "out of memory"),
    (0x51, "call to a zero-initialized function"),
];

/// The size of the stack buffer a report's text is written from.
const TEXT_BUFFER_BYTES: usize = 64;

impl<I: LoweringInstSet + 'static> ModuleLowerer<'_, '_, I> {
    /// The function that reports `panic`, declared on first use, or `None`
    /// when the module traps instead.
    pub(super) fn native_panic_function(&self, panic: NativePanic) -> Option<FuncRef> {
        let linkage = match (self.panics, panic) {
            (NativePanics::Trap, _) => return None,
            (NativePanics::HostRevert, NativePanic::Revert) => Linkage::External,
            _ => Linkage::Private,
        };
        let args = match panic {
            NativePanic::Trap => vec![],
            NativePanic::Revert => vec![self.builder.ptr_type(Type::I8), Type::I64],
            NativePanic::Code => vec![Type::I256],
        };
        let func = self
            .builder
            .declare_function(Signature::new_unit(panic.symbol(), linkage, &args))
            .expect("native panic symbols are reserved");
        self.builder.ctx.set_inline_hint(func, InlineHint::Never);
        Some(func)
    }

    /// Defines the panic functions that the module's bodies call.
    pub(super) fn define_native_panic_functions(&mut self) -> Result<(), LowerError> {
        // `__fe_panic_code` calls `__fe_revert`, so it is defined first.
        for panic in [NativePanic::Code, NativePanic::Trap, NativePanic::Revert] {
            let Some(func) = self.builder.lookup_func(panic.symbol()) else {
                continue;
            };
            if self.builder.sig(func, |sig| sig.linkage()) == Linkage::External {
                continue;
            }
            let mut fb = self.builder.func_builder::<InstInserter>(func);
            let entry = fb.append_block();
            fb.switch_to_block(entry);
            match panic {
                NativePanic::Code => {
                    let revert = self
                        .native_panic_function(NativePanic::Revert)
                        .expect("a module with panic functions does not trap");
                    self.define_panic_code(&mut fb, revert);
                }
                NativePanic::Trap => {
                    let mut report = Report::new(&mut fb, self)?;
                    report.text(b"panicked\n");
                    report.abort();
                }
                NativePanic::Revert => self.define_revert(&mut fb)?,
            }
            fb.seal_all();
            fb.finish();
        }
        Ok(())
    }

    /// `__fe_panic_code(code)` passes `Panic(code)` data to `__fe_revert`.
    fn define_panic_code(&self, fb: &mut FunctionBuilder<InstInserter>, revert: FuncRef) {
        let inst_set = self.inst_set;
        let code = fb.func.arg_values[0];
        let data_ty = fb.declare_array_type(Type::I8, 36);
        let data_ptr_ty = fb.ptr_type(data_ty);
        let data = fb.insert_inst(Alloca::new(inst_set, data_ty), data_ptr_ty);
        let pointer = fb.ptr_type(Type::I8);
        let data = fb.insert_inst(Bitcast::new(inst_set, data, pointer), pointer);
        let selector = fb.make_imm_value(i32::from_le_bytes(PANIC_SELECTOR));
        fb.insert_inst_no_result(Mstore::new(inst_set, data, selector, Type::I32));
        // The code is a big-endian word after the selector.
        let code = fb.insert_inst(Bswap::new(inst_set, code), Type::I256);
        let code_address = offset(fb, inst_set, data, 4);
        fb.insert_inst_no_result(Mstore::new(inst_set, code_address, code, Type::I256));
        let len = fb.make_imm_value(36i64);
        fb.insert_inst_no_result(Call::new(inst_set, revert, smallvec![data, len]));
        fb.insert_inst_no_result(Unreachable::new(inst_set));
    }

    /// `__fe_revert(data, len)` reports `Panic(uint256)` data by its code's
    /// reason, `Error(string)` data by its message, and other data in hex.
    fn define_revert(&self, fb: &mut FunctionBuilder<InstInserter>) -> Result<(), LowerError> {
        let inst_set = self.inst_set;
        let [data, len] = [fb.func.arg_values[0], fb.func.arg_values[1]];
        let mut report = Report::new(fb, self)?;
        let [
            has_selector,
            panic_data,
            panic_word,
            panic_code,
            error_data,
            error_words,
        ] = [(); 6].map(|_| report.fb.append_block());
        let [error_message, other, empty, hex] = [(); 4].map(|_| report.fb.append_block());

        // Data of four bytes or more starts with a selector.
        let three = report.fb.make_imm_value(3i64);
        let long_enough = report
            .fb
            .insert_inst(Gt::new(inst_set, len, three), Type::I1);
        report.branch(long_enough, has_selector, other);

        report.fb.switch_to_block(has_selector);
        let selector = report
            .fb
            .insert_inst(Mload::new(inst_set, data, Type::I32), Type::I32);
        let is_panic = report.eq_imm(selector, i32::from_le_bytes(PANIC_SELECTOR));
        report.branch(is_panic, panic_data, error_data);

        report.fb.switch_to_block(panic_data);
        let is_panic_len = report.eq_imm(len, 36i64);
        report.branch(is_panic_len, panic_word, other);

        report.fb.switch_to_block(panic_word);
        let code = report.word(data, 4);
        let limit = report.fb.make_imm_value(I256::from(256u64));
        let is_byte = report
            .fb
            .insert_inst(Lt::new(inst_set, code, limit), Type::I1);
        report.branch(is_byte, panic_code, other);

        report.fb.switch_to_block(panic_code);
        let code = report
            .fb
            .insert_inst(Trunc::new(inst_set, code, Type::I64), Type::I64);
        let mut cases = Vec::with_capacity(PANIC_REASONS.len());
        let mut reasons = Vec::with_capacity(PANIC_REASONS.len());
        for (code, reason) in PANIC_REASONS {
            let block = report.fb.append_block();
            cases.push((report.fb.make_imm_value(i64::from(code)), block));
            reasons.push((block, reason));
        }
        report
            .fb
            .insert_inst_no_result(BrTable::new(inst_set, code, Some(other), cases));
        for (block, reason) in reasons {
            report.fb.switch_to_block(block);
            report.text(format!("panicked: {reason}\n").as_bytes());
            report.abort();
        }

        // `Error(string)` data is the selector, the offset 32, the message
        // length and the message.
        report.fb.switch_to_block(error_data);
        let is_error = report.eq_imm(selector, i32::from_le_bytes(ERROR_SELECTOR));
        let header = report.fb.make_imm_value(67i64);
        let has_header = report
            .fb
            .insert_inst(Gt::new(inst_set, len, header), Type::I1);
        let is_error = report
            .fb
            .insert_inst(And::new(inst_set, is_error, has_header), Type::I1);
        report.branch(is_error, error_words, other);

        report.fb.switch_to_block(error_words);
        let message_offset = report.word(data, 4);
        let thirty_two = report.fb.make_imm_value(I256::from(32u64));
        let is_offset = report
            .fb
            .insert_inst(Eq::new(inst_set, message_offset, thirty_two), Type::I1);
        let message_len = report.word(data, 36);
        let header = report.fb.make_imm_value(68i64);
        let available = report
            .fb
            .insert_inst(Sub::new(inst_set, len, header), Type::I64);
        let available = report
            .fb
            .insert_inst(Zext::new(inst_set, available, Type::I256), Type::I256);
        let too_long = report
            .fb
            .insert_inst(Gt::new(inst_set, message_len, available), Type::I1);
        let fits = report
            .fb
            .insert_inst(IsZero::new(inst_set, too_long), Type::I1);
        let is_message = report
            .fb
            .insert_inst(And::new(inst_set, is_offset, fits), Type::I1);
        report.branch(is_message, error_message, other);

        report.fb.switch_to_block(error_message);
        report.text(b"panicked: ");
        let message = offset(report.fb, inst_set, data, 68);
        let message_len = report
            .fb
            .insert_inst(Trunc::new(inst_set, message_len, Type::I64), Type::I64);
        report.write(message, message_len);
        report.text(b"\n");
        report.abort();

        report.fb.switch_to_block(other);
        let is_empty = report.eq_imm(len, 0i64);
        report.branch(is_empty, empty, hex);

        report.fb.switch_to_block(empty);
        report.text(b"panicked: reverted without data\n");
        report.abort();

        report.fb.switch_to_block(hex);
        report.text(b"panicked: reverted with 0x");
        report.hex(data, len);
        report.text(b"\n");
        report.abort();
        Ok(())
    }
}

/// Writes a panic report on stderr from a stack buffer.
struct Report<'a, I: 'static> {
    fb: &'a mut FunctionBuilder<InstInserter>,
    inst_set: &'static I,
    /// The text buffer's first byte.
    buffer: ValueId,
    write: FuncRef,
    abort: FuncRef,
}

impl<'a, I: LoweringInstSet + 'static> Report<'a, I> {
    /// Flushes the C streams, so that the report follows the output before
    /// the panic, and allocates the text buffer.
    fn new(
        fb: &'a mut FunctionBuilder<InstInserter>,
        module: &ModuleLowerer<'_, '_, I>,
    ) -> Result<Self, LowerError> {
        let inst_set = module.inst_set;
        let fflush = module.host_function(HostFunction::Fflush)?;
        let write = module.host_function(HostFunction::Write)?;
        let abort = module.host_function(HostFunction::Abort)?;
        // A null stream flushes them all.
        let pointer = fb.ptr_type(Type::I8);
        let null = fb.make_imm_value(0i64);
        let null = fb.insert_inst(IntToPtr::new(inst_set, null, pointer), pointer);
        fb.insert_inst(Call::new(inst_set, fflush, smallvec![null]), Type::I32);
        let buffer_ty = fb.declare_array_type(Type::I8, TEXT_BUFFER_BYTES);
        let buffer_ptr_ty = fb.ptr_type(buffer_ty);
        let buffer = fb.insert_inst(Alloca::new(inst_set, buffer_ty), buffer_ptr_ty);
        let buffer = fb.insert_inst(Bitcast::new(inst_set, buffer, pointer), pointer);
        Ok(Self {
            fb,
            inst_set,
            buffer,
            write,
            abort,
        })
    }

    fn text(&mut self, text: &[u8]) {
        assert!(
            text.len() <= TEXT_BUFFER_BYTES,
            "panic report text is too long"
        );
        for (index, chunk) in text.chunks(32).enumerate() {
            let mut word = [0; 32];
            word[..chunk.len()].copy_from_slice(chunk);
            let value = self.fb.make_imm_value(I256::from_le_bytes(&word));
            let address = offset(self.fb, self.inst_set, self.buffer, 32 * index as i64);
            self.fb
                .insert_inst_no_result(Mstore::new(self.inst_set, address, value, Type::I256));
        }
        let len = self.fb.make_imm_value(text.len() as i64);
        self.write(self.buffer, len);
    }

    /// Writes the `len` bytes at `bytes`. A failed or short write is ignored.
    fn write(&mut self, bytes: ValueId, len: ValueId) {
        let stderr = self.fb.make_imm_value(2i32);
        self.fb.insert_inst(
            Call::new(self.inst_set, self.write, smallvec![stderr, bytes, len]),
            Type::I64,
        );
    }

    /// Writes the `len` bytes at `data` as lowercase hex digits, a full text
    /// buffer at a time.
    fn hex(&mut self, data: ValueId, len: ValueId) {
        let inst_set = self.inst_set;
        let entry = self.fb.current_block().expect("a report is in a block");
        let [header, body, flush, latch, done] = [(); 5].map(|_| self.fb.append_block());
        let zero = self.fb.make_imm_value(0i64);
        self.fb.insert_inst_no_result(Jump::new(inst_set, header));

        self.fb.switch_to_block(header);
        let index = self
            .fb
            .insert_inst(Phi::new(inst_set, smallvec![(zero, entry)]), Type::I64);
        let more = self.fb.insert_inst(Lt::new(inst_set, index, len), Type::I1);
        self.branch(more, body, done);

        self.fb.switch_to_block(body);
        let pointer = self.fb.type_of(data);
        let address = self
            .fb
            .insert_inst(Gep::new(inst_set, smallvec![data, index]), pointer);
        let byte = self
            .fb
            .insert_inst(Mload::new(inst_set, address, Type::I8), Type::I8);
        let four = self.fb.make_imm_value(4i8);
        let high = self
            .fb
            .insert_inst(Shr::new(inst_set, four, byte), Type::I8);
        let mask = self.fb.make_imm_value(15i8);
        let low = self
            .fb
            .insert_inst(And::new(inst_set, byte, mask), Type::I8);
        // The byte's two digits go at twice its place among the buffer's bytes.
        let last_slot = self.fb.make_imm_value(TEXT_BUFFER_BYTES as i64 / 2 - 1);
        let slot = self
            .fb
            .insert_inst(And::new(inst_set, index, last_slot), Type::I64);
        let two = self.fb.make_imm_value(2i64);
        let digits = self
            .fb
            .insert_inst(Mul::new(inst_set, slot, two), Type::I64);
        let pointer = self.fb.type_of(self.buffer);
        let digits = self
            .fb
            .insert_inst(Gep::new(inst_set, smallvec![self.buffer, digits]), pointer);
        for (position, nibble) in [high, low].into_iter().enumerate() {
            let digit = self.hex_digit(nibble);
            let address = offset(self.fb, inst_set, digits, position as i64);
            self.fb
                .insert_inst_no_result(Mstore::new(inst_set, address, digit, Type::I8));
        }
        let one = self.fb.make_imm_value(1i64);
        let next = self
            .fb
            .insert_inst(Add::new(inst_set, index, one), Type::I64);
        // Write the buffer once it is full or the data ends.
        let full = self
            .fb
            .insert_inst(Eq::new(inst_set, slot, last_slot), Type::I1);
        let ends = self.fb.insert_inst(Eq::new(inst_set, next, len), Type::I1);
        let write = self.fb.insert_inst(Or::new(inst_set, full, ends), Type::I1);
        self.branch(write, flush, latch);

        self.fb.switch_to_block(flush);
        let filled = self
            .fb
            .insert_inst(Add::new(inst_set, slot, one), Type::I64);
        let filled = self
            .fb
            .insert_inst(Mul::new(inst_set, filled, two), Type::I64);
        self.write(self.buffer, filled);
        self.fb.insert_inst_no_result(Jump::new(inst_set, latch));

        self.fb.switch_to_block(latch);
        self.fb.append_phi_arg(index, next, latch);
        self.fb.insert_inst_no_result(Jump::new(inst_set, header));

        self.fb.switch_to_block(done);
    }

    /// The ASCII hex digit of `nibble`: `'0' + nibble`, past `'9'` from `'a'`.
    fn hex_digit(&mut self, nibble: ValueId) -> ValueId {
        let inst_set = self.inst_set;
        let nine = self.fb.make_imm_value(9i8);
        let is_letter = self
            .fb
            .insert_inst(Gt::new(inst_set, nibble, nine), Type::I1);
        let is_letter = self
            .fb
            .insert_inst(Zext::new(inst_set, is_letter, Type::I8), Type::I8);
        let gap = self
            .fb
            .make_imm_value(i8::try_from(b'a' - b'9' - 1).unwrap());
        let gap = self
            .fb
            .insert_inst(Mul::new(inst_set, is_letter, gap), Type::I8);
        let zero = self.fb.make_imm_value(b'0');
        let digit = self
            .fb
            .insert_inst(Add::new(inst_set, nibble, zero), Type::I8);
        self.fb
            .insert_inst(Add::new(inst_set, digit, gap), Type::I8)
    }

    /// The big-endian word `offset` bytes into `data`.
    fn word(&mut self, data: ValueId, offset_bytes: i64) -> ValueId {
        let address = offset(self.fb, self.inst_set, data, offset_bytes);
        let word = self
            .fb
            .insert_inst(Mload::new(self.inst_set, address, Type::I256), Type::I256);
        self.fb
            .insert_inst(Bswap::new(self.inst_set, word), Type::I256)
    }

    fn eq_imm(&mut self, value: ValueId, imm: impl Into<sonatina_ir::Immediate>) -> ValueId {
        let imm = self.fb.make_imm_value(imm);
        self.fb
            .insert_inst(Eq::new(self.inst_set, value, imm), Type::I1)
    }

    fn branch(&mut self, cond: ValueId, then: BlockId, otherwise: BlockId) {
        self.fb
            .insert_inst_no_result(Br::new(self.inst_set, cond, then, otherwise));
    }

    fn abort(&mut self) {
        self.fb
            .insert_inst_no_result(Call::new(self.inst_set, self.abort, smallvec![]));
        self.fb
            .insert_inst_no_result(Unreachable::new(self.inst_set));
    }
}

/// The byte `bytes` past the byte pointer `address`.
fn offset<I: LoweringInstSet>(
    fb: &mut FunctionBuilder<InstInserter>,
    inst_set: &'static I,
    address: ValueId,
    bytes: i64,
) -> ValueId {
    if bytes == 0 {
        return address;
    }
    let bytes = fb.make_imm_value(bytes);
    let pointer = fb.type_of(address);
    fb.insert_inst(Gep::new(inst_set, smallvec![address, bytes]), pointer)
}
