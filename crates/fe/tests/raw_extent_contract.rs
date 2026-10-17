use contract_harness::{ExecutionOptions, HarnessError, RuntimeInstance};
use fe::bench_support::compile_fe_sonatina_bytecode;

#[test]
fn checked_slice_reverts_before_accessing_an_invalid_complete_range() {
    let source = r#"
use core::ptr::FixedMemBuffer
msg RangeMsg {
    #[selector = 1]
    Check { len: usize } -> usize,
}
pub contract RangeContract {
    recv RangeMsg {
        Check { len } -> usize {
            let buffer = FixedMemBuffer<32>::alloc()
            buffer.span().slice(offset: 16, len).len()
        }
    }
}
"#;
    let bytecode = compile_fe_sonatina_bytecode(source, "raw_extent", "RangeContract")
        .expect("compile checked range contract");
    let mut runtime = RuntimeInstance::deploy(&hex::encode(bytecode.deploy)).expect("deploy");
    let mut calldata = vec![0u8; 36];
    calldata[3] = 1;
    calldata[35] = 16;
    let valid = runtime
        .call_raw(&calldata, ExecutionOptions::default())
        .expect("complete in-bounds range");
    assert_eq!(valid.return_data.len(), 32);
    assert_eq!(valid.return_data[31], 16);

    calldata[35] = 17;
    let error = runtime
        .call_raw(&calldata, ExecutionOptions::default())
        .expect_err("range crossing the allocation end must fail");
    // Core range checks fail like Solidity index errors: Panic(0x32).
    let mut panic = vec![0x4e, 0x48, 0x7b, 0x71];
    panic.extend([0u8; 31]);
    panic.push(0x32);
    assert!(
        matches!(&error, HarnessError::Revert(data) if data.0 == panic),
        "{error:?}"
    );
}
