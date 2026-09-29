Fix `fe build` failing in MIR lowering when emitting an `#[event]` struct with no fields. Such events now emit a LOG1 with `keccak256("Name()")` as the only topic and empty data, matching Solidity.
