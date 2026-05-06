// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairStringU64DeterministicTest is AbiRoundtripBase {
    function testEchoPairDeterministic0() public {
        StringU64Pair memory value = StringU64Pair({text: "pair payload", count: uint64(42)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoPair.selector, value);
        assertEquivalent(callData);
    }

    function testEchoPairDeterministic1() public {
        StringU64Pair memory value = StringU64Pair({text: "0123456789abcdefghijklmnopqrstuv", count: type(uint64).max});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoPair.selector, value);
        assertEquivalent(callData);
    }
}
