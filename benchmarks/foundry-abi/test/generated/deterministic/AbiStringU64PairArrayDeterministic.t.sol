// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringU64PairArrayDeterministicTest is AbiRoundtripBase {
    function testEchoStringU64PairArrayDeterministic0() public {
        StringU64Pair[] memory value = new StringU64Pair[](0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringU64PairArrayDeterministic1() public {
        StringU64Pair[] memory value = new StringU64Pair[](2);
        value[0] = StringU64Pair({text: "pair-one", count: uint64(1)});
        value[1] = StringU64Pair({text: "pair-two", count: uint64(2)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringU64PairArrayDeterministic2() public {
        StringU64Pair[] memory value = new StringU64Pair[](2);
        value[0] = StringU64Pair({text: "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", count: type(uint64).max});
        value[1] = StringU64Pair({text: "tail", count: uint64(9)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray.selector, value);
        assertEquivalent(callData);
    }
}
