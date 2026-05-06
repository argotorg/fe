// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringU64PairArray2DeterministicTest is AbiRoundtripBase {
    function testEchoStringU64PairArray2Deterministic0() public {
        StringU64Pair[2] memory value = [StringU64Pair({text: "pair-one", count: uint64(1)}), StringU64Pair({text: "pair-two", count: uint64(2)})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringU64PairArray2Deterministic1() public {
        StringU64Pair[2] memory value = [StringU64Pair({text: "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", count: type(uint64).max}), StringU64Pair({text: "tail", count: uint64(9)})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray2.selector, value);
        assertEquivalent(callData);
    }
}
