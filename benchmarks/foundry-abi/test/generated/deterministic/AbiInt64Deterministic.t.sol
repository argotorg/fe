// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt64DeterministicTest is AbiRoundtripBase {
    function testEchoInt64Deterministic0() public {
        int64 value = int64(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt64Deterministic1() public {
        int64 value = int64(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt64Deterministic2() public {
        int64 value = type(int64).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt64Deterministic3() public {
        int64 value = type(int64).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64.selector, value);
        assertEquivalent(callData);
    }
}
