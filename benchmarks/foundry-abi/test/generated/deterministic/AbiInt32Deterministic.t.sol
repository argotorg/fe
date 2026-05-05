// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt32DeterministicTest is AbiRoundtripBase {
    function testEchoInt32Deterministic0() public {
        int32 value = int32(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt32Deterministic1() public {
        int32 value = int32(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt32Deterministic2() public {
        int32 value = type(int32).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt32Deterministic3() public {
        int32 value = type(int32).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32.selector, value);
        assertEquivalent(callData);
    }
}
