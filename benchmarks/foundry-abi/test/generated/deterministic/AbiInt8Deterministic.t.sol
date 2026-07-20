// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt8DeterministicTest is AbiRoundtripBase {
    function testEchoInt8Deterministic0() public {
        int8 value = int8(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt8Deterministic1() public {
        int8 value = int8(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt8Deterministic2() public {
        int8 value = type(int8).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt8Deterministic3() public {
        int8 value = type(int8).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8.selector, value);
        assertEquivalent(callData);
    }
}
