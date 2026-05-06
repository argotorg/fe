// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt16DeterministicTest is AbiRoundtripBase {
    function testEchoInt16Deterministic0() public {
        int16 value = int16(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt16Deterministic1() public {
        int16 value = int16(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt16Deterministic2() public {
        int16 value = type(int16).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt16Deterministic3() public {
        int16 value = type(int16).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16.selector, value);
        assertEquivalent(callData);
    }
}
