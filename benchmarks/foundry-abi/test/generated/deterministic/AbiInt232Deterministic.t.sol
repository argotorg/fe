// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt232DeterministicTest is AbiRoundtripBase {
    function testEchoInt232Deterministic0() public {
        int232 value = int232(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt232.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt232Deterministic1() public {
        int232 value = int232(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt232.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt232Deterministic2() public {
        int232 value = type(int232).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt232.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt232Deterministic3() public {
        int232 value = type(int232).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt232.selector, value);
        assertEquivalent(callData);
    }
}
