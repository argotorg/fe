// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt152DeterministicTest is AbiRoundtripBase {
    function testEchoInt152Deterministic0() public {
        int152 value = int152(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt152.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt152Deterministic1() public {
        int152 value = int152(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt152.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt152Deterministic2() public {
        int152 value = type(int152).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt152.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt152Deterministic3() public {
        int152 value = type(int152).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt152.selector, value);
        assertEquivalent(callData);
    }
}
