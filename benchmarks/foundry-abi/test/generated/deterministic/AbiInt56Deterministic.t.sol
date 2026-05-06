// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt56DeterministicTest is AbiRoundtripBase {
    function testEchoInt56Deterministic0() public {
        int56 value = int56(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt56.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt56Deterministic1() public {
        int56 value = int56(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt56.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt56Deterministic2() public {
        int56 value = type(int56).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt56.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt56Deterministic3() public {
        int56 value = type(int56).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt56.selector, value);
        assertEquivalent(callData);
    }
}
