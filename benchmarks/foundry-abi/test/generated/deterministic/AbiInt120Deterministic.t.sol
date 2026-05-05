// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt120DeterministicTest is AbiRoundtripBase {
    function testEchoInt120Deterministic0() public {
        int120 value = int120(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt120.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt120Deterministic1() public {
        int120 value = int120(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt120.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt120Deterministic2() public {
        int120 value = type(int120).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt120.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt120Deterministic3() public {
        int120 value = type(int120).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt120.selector, value);
        assertEquivalent(callData);
    }
}
