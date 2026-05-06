// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt96DeterministicTest is AbiRoundtripBase {
    function testEchoInt96Deterministic0() public {
        int96 value = int96(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt96Deterministic1() public {
        int96 value = int96(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt96Deterministic2() public {
        int96 value = type(int96).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt96Deterministic3() public {
        int96 value = type(int96).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96.selector, value);
        assertEquivalent(callData);
    }
}
