// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt80DeterministicTest is AbiRoundtripBase {
    function testEchoInt80Deterministic0() public {
        int80 value = int80(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt80.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt80Deterministic1() public {
        int80 value = int80(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt80.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt80Deterministic2() public {
        int80 value = type(int80).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt80.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt80Deterministic3() public {
        int80 value = type(int80).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt80.selector, value);
        assertEquivalent(callData);
    }
}
