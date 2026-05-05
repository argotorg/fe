// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt168DeterministicTest is AbiRoundtripBase {
    function testEchoInt168Deterministic0() public {
        int168 value = int168(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt168.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt168Deterministic1() public {
        int168 value = int168(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt168.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt168Deterministic2() public {
        int168 value = type(int168).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt168.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt168Deterministic3() public {
        int168 value = type(int168).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt168.selector, value);
        assertEquivalent(callData);
    }
}
