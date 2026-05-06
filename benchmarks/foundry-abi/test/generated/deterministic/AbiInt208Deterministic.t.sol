// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt208DeterministicTest is AbiRoundtripBase {
    function testEchoInt208Deterministic0() public {
        int208 value = int208(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt208.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt208Deterministic1() public {
        int208 value = int208(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt208.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt208Deterministic2() public {
        int208 value = type(int208).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt208.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt208Deterministic3() public {
        int208 value = type(int208).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt208.selector, value);
        assertEquivalent(callData);
    }
}
