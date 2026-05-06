// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt216DeterministicTest is AbiRoundtripBase {
    function testEchoInt216Deterministic0() public {
        int216 value = int216(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt216.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt216Deterministic1() public {
        int216 value = int216(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt216.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt216Deterministic2() public {
        int216 value = type(int216).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt216.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt216Deterministic3() public {
        int216 value = type(int216).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt216.selector, value);
        assertEquivalent(callData);
    }
}
