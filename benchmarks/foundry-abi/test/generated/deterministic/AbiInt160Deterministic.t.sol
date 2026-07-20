// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt160DeterministicTest is AbiRoundtripBase {
    function testEchoInt160Deterministic0() public {
        int160 value = int160(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt160Deterministic1() public {
        int160 value = int160(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt160Deterministic2() public {
        int160 value = type(int160).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt160Deterministic3() public {
        int160 value = type(int160).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160.selector, value);
        assertEquivalent(callData);
    }
}
