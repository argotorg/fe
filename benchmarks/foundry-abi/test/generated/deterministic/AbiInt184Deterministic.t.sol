// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt184DeterministicTest is AbiRoundtripBase {
    function testEchoInt184Deterministic0() public {
        int184 value = int184(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt184.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt184Deterministic1() public {
        int184 value = int184(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt184.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt184Deterministic2() public {
        int184 value = type(int184).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt184.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt184Deterministic3() public {
        int184 value = type(int184).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt184.selector, value);
        assertEquivalent(callData);
    }
}
