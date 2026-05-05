// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt240DeterministicTest is AbiRoundtripBase {
    function testEchoInt240Deterministic0() public {
        int240 value = int240(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt240.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt240Deterministic1() public {
        int240 value = int240(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt240.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt240Deterministic2() public {
        int240 value = type(int240).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt240.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt240Deterministic3() public {
        int240 value = type(int240).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt240.selector, value);
        assertEquivalent(callData);
    }
}
