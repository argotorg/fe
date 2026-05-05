// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt256DeterministicTest is AbiRoundtripBase {
    function testEchoInt256Deterministic0() public {
        int256 value = int256(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt256Deterministic1() public {
        int256 value = int256(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt256Deterministic2() public {
        int256 value = type(int256).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt256Deterministic3() public {
        int256 value = type(int256).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256.selector, value);
        assertEquivalent(callData);
    }
}
