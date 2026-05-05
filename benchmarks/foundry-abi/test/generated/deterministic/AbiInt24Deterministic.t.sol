// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt24DeterministicTest is AbiRoundtripBase {
    function testEchoInt24Deterministic0() public {
        int24 value = int24(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt24Deterministic1() public {
        int24 value = int24(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt24Deterministic2() public {
        int24 value = type(int24).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt24Deterministic3() public {
        int24 value = type(int24).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24.selector, value);
        assertEquivalent(callData);
    }
}
