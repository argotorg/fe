// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt192DeterministicTest is AbiRoundtripBase {
    function testEchoInt192Deterministic0() public {
        int192 value = int192(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt192.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt192Deterministic1() public {
        int192 value = int192(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt192.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt192Deterministic2() public {
        int192 value = type(int192).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt192.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt192Deterministic3() public {
        int192 value = type(int192).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt192.selector, value);
        assertEquivalent(callData);
    }
}
