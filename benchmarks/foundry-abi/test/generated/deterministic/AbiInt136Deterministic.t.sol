// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt136DeterministicTest is AbiRoundtripBase {
    function testEchoInt136Deterministic0() public {
        int136 value = int136(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt136.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt136Deterministic1() public {
        int136 value = int136(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt136.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt136Deterministic2() public {
        int136 value = type(int136).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt136.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt136Deterministic3() public {
        int136 value = type(int136).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt136.selector, value);
        assertEquivalent(callData);
    }
}
