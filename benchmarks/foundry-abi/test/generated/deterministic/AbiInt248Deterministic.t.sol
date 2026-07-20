// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt248DeterministicTest is AbiRoundtripBase {
    function testEchoInt248Deterministic0() public {
        int248 value = int248(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt248Deterministic1() public {
        int248 value = int248(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt248Deterministic2() public {
        int248 value = type(int248).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt248Deterministic3() public {
        int248 value = type(int248).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248.selector, value);
        assertEquivalent(callData);
    }
}
