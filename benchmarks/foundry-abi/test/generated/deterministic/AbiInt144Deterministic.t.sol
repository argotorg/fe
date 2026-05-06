// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt144DeterministicTest is AbiRoundtripBase {
    function testEchoInt144Deterministic0() public {
        int144 value = int144(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt144.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt144Deterministic1() public {
        int144 value = int144(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt144.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt144Deterministic2() public {
        int144 value = type(int144).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt144.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt144Deterministic3() public {
        int144 value = type(int144).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt144.selector, value);
        assertEquivalent(callData);
    }
}
