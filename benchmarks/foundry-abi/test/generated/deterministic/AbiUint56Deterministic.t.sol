// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint56DeterministicTest is AbiRoundtripBase {
    function testEchoUint56Deterministic0() public {
        uint56 value = uint56(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint56.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint56Deterministic1() public {
        uint56 value = uint56(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint56.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint56Deterministic2() public {
        uint56 value = type(uint56).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint56.selector, value);
        assertEquivalent(callData);
    }
}
