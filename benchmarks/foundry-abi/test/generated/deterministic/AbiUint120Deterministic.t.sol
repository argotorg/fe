// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint120DeterministicTest is AbiRoundtripBase {
    function testEchoUint120Deterministic0() public {
        uint120 value = uint120(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint120.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint120Deterministic1() public {
        uint120 value = uint120(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint120.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint120Deterministic2() public {
        uint120 value = type(uint120).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint120.selector, value);
        assertEquivalent(callData);
    }
}
