// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint200DeterministicTest is AbiRoundtripBase {
    function testEchoUint200Deterministic0() public {
        uint200 value = uint200(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint200.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint200Deterministic1() public {
        uint200 value = uint200(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint200.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint200Deterministic2() public {
        uint200 value = type(uint200).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint200.selector, value);
        assertEquivalent(callData);
    }
}
