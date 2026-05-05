// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint136DeterministicTest is AbiRoundtripBase {
    function testEchoUint136Deterministic0() public {
        uint136 value = uint136(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint136.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint136Deterministic1() public {
        uint136 value = uint136(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint136.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint136Deterministic2() public {
        uint136 value = type(uint136).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint136.selector, value);
        assertEquivalent(callData);
    }
}
