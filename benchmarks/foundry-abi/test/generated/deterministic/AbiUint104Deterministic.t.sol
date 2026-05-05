// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint104DeterministicTest is AbiRoundtripBase {
    function testEchoUint104Deterministic0() public {
        uint104 value = uint104(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint104.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint104Deterministic1() public {
        uint104 value = uint104(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint104.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint104Deterministic2() public {
        uint104 value = type(uint104).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint104.selector, value);
        assertEquivalent(callData);
    }
}
