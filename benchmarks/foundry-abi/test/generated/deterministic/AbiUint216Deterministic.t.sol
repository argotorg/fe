// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint216DeterministicTest is AbiRoundtripBase {
    function testEchoUint216Deterministic0() public {
        uint216 value = uint216(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint216.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint216Deterministic1() public {
        uint216 value = uint216(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint216.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint216Deterministic2() public {
        uint216 value = type(uint216).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint216.selector, value);
        assertEquivalent(callData);
    }
}
