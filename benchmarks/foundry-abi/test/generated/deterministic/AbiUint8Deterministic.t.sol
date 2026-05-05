// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint8DeterministicTest is AbiRoundtripBase {
    function testEchoUint8Deterministic0() public {
        uint8 value = uint8(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint8Deterministic1() public {
        uint8 value = uint8(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint8Deterministic2() public {
        uint8 value = type(uint8).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8.selector, value);
        assertEquivalent(callData);
    }
}
