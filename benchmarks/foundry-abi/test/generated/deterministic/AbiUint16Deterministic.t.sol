// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint16DeterministicTest is AbiRoundtripBase {
    function testEchoUint16Deterministic0() public {
        uint16 value = uint16(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint16Deterministic1() public {
        uint16 value = uint16(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint16Deterministic2() public {
        uint16 value = type(uint16).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16.selector, value);
        assertEquivalent(callData);
    }
}
