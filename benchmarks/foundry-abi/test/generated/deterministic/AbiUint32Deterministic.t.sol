// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint32DeterministicTest is AbiRoundtripBase {
    function testEchoUint32Deterministic0() public {
        uint32 value = uint32(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint32Deterministic1() public {
        uint32 value = uint32(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint32Deterministic2() public {
        uint32 value = type(uint32).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32.selector, value);
        assertEquivalent(callData);
    }
}
