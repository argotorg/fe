// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint152DeterministicTest is AbiRoundtripBase {
    function testEchoUint152Deterministic0() public {
        uint152 value = uint152(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint152.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint152Deterministic1() public {
        uint152 value = uint152(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint152.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint152Deterministic2() public {
        uint152 value = type(uint152).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint152.selector, value);
        assertEquivalent(callData);
    }
}
