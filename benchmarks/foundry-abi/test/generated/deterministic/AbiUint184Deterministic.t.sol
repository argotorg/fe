// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint184DeterministicTest is AbiRoundtripBase {
    function testEchoUint184Deterministic0() public {
        uint184 value = uint184(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint184.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint184Deterministic1() public {
        uint184 value = uint184(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint184.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint184Deterministic2() public {
        uint184 value = type(uint184).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint184.selector, value);
        assertEquivalent(callData);
    }
}
