// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint192DeterministicTest is AbiRoundtripBase {
    function testEchoUint192Deterministic0() public {
        uint192 value = uint192(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint192.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint192Deterministic1() public {
        uint192 value = uint192(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint192.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint192Deterministic2() public {
        uint192 value = type(uint192).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint192.selector, value);
        assertEquivalent(callData);
    }
}
