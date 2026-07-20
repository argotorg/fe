// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint112DeterministicTest is AbiRoundtripBase {
    function testEchoUint112Deterministic0() public {
        uint112 value = uint112(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint112.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint112Deterministic1() public {
        uint112 value = uint112(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint112.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint112Deterministic2() public {
        uint112 value = type(uint112).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint112.selector, value);
        assertEquivalent(callData);
    }
}
