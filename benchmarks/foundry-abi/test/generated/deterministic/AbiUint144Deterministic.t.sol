// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint144DeterministicTest is AbiRoundtripBase {
    function testEchoUint144Deterministic0() public {
        uint144 value = uint144(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint144.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint144Deterministic1() public {
        uint144 value = uint144(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint144.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint144Deterministic2() public {
        uint144 value = type(uint144).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint144.selector, value);
        assertEquivalent(callData);
    }
}
