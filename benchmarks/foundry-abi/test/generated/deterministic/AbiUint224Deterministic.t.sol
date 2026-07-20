// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint224DeterministicTest is AbiRoundtripBase {
    function testEchoUint224Deterministic0() public {
        uint224 value = uint224(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint224.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint224Deterministic1() public {
        uint224 value = uint224(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint224.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint224Deterministic2() public {
        uint224 value = type(uint224).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint224.selector, value);
        assertEquivalent(callData);
    }
}
