// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint128DeterministicTest is AbiRoundtripBase {
    function testEchoUint128Deterministic0() public {
        uint128 value = uint128(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint128Deterministic1() public {
        uint128 value = uint128(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint128Deterministic2() public {
        uint128 value = type(uint128).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128.selector, value);
        assertEquivalent(callData);
    }
}
