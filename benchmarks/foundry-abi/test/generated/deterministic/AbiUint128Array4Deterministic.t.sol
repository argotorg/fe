// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint128Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint128Array4Deterministic0() public {
        uint128[4] memory value = [uint128(0), uint128(1), type(uint128).max, uint128(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint128Array4Deterministic1() public {
        uint128[4] memory value = [uint128(1), type(uint128).max, uint128(0), uint128(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128Array4.selector, value);
        assertEquivalent(callData);
    }
}
