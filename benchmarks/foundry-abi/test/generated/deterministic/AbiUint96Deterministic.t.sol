// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint96DeterministicTest is AbiRoundtripBase {
    function testEchoUint96Deterministic0() public {
        uint96 value = uint96(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint96Deterministic1() public {
        uint96 value = uint96(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint96Deterministic2() public {
        uint96 value = type(uint96).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96.selector, value);
        assertEquivalent(callData);
    }
}
