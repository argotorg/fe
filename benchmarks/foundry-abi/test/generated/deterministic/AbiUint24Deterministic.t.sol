// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint24DeterministicTest is AbiRoundtripBase {
    function testEchoUint24Deterministic0() public {
        uint24 value = uint24(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint24Deterministic1() public {
        uint24 value = uint24(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint24Deterministic2() public {
        uint24 value = type(uint24).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24.selector, value);
        assertEquivalent(callData);
    }
}
