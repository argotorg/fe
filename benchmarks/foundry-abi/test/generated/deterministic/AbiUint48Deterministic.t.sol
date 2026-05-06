// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint48DeterministicTest is AbiRoundtripBase {
    function testEchoUint48Deterministic0() public {
        uint48 value = uint48(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint48.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint48Deterministic1() public {
        uint48 value = uint48(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint48.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint48Deterministic2() public {
        uint48 value = type(uint48).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint48.selector, value);
        assertEquivalent(callData);
    }
}
