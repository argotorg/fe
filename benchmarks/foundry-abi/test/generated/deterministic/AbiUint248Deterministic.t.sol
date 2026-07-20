// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint248DeterministicTest is AbiRoundtripBase {
    function testEchoUint248Deterministic0() public {
        uint248 value = uint248(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint248Deterministic1() public {
        uint248 value = uint248(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint248Deterministic2() public {
        uint248 value = type(uint248).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248.selector, value);
        assertEquivalent(callData);
    }
}
