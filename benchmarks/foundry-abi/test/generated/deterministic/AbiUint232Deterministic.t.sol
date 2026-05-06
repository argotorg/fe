// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint232DeterministicTest is AbiRoundtripBase {
    function testEchoUint232Deterministic0() public {
        uint232 value = uint232(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint232.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint232Deterministic1() public {
        uint232 value = uint232(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint232.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint232Deterministic2() public {
        uint232 value = type(uint232).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint232.selector, value);
        assertEquivalent(callData);
    }
}
