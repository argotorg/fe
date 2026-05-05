// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint88DeterministicTest is AbiRoundtripBase {
    function testEchoUint88Deterministic0() public {
        uint88 value = uint88(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint88.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint88Deterministic1() public {
        uint88 value = uint88(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint88.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint88Deterministic2() public {
        uint88 value = type(uint88).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint88.selector, value);
        assertEquivalent(callData);
    }
}
