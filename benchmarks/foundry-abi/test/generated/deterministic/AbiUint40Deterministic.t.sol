// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint40DeterministicTest is AbiRoundtripBase {
    function testEchoUint40Deterministic0() public {
        uint40 value = uint40(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint40Deterministic1() public {
        uint40 value = uint40(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint40Deterministic2() public {
        uint40 value = type(uint40).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40.selector, value);
        assertEquivalent(callData);
    }
}
