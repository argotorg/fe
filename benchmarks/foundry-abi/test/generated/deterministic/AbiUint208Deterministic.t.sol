// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint208DeterministicTest is AbiRoundtripBase {
    function testEchoUint208Deterministic0() public {
        uint208 value = uint208(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint208.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint208Deterministic1() public {
        uint208 value = uint208(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint208.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint208Deterministic2() public {
        uint208 value = type(uint208).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint208.selector, value);
        assertEquivalent(callData);
    }
}
