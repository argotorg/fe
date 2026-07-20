// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint168DeterministicTest is AbiRoundtripBase {
    function testEchoUint168Deterministic0() public {
        uint168 value = uint168(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint168.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint168Deterministic1() public {
        uint168 value = uint168(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint168.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint168Deterministic2() public {
        uint168 value = type(uint168).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint168.selector, value);
        assertEquivalent(callData);
    }
}
