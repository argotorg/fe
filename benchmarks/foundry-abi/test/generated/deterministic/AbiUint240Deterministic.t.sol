// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint240DeterministicTest is AbiRoundtripBase {
    function testEchoUint240Deterministic0() public {
        uint240 value = uint240(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint240.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint240Deterministic1() public {
        uint240 value = uint240(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint240.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint240Deterministic2() public {
        uint240 value = type(uint240).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint240.selector, value);
        assertEquivalent(callData);
    }
}
