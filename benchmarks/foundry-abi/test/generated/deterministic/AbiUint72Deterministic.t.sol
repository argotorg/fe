// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint72DeterministicTest is AbiRoundtripBase {
    function testEchoUint72Deterministic0() public {
        uint72 value = uint72(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint72.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint72Deterministic1() public {
        uint72 value = uint72(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint72.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint72Deterministic2() public {
        uint72 value = type(uint72).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint72.selector, value);
        assertEquivalent(callData);
    }
}
