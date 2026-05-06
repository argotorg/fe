// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint80DeterministicTest is AbiRoundtripBase {
    function testEchoUint80Deterministic0() public {
        uint80 value = uint80(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint80.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint80Deterministic1() public {
        uint80 value = uint80(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint80.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint80Deterministic2() public {
        uint80 value = type(uint80).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint80.selector, value);
        assertEquivalent(callData);
    }
}
