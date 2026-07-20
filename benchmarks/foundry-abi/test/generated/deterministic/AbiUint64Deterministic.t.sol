// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint64DeterministicTest is AbiRoundtripBase {
    function testEchoUint64Deterministic0() public {
        uint64 value = uint64(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint64Deterministic1() public {
        uint64 value = uint64(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint64Deterministic2() public {
        uint64 value = type(uint64).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64.selector, value);
        assertEquivalent(callData);
    }
}
