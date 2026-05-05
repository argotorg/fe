// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint176DeterministicTest is AbiRoundtripBase {
    function testEchoUint176Deterministic0() public {
        uint176 value = uint176(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint176.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint176Deterministic1() public {
        uint176 value = uint176(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint176.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint176Deterministic2() public {
        uint176 value = type(uint176).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint176.selector, value);
        assertEquivalent(callData);
    }
}
