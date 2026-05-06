// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint256DeterministicTest is AbiRoundtripBase {
    function testEchoUintDeterministic0() public {
        uint256 value = uint256(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUintDeterministic1() public {
        uint256 value = uint256(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUintDeterministic2() public {
        uint256 value = type(uint256).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint.selector, value);
        assertEquivalent(callData);
    }
}
