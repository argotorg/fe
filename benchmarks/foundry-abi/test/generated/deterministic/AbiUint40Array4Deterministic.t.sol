// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint40Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint40Array4Deterministic0() public {
        uint40[4] memory value = [uint40(0), uint40(1), type(uint40).max, uint40(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint40Array4Deterministic1() public {
        uint40[4] memory value = [uint40(1), type(uint40).max, uint40(0), uint40(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40Array4.selector, value);
        assertEquivalent(callData);
    }
}
