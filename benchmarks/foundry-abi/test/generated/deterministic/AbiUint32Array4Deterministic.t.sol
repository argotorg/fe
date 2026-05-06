// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint32Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint32Array4Deterministic0() public {
        uint32[4] memory value = [uint32(0), uint32(1), type(uint32).max, uint32(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint32Array4Deterministic1() public {
        uint32[4] memory value = [uint32(1), type(uint32).max, uint32(0), uint32(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32Array4.selector, value);
        assertEquivalent(callData);
    }
}
