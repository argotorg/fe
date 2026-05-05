// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint8Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint8Array4Deterministic0() public {
        uint8[4] memory value = [uint8(0), uint8(1), type(uint8).max, uint8(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint8Array4Deterministic1() public {
        uint8[4] memory value = [uint8(1), type(uint8).max, uint8(0), uint8(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8Array4.selector, value);
        assertEquivalent(callData);
    }
}
