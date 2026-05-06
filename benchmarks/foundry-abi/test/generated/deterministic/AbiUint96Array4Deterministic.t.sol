// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint96Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint96Array4Deterministic0() public {
        uint96[4] memory value = [uint96(0), uint96(1), type(uint96).max, uint96(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint96Array4Deterministic1() public {
        uint96[4] memory value = [uint96(1), type(uint96).max, uint96(0), uint96(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96Array4.selector, value);
        assertEquivalent(callData);
    }
}
