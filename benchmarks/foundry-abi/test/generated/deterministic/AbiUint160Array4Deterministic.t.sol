// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint160Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint160Array4Deterministic0() public {
        uint160[4] memory value = [uint160(0), uint160(1), type(uint160).max, uint160(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint160Array4Deterministic1() public {
        uint160[4] memory value = [uint160(1), type(uint160).max, uint160(0), uint160(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160Array4.selector, value);
        assertEquivalent(callData);
    }
}
