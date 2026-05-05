// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint64Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint64Array4Deterministic0() public {
        uint64[4] memory value = [uint64(0), uint64(1), type(uint64).max, uint64(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint64Array4Deterministic1() public {
        uint64[4] memory value = [uint64(1), type(uint64).max, uint64(0), uint64(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64Array4.selector, value);
        assertEquivalent(callData);
    }
}
