// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint64FuzzTest is AbiRoundtripBase {
    function testEchoUint64Fuzz(uint64 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64.selector, value);
        assertEquivalent(callData);
    }
}
