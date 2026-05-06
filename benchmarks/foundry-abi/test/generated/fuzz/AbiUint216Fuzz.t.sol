// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint216FuzzTest is AbiRoundtripBase {
    function testEchoUint216Fuzz(uint216 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint216.selector, value);
        assertEquivalent(callData);
    }
}
