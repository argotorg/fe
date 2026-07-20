// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint112FuzzTest is AbiRoundtripBase {
    function testEchoUint112Fuzz(uint112 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint112.selector, value);
        assertEquivalent(callData);
    }
}
