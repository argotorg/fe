// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint200FuzzTest is AbiRoundtripBase {
    function testEchoUint200Fuzz(uint200 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint200.selector, value);
        assertEquivalent(callData);
    }
}
