// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint16FuzzTest is AbiRoundtripBase {
    function testEchoUint16Fuzz(uint16 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16.selector, value);
        assertEquivalent(callData);
    }
}
