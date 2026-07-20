// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint8FuzzTest is AbiRoundtripBase {
    function testEchoUint8Fuzz(uint8 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8.selector, value);
        assertEquivalent(callData);
    }
}
