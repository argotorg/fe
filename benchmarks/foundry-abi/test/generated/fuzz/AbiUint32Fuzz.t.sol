// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint32FuzzTest is AbiRoundtripBase {
    function testEchoUint32Fuzz(uint32 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32.selector, value);
        assertEquivalent(callData);
    }
}
