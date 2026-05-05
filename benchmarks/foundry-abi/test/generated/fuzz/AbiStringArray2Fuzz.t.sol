// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringArray2FuzzTest is AbiRoundtripBase {
    function testEchoStringArray2Fuzz(string[2] memory value) public {
        assumeShortString(value[0]);
        assumeShortString(value[1]);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray2.selector, value);
        assertEquivalent(callData);
    }
}
