// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringFuzzTest is AbiRoundtripBase {
    function testEchoStringFuzz(string memory value) public {
        assumeShortString(value);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoString.selector, value);
        assertEquivalent(callData);
    }
}
