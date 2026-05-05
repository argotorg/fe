// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt8FuzzTest is AbiRoundtripBase {
    function testEchoInt8Fuzz(int8 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8.selector, value);
        assertEquivalent(callData);
    }
}
