// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt216FuzzTest is AbiRoundtripBase {
    function testEchoInt216Fuzz(int216 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt216.selector, value);
        assertEquivalent(callData);
    }
}
