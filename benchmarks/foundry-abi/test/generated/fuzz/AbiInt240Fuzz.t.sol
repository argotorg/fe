// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt240FuzzTest is AbiRoundtripBase {
    function testEchoInt240Fuzz(int240 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt240.selector, value);
        assertEquivalent(callData);
    }
}
