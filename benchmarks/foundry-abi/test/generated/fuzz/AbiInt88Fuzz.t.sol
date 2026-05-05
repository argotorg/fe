// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt88FuzzTest is AbiRoundtripBase {
    function testEchoInt88Fuzz(int88 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt88.selector, value);
        assertEquivalent(callData);
    }
}
