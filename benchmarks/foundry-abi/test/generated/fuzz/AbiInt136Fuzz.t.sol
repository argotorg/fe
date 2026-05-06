// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt136FuzzTest is AbiRoundtripBase {
    function testEchoInt136Fuzz(int136 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt136.selector, value);
        assertEquivalent(callData);
    }
}
