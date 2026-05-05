// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt112FuzzTest is AbiRoundtripBase {
    function testEchoInt112Fuzz(int112 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt112.selector, value);
        assertEquivalent(callData);
    }
}
