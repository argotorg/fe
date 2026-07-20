// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt168FuzzTest is AbiRoundtripBase {
    function testEchoInt168Fuzz(int168 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt168.selector, value);
        assertEquivalent(callData);
    }
}
