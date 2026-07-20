// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt160FuzzTest is AbiRoundtripBase {
    function testEchoInt160Fuzz(int160 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160.selector, value);
        assertEquivalent(callData);
    }
}
