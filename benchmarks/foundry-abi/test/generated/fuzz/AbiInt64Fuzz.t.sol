// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt64FuzzTest is AbiRoundtripBase {
    function testEchoInt64Fuzz(int64 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64.selector, value);
        assertEquivalent(callData);
    }
}
