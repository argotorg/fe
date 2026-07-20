// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint232FuzzTest is AbiRoundtripBase {
    function testEchoUint232Fuzz(uint232 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint232.selector, value);
        assertEquivalent(callData);
    }
}
