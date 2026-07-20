// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint240FuzzTest is AbiRoundtripBase {
    function testEchoUint240Fuzz(uint240 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint240.selector, value);
        assertEquivalent(callData);
    }
}
