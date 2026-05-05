// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint208FuzzTest is AbiRoundtripBase {
    function testEchoUint208Fuzz(uint208 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint208.selector, value);
        assertEquivalent(callData);
    }
}
