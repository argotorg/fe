// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint96FuzzTest is AbiRoundtripBase {
    function testEchoUint96Fuzz(uint96 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96.selector, value);
        assertEquivalent(callData);
    }
}
