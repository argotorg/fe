// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint128Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint128Array4Fuzz(uint128[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128Array4.selector, value);
        assertEquivalent(callData);
    }
}
