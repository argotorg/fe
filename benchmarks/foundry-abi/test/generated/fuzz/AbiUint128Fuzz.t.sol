// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint128FuzzTest is AbiRoundtripBase {
    function testEchoUint128Fuzz(uint128 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint128.selector, value);
        assertEquivalent(callData);
    }
}
