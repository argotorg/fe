// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint80FuzzTest is AbiRoundtripBase {
    function testEchoUint80Fuzz(uint80 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint80.selector, value);
        assertEquivalent(callData);
    }
}
