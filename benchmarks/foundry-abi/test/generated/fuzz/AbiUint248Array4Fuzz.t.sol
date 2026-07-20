// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint248Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint248Array4Fuzz(uint248[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248Array4.selector, value);
        assertEquivalent(callData);
    }
}
