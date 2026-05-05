// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt64Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt64Array4Fuzz(int64[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64Array4.selector, value);
        assertEquivalent(callData);
    }
}
