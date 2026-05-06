// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint64Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint64Array4Fuzz(uint64[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint64Array4.selector, value);
        assertEquivalent(callData);
    }
}
