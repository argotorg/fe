// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringU64PairArray2FuzzTest is AbiRoundtripBase {
    function testEchoStringU64PairArray2Fuzz(StringU64Pair[2] memory value) public {
        assumeShortString(value[0].text);
        assumeShortString(value[1].text);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray2.selector, value);
        assertEquivalent(callData);
    }
}
