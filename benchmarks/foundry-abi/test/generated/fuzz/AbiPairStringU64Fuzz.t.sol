// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairStringU64FuzzTest is AbiRoundtripBase {
    function testEchoPairFuzz(string memory text, uint64 count) public {
        assumeShortString(text);
        StringU64Pair memory value = StringU64Pair({text: text, count: count});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoPair.selector, value);
        assertEquivalent(callData);
    }
}
