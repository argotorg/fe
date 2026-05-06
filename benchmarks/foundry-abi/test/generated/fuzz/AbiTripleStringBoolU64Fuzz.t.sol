// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringBoolU64Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleStringBoolU64FuzzTest is AbiRoundtripBase {
    function testEchoStringBoolU64TripleFuzz(string memory text, bool flag, uint64 count) public {
        assumeShortString(text);
        StringBoolU64Triple memory value = StringBoolU64Triple({text: text, flag: flag, count: count});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringBoolU64Triple.selector, value);
        assertEquivalent(callData);
    }
}
