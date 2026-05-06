// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringBoolU64Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleStringBoolU64DeterministicTest is AbiRoundtripBase {
    function testEchoStringBoolU64TripleDeterministic0() public {
        StringBoolU64Triple memory value = StringBoolU64Triple({text: "hello", flag: false, count: uint64(1)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringBoolU64Triple.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringBoolU64TripleDeterministic1() public {
        StringBoolU64Triple memory value = StringBoolU64Triple({text: "0123456789abcdefghijklmnopqrstuv", flag: true, count: type(uint64).max});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringBoolU64Triple.selector, value);
        assertEquivalent(callData);
    }
}
