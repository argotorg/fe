// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {Uint24Int40Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairUint24Int40Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint24Int40PairArray4() public {
        Uint24Int40Pair[4] memory value = [Uint24Int40Pair({left: uint24(123), right: int40(-7)}), Uint24Int40Pair({left: uint24(0), right: int40(0)}), Uint24Int40Pair({left: type(uint24).max, right: type(int40).min}), Uint24Int40Pair({left: uint24(123), right: int40(-7)})];
        Uint24Int40Pair[4] memory solValue = solBench.benchEchoUint24Int40PairArray4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        Uint24Int40Pair[4] memory feValue = feBench.benchEchoUint24Int40PairArray4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
