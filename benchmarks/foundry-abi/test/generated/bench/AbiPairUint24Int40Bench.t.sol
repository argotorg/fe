// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {Uint24Int40Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairUint24Int40BenchTest is AbiRoundtripBase {
    function testBenchEchoUint24Int40Pair() public {
        Uint24Int40Pair memory value = Uint24Int40Pair({left: uint24(123), right: int40(-7)});
        Uint24Int40Pair memory solValue = solBench.benchEchoUint24Int40Pair(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        Uint24Int40Pair memory feValue = feBench.benchEchoUint24Int40Pair(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
