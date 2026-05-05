// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolAddressPairArrayBenchTest is AbiRoundtripBase {
    function testBenchEchoBoolAddressPairArray() public {
        BoolAddressPair[] memory value = new BoolAddressPair[](2);
        value[0] = BoolAddressPair({flag: true, addr: address(0x4000000000000000000000000000000000000004)});
        value[1] = BoolAddressPair({flag: false, addr: address(0x5000000000000000000000000000000000000005)});
        BoolAddressPair[] memory solValue = solBench.benchEchoBoolAddressPairArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        BoolAddressPair[] memory feValue = feBench.benchEchoBoolAddressPairArray(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
