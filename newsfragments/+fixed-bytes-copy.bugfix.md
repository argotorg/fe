`FixedBytes<N>` (`Bytes1` through `Bytes32`) now implements `Copy`, so a `bytesN` value such as a conduit key can be read from a view parameter or used more than once without a move conflict.
