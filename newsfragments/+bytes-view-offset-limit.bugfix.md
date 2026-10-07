Reject `bytes` and `string` views whose ABI offset or length word is `2**64` or more, as Solidity does, also when the input reports more than `2**64` bytes.
