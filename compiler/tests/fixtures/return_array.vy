contract Foo:
    pub def bar(x: u256) -> u256[5]:
        my_array: u256[5]
        my_array[3] = x
        return my_array
