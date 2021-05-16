
# Release Notes

[🖥️ Download Binaries](https://github.com/ethereum/fe/releases)
[📄 Draft Spec](https://github.com/ethereum/fe/tree/master/spec)
[ℹ️ Getting Started](https://github.com/ethereum/fe#getting-started)

<!-- toc -->

Fe is moving fast. Read up on all the latest improvements.

**WARNING: All Fe releases are alpha releases and only meant to share the development progress with developers and enthusiasts. It is NOT yet ready for production usage.**

[//]: # (towncrier release notes start)
## 0.4.0-alpha (2021-04-28)


### Features


- Support for revert messages in assert statements

  E.g

  ```
  assert a == b, "my revert statement"
  ```

  The provided string is abi-encoded as if it were a call
  to a function `Error(string)`. For example, the revert string `"Not enough Ether provided."` returns the following hexadecimal as error return data:

  ```
  0x08c379a0                                                         // Function selector for Error(string)
  0x0000000000000000000000000000000000000000000000000000000000000020 // Data offset
  0x000000000000000000000000000000000000000000000000000000000000001a // String length
  0x4e6f7420656e6f7567682045746865722070726f76696465642e000000000000 // String data
  ```
  ([#288](https://github.com/ethereum/fe/issues/288))

- Added support for augmented assignments.

  e.g.

  ```
  contract Foo:
      pub def add(a: u256, b: u256) -> u256:
          a += b
          return a

      pub def sub(a: u256, b: u256) -> u256:
          a -= b
          return a

      pub def mul(a: u256, b: u256) -> u256:
          a *= b
          return a

      pub def div(a: u256, b: u256) -> u256:
          a /= b
          return a

      pub def mod(a: u256, b: u256) -> u256:
          a %= b
          return a

      pub def pow(a: u256, b: u256) -> u256:
          a **= b
          return a

      pub def lshift(a: u8, b: u8) -> u8:
          a <<= b
          return a

      pub def rshift(a: u8, b: u8) -> u8:
          a >>= b
          return a

      pub def bit_or(a: u8, b: u8) -> u8:
          a |= b
          return a

      pub def bit_xor(a: u8, b: u8) -> u8:
          a ^= b
          return a

      pub def bit_and(a: u8, b: u8) -> u8:
          a &= b
          return a
  ```
  ([#338](https://github.com/ethereum/fe/issues/338))

- A new parser implementation, which provides more helpful error messages
  with fancy underlines and code context. ([#346](https://github.com/ethereum/fe/issues/346))
- Added support for tuples with base type items.

  e.g.

  ```
  contract Foo:
      my_num: u256

      pub def bar(my_num: u256, my_bool: bool) -> (u256, bool):
          my_tuple: (u256, bool) = (my_num, my_bool)
          self.my_num = my_tuple.item0
          return my_tuple
  ```
  ([#352](https://github.com/ethereum/fe/issues/352))


### Bugfixes


- Properly reject invalid emit ([#211](https://github.com/ethereum/fe/issues/211))
- Properly tokenize numeric literals when they start with 0 ([#331](https://github.com/ethereum/fe/issues/331))
- Reject non-string assert reasons as type error ([#335](https://github.com/ethereum/fe/issues/335))
- Properly reject code that creates a circular dependency when using `create` or `create2`.

  Example, the follwing code is now rightfully rejected because it tries to create an
  instance of `Foo` from within the `Foo` contract itself.

  ```
  contract Foo:
    pub def bar()->address:
      foo:Foo=Foo.create(0)

      return address(foo)
  ```
  ([#362](https://github.com/ethereum/fe/issues/362))


### Internal Changes - for Fe Contributors


- AST nodes use `String`s instead of `&str`s. This way we can perform incremental compilation on the AST. ([#332](https://github.com/ethereum/fe/issues/332))
- Added support for running tests against solidity fixtures.
  Also added tests that cover how solidity encodes revert reason strings. ([#342](https://github.com/ethereum/fe/issues/342))
- Refactoring of binary operation type checking. ([#347](https://github.com/ethereum/fe/issues/347))


## 0.3.0-alpha "Calamine" (2021-03-24)


### Features


- Add over/underflow checks for multiplications of all integers ([#271](https://github.com/ethereum/fe/issues/271))
- Add full support for empty Tuples. ([#276](https://github.com/ethereum/fe/issues/276))

  All functions in Fe implicitly return an empty Tuple if they have no other return value.
  However, before this change one was not able to use the empty Tuple syntax `()` explicitly.

  With this change, all of these are treated equally:

  ```
  contract Foo:

    pub def explicit_return_a1():
      return

    pub def explicit_return_a2():
      return ()

    pub def explicit_return_b1() ->():
      return

    pub def explicit_return_b2() ->():
      return ()

    pub def implicit_a1():
      pass

    pub def implicit_a2() ->():
      pass
  ```
  
- The JSON ABI builder now supports structs as both input and output. ([#296](https://github.com/ethereum/fe/issues/296))
- Make subsequently defined contracts visible.

  Before this change:

  ```
  # can't see Bar
  contract Foo:
     ...
  # can see Foo
  contract Bar:
     ...
  ```

  With this change the restriction is lifted and the following becomes possible. ([#298](https://github.com/ethereum/fe/issues/298))

  ```
  contract Foo:
      bar: Bar
      pub def external_bar() -> u256:
          return self.bar.bar()
  contract Bar:
      foo: Foo
      pub def external_foo() -> u256:
          return self.foo.foo()
  ```

- Perform checks for divison operations on integers ([#308](https://github.com/ethereum/fe/issues/308))
- Support for msg.sig to read the function identifier. ([#311](https://github.com/ethereum/fe/issues/311))
- Perform checks for modulo operations on integers ([#312](https://github.com/ethereum/fe/issues/312))
- Perform over/underflow checks for exponentiation operations on integers ([#313](https://github.com/ethereum/fe/issues/313))


### Bugfixes


- Properly reject `emit` not followed by an event invocation ([#212](https://github.com/ethereum/fe/issues/212))
- Properly reject octal number literals ([#222](https://github.com/ethereum/fe/issues/222))
- Properly reject code that tries to emit a non-existing event. ([#250](https://github.com/ethereum/fe/issues/250))

  Example that now produces a compile time error:

  ```
  emit DoesNotExist()
  ```
- Contracts that create other contracts can now include `__init__` functions.

  See https://github.com/ethereum/fe/issues/284 ([#304](https://github.com/ethereum/fe/issues/304))
- Prevent multiple types with same name in one module. ([#317](https://github.com/ethereum/fe/issues/317))

  Examples that now produce compile time errors:

  ```
  type bar = u8
  type bar = u16
  ```

  or

  ```
  struct SomeStruct:
      some_field: u8

  struct SomeStruct:
      other: u8
  ```

  or

  ```
  contract SomeContract:
      some_field: u8

  contract SomeContract:
      other: u8
  ```


  Prevent multiple fields with same name in one struct.

  Example that now produces a compile time error:

  ```
  struct SomeStruct:
      some_field: u8
      some_field: u8
  ```


  Prevent variable definition in child scope when name already taken in parent scope.

  Example that now produces a compile time error:

  ```
  pub def bar():
      my_array: u256[3]
      sum: u256 = 0
      for i in my_array:
          sum: u256 = 0
  ```
- The CLI was using the overwrite flag to enable Yul optimization.

  i.e.

  ```
  # Would both overwite output files and run the Yul optimizer. 
  $ fe my_contract.fe --overwrite
  ```


  Using the overwrite flag now only overwrites and optimization is enabled with the optimize flag. ([#320](https://github.com/ethereum/fe/issues/320))
- Ensure analyzer rejects code that uses return values for `__init__` functions. ([#323](https://github.com/ethereum/fe/issues/323))

  An example that now produces a compile time error:

  ```
  contract C:
      pub def __init__() -> i32:
          return 0
  ```
- Properly reject calling an undefined function on an external contract ([#324](https://github.com/ethereum/fe/issues/324))


### Internal Changes - for Fe Contributors


- Added the Uniswap demo contracts to our testing fixtures and validated their behaviour. ([#179](https://github.com/ethereum/fe/issues/179))
- IDs added to AST nodes. ([#315](https://github.com/ethereum/fe/issues/315))
- Failures in the Yul generation phase now panic; any failure is a bug. ([#327](https://github.com/ethereum/fe/issues/327))


## 0.2.0-alpha "Borax" (2021-02-27)


### Features


- Add support for string literals.

  Example:

  ```
  def get_ticker_symbol() -> string3:
      return "ETH"
  ```

  String literals are stored in and loaded from the compiled bytecode. ([#186](https://github.com/ethereum/fe/issues/186))
- The CLI now compiles every contract in a module, not just the first one. ([#197](https://github.com/ethereum/fe/issues/197))

  Sample compiler output with all targets enabled:

  ```
  output
  |-- Bar
  |   |-- Bar.bin
  |   |-- Bar_abi.json
  |   `-- Bar_ir.yul
  |-- Foo
  |   |-- Foo.bin
  |   |-- Foo_abi.json
  |   `-- Foo_ir.yul
  |-- module.ast
  `-- module.tokens
  ```

- Add support for string type casts ([#201](https://github.com/ethereum/fe/issues/201))

  Example:

  ```
  val: string100 = string100("foo")
  ```
- Add basic support for structs. ([#203](https://github.com/ethereum/fe/issues/203))

  Example:

  ```
  struct House:
      price: u256
      size: u256
      vacant: bool

  contract City:

      pub def get_price() -> u256:
          building: House = House(300, 500, true)

          assert building.size == 500
          assert building.price == 300
          assert building.vacant

          return building.price
  ``` 
- Added support for external contract calls. Contract definitions now 
  add a type to the module scope, which may be used to create contract 
  values with the contract's public functions as callable attributes. ([#204](https://github.com/ethereum/fe/issues/204))

  Example:

  ```python
  contract Foo:
      pub def build_array(a: u256, b: u256) -> u256[3]:
          my_array: u256[3]
          my_array[0] = a
          my_array[1] = a * b
          my_array[2] = b
          return my_array

  contract FooProxy:
      pub def call_build_array(
          foo_address: address,
          a: u256,
          b: u256,
      ) -> u256[3]:
          foo: Foo = Foo(foo_address)
          return foo.build_array(a, b)
  ```
- Add support for `block`, `msg`, `chain`, and `tx` properties: ([#208](https://github.com/ethereum/fe/issues/208))
  ```
  block.coinbase: address
  block.difficulty: u256
  block.number: u256
  block.timestamp: u256
  chain.id: u256
  msg.value: u256
  tx.gas_price: u256
  tx.origin: address
  ```
  (Note that `msg.sender: address` was added previously.)

  Example:
  ```
  def post_fork() -> bool:
      return block.number > 2675000
  ```
- The CLI now panics if an error is encountered during Yul compilation. ([#218](https://github.com/ethereum/fe/issues/218))
- Support for contract creations.

  Example of `create2`, which takes a `value` and address `salt` as parameters.

  ```
  contract Foo:
      pub def get_my_num() -> u256:
          return 42

  contract FooFactory:
      pub def create2_foo() -> address:
          # value and salt
          foo: Foo = Foo.create2(0, 52)
          return address(foo)
  ```

  Example of `create`, which just takes a `value` parameter.

  ```
  contract Foo:
      pub def get_my_num() -> u256:
          return 42

  contract FooFactory:
      pub def create_foo() -> address:
          # value and salt
          foo: Foo = Foo.create(0)
          return address(foo)
  ```

  *Note: We do not yet support init parameters.* ([#239](https://github.com/ethereum/fe/issues/239))
- Support updating individual struct fields in storage. ([#246](https://github.com/ethereum/fe/issues/246))

  Example:

  ```
   pub def update_house_price(price: u256):
          self.my_house.price = price
  ``` 
- Implement global `keccak256` method. The method expects one parameter of `bytes[n]`
  and returns the hash as an `u256`. In a future version `keccak256` will most likely
  be moved behind an import so that it has to be imported (e.g. `from std.crypto import keccak256`). ([#255](https://github.com/ethereum/fe/issues/255))

  Example:

  ```
  pub def hash_single_byte(val: bytes[1]) -> u256:
      return keccak256(val)
  ```
- Require structs to be initialized using keyword arguments.

  Example:

  ```
  struct House:
      vacant: bool
      price: u256
  ```

  Previously, `House` could be instantiated as `House(true, 1000000)`.
  With this change it is required to be instantiated like `House(vacant=true, price=1000000)`

  This ensures property assignment is less prone to get mixed up. It also makes struct
  initialization visually stand out more from function calls. ([#260](https://github.com/ethereum/fe/issues/260))
- Implement support for boolean `not` operator. ([#264](https://github.com/ethereum/fe/issues/264))

  Example:

  ```
  if not covid_test.is_positive(person):
      allow_boarding(person)
  ```
- Do over/underflow checks for additions (SafeMath).

  With this change all additions (e.g `x + y`) for signed and unsigned
  integers check for over- and underflows and revert if necessary. ([#265](https://github.com/ethereum/fe/issues/265))
- Added a builtin function `abi_encode()` that can be used to encode stucts. The return type is a 
  fixed-size array of bytes that is equal in size to the encoding. The type system does not support 
  dynamically-sized arrays yet, which is why we used fixed. ([#266](https://github.com/ethereum/fe/issues/266))

  Example:

  ```
  struct House:
      price: u256
      size: u256
      rooms: u8
      vacant: bool
    
  contract Foo:
      pub def hashed_house() -> u256:
          house: House = House(
              price=300,
              size=500,
              rooms=u8(20),
              vacant=true
          )
          return keccak256(house.abi_encode())
  ```
- Perform over/underflow checks for subtractions (SafeMath). ([#267](https://github.com/ethereum/fe/issues/267))

  With this change all subtractions (e.g `x - y`) for signed and unsigned
  integers check for over- and underflows and revert if necessary. 
- Support for the boolean operations `and` and `or`. ([#270](https://github.com/ethereum/fe/issues/270))

  Examples:

  ```
  contract Foo:
      pub def bar(x: bool, y: bool) -> bool:
          return x and y
  ```

  ```
  contract Foo:
      pub def bar(x: bool, y: bool) -> bool:
          return x or y
  ```

  Support for `self.address`.

  This expression returns the address of the current contract.

  Example:

  ```
  contract Foo:
      pub def bar() -> address:
          return self.address
  ```


### Bugfixes


- Perform type checking when calling event constructors

  Previously, the following would not raise an error even though it should:

  ```
  contract Foo:
      event MyEvent:
          val_1: string100
          val_2: u8

      pub def foo():
          emit MyEvent("foo", 1000)

  ```

  Wit this change, the code fails with a type error as expected. ([#202](https://github.com/ethereum/fe/issues/202))
- Fix bug where compilation of contracts without public functions would result in illegal YUL. ([#219](https://github.com/ethereum/fe/issues/219))

  E.g without this change, the following doesn't compile to proper YUL

  ```
  contract Empty:
    lonely: u256
  ```
- Ensure numeric literals can't exceed 256 bit range. Previously, this would result in a
  non user friendly error at the YUL compilation stage. With this change it is caught
  at the analyzer stage and presented to the user as a regular error. ([#225](https://github.com/ethereum/fe/issues/225))
- Fix crash when return is used without value.

  These two methods should both be treated as returning `()`

  ```
    pub def explicit_return():
      return

    pub def implicit():
      pass
  ```

  Without this change, the `explicit_return` crashes the compiler. ([#261](https://github.com/ethereum/fe/issues/261))


### Internal Changes - for Fe Contributors


- Renamed the fe-semantics library to fe-analyzer. ([#207](https://github.com/ethereum/fe/issues/207))
- Runtime testing utilities. ([#243](https://github.com/ethereum/fe/issues/243))
- Values are stored more efficiently in storage. ([#251](https://github.com/ethereum/fe/issues/251))


## 0.1.0-alpha "Amethyst" (2021-01-20)

**WARNING: This is an alpha version to share the development progress with developers and enthusiasts. It is NOT yet intended to be used for anything serious. At this point Fe is missing a lot of features and has a lot of bugs instead.**

This is the first **alpha** release and kicks off our release schedule which will be one release every month in the future. Since we have just started tracking progress on changes, the following list of changes is incomplete, but will appropriately document progress between releases from now on.

### Features


- Added support for `for loop`, allows iteration over static arrays. ([#134](https://github.com/ethereum/fe/issues/134))
- Enforce bounds on numeric literals in type constructors.

  For instance calling `u8(1000)` or `i8(-250)` will give an error because
  the literals `1000` and `-250` do not fit into `u8` or `i8`. ([#145](https://github.com/ethereum/fe/issues/145))
- Added builtin copying methods `clone()` and `to_mem()` to reference types. ([#155](https://github.com/ethereum/fe/issues/155))


  usage:

  ```
  # copy a segment of storage into memory and assign the new pointer
  my_mem_array = self.my_sto_array.to_mem()

  # copy a segment of memory into another segment of memory and assign the new pointer
  my_other_mem_array = my_mem_array.clone()
  ```
- Support emitting JSON ABI via `--emit abi`.
  The default value of `--emit` is now `abi,bytecode`. ([#160](https://github.com/ethereum/fe/issues/160))
- Ensure integer type constructor reject all expressions that aren't a numeric literal.
  For instance, previously the compiler would not reject the following code even though it could not be guaranteed that `val` would fit into an `u16`.

  ```
  pub def bar(val: u8) -> u16:
          return u16(val)
  ```

  Now such code is rejected and integer type constructor do only work with numeric literals such as `1` or `-3`. ([#163](https://github.com/ethereum/fe/issues/163))
- Support for ABI decoding of all array type. ([#172](https://github.com/ethereum/fe/issues/172))
- Support for value assignments in declaration.

  Previously, this code would fail:

  ```
  another_reference: u256[10] = my_array
  ```

  As a workaround declaration and assignment could be split apart.

  ```
  another_reference: u256[10]
  another_reference = my_array
  ```

  With this change, the shorter declaration with assignment syntax is supported. ([#173](https://github.com/ethereum/fe/issues/173))


### Improved Documentation


- Point to examples in the README ([#162](https://github.com/ethereum/fe/issues/162))
- Overhaul README page to better reflect the current state of the project. ([#177](https://github.com/ethereum/fe/issues/177))
- Added descriptions of the `to_mem` and `clone` functions to the spec. ([#195](https://github.com/ethereum/fe/issues/195))


### Internal Changes - for Fe Contributors


- Updated the Solidity backend to v0.8.0. ([#169](https://github.com/ethereum/fe/issues/169))
- Run CI tests on Mac and support creating Mac binaries for releases. ([#178](https://github.com/ethereum/fe/issues/178))


